{-# OPTIONS_GHC -Wall -Werror #-}

module TransLLVM where

import qualified Data.Map as M

import qualified LLVM
import qualified Data.Set as S
import qualified Data.Set.Extra as SE
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import Data.Bool (bool)
import Control.Monad.State (evalStateT, runState, get, put, StateT, State, when)
import qualified CompilerErr as CE
import qualified CompilerState as CS

instrsToBlocks :: [LLVM.Instr] -> [LLVM.Block]
instrsToBlocks [] = []
instrsToBlocks (LLVM.ILabel label : rest) =
  LLVM.Block label innerInstrs lastInstr :
  instrsToBlocks postBlock
  where
    (innerInstrs, lastInstr, postBlock) = getNextBlock rest

    getNextBlock :: [LLVM.Instr] -> ([LLVM.Instr], LLVM.Instr, [LLVM.Instr])
    getNextBlock [] = error "basic block missies exit"
    getNextBlock (LLVM.ILabel {} : _) = error "basic block misses exit"
    getNextBlock (instr : rest') = if isExit instr then
      ([], instr, rest')
    else
      let (inner, exit, rest'') = getNextBlock rest'
      in (instr : inner, exit, rest'')
-- Ignore instruction if no block is open.
instrsToBlocks (_ : rest) = instrsToBlocks rest

isExit :: LLVM.Instr -> Bool
isExit instr = case instr of
  LLVM.IUnreachable -> True
  LLVM.IRet {} -> True
  LLVM.IRetVoid -> True
  LLVM.IBr {} -> True
  LLVM.IBrCond {} -> True
  _ -> False

removeUnreachableBlocks :: LLVM.Function -> LLVM.Function
removeUnreachableBlocks function@LLVM.Function { LLVM.fBlocks = blocks} =
  function { LLVM.fBlocks = filter
      (\ LLVM.Block { LLVM.bLabel = label } -> label `elem` reachable)
      (map (mapBlockInstrs updatePhi) blocks) }

  where
    instrNeighbours :: LLVM.Instr -> [LLVM.Label]
    instrNeighbours (LLVM.IBr label) = [label]
    instrNeighbours (LLVM.IBrCond _ _ label1 label2) = [label1, label2]
    instrNeighbours _ = []
    neighbours :: LLVM.Label -> [LLVM.Label]
    neighbours = instrNeighbours . LLVM.bExitInstr . getBlock
    getBlock :: LLVM.Label -> LLVM.Block
    getBlock label = head $
      filter (\ LLVM.Block { LLVM.bLabel = label' } -> (label == label'))
        blocks
    startingLabel :: LLVM.Label
    startingLabel = LLVM.bLabel (head blocks)
    follow :: S.Set LLVM.Label -> S.Set LLVM.Label
    follow s = s `S.union` SE.concatMap (S.fromList . neighbours) s
    reachable :: S.Set LLVM.Label
    reachable = fixEq follow (S.singleton startingLabel)

    updatePhi (LLVM.IPhi t pairs reg) = LLVM.IPhi t newPairs reg
      where
        newPairs = filter (\(_, label) -> label `S.member` reachable) pairs
    updatePhi other = other

fixEq :: (Eq a) => (a -> a) -> a -> a
fixEq follow prev = let next = follow prev in
  if next == prev then next else fixEq follow next

hasUnreachableInstruction :: LLVM.Function -> Bool
hasUnreachableInstruction LLVM.Function { LLVM.fBlocks = blocks } =
  any (\ block -> LLVM.bExitInstr block == LLVM.IUnreachable) blocks

type ConstantPropMonad = StateT (M.Map LLVM.Register LLVM.Value) CS.CompilerErrorM

constantProp :: LLVM.Function -> CS.CompilerErrorM LLVM.Function
constantProp function =
  evalStateT (mapFuncInstrsM iConstProp (topoSortBlocks function)) M.empty

  where
    topoSortBlocks :: LLVM.Function -> LLVM.Function
    topoSortBlocks func = func  -- TODO: implement

    iConstProp :: LLVM.Instr -> ConstantPropMonad LLVM.Instr
    iConstProp instr = do
      transInstr <- mapValInInstrM tryToConst instr
      case transInstr of
        LLVM.IArithm LLVM.Ti32 (LLVM.VConst c1) (LLVM.VConst c2) op resultReg -> do
          c <- eval op c1 c2
          storeConst (LLVM.VConst c) resultReg
          return transInstr
        LLVM.IIcmp cond LLVM.Ti32 (LLVM.VConst c1) (LLVM.VConst c2) resultReg -> do
          storeConst (boolToLLVM (evalCond cond c1 c2)) resultReg
          return transInstr
        LLVM.IBrCond _ val ltrue lfalse -> case val of
            LLVM.VTrue -> return $ LLVM.IBr ltrue
            LLVM.VFalse -> return $ LLVM.IBr lfalse
            _ -> return transInstr
        _ -> return transInstr
      where
        storeConst :: LLVM.Value -> LLVM.Register -> ConstantPropMonad ()
        storeConst val reg = do
          store <- get
          put (M.insert reg val store)
        eval :: LLVM.ArithmOp -> Integer -> Integer -> ConstantPropMonad Integer
        eval LLVM.OAdd a b = return $ a + b
        eval LLVM.OMul a b = return $ a * b
        eval LLVM.OSub a b = return $ a - b
        eval LLVM.OSDiv a b = do
          when (b == 0) $
            CS.raise CE.CEDivisionByZero
          return $ a `quot` b
        eval LLVM.OSRem a b = do
          when (b == 0) $
            CS.raise CE.CEDivisionByZero
          return $ a `rem` b
        evalCond :: LLVM.Cond -> Integer -> Integer -> Bool
        evalCond LLVM.RelOpNE = (/=)
        evalCond LLVM.RelOpEQ = (==)
        evalCond LLVM.RelOpSGE = (>=)
        evalCond LLVM.RelOpSGT = (>)
        evalCond LLVM.RelOpSLE = (<=)
        evalCond LLVM.RelOpSLT = (<)
        boolToLLVM :: Bool -> LLVM.Value
        boolToLLVM True = LLVM.VTrue
        boolToLLVM False = LLVM.VFalse


    tryToConst :: LLVM.Value -> ConstantPropMonad LLVM.Value
    tryToConst (LLVM.VRegister reg) = do
      store <- get
      return $ fromMaybe (LLVM.VRegister reg) (M.lookup reg store)
    tryToConst somethingElse = return somethingElse

mapValInInstrM :: (Monad m)
                  => (LLVM.Value
                  -> m LLVM.Value)
                  -> LLVM.Instr
                  -> m LLVM.Instr
mapValInInstrM go (LLVM.ICall type_ string args mRegister) = do
  args' <- mapM (\ (t, v) -> do v' <- go v
                                return (t, v')) args
  return $ LLVM.ICall type_ string args' mRegister
mapValInInstrM _ LLVM.IRetVoid = return LLVM.IRetVoid
mapValInInstrM go (LLVM.IRet type_ val) = do
  val' <- go val
  return $ LLVM.IRet type_ val'
mapValInInstrM _ i@(LLVM.IBr _) = return i
mapValInInstrM go (LLVM.IBrCond type_ val ltrue lfalse) = do
  val' <- go val
  return $ LLVM.IBrCond type_ val' ltrue lfalse
mapValInInstrM _ i@(LLVM.ILabel _) = return i
mapValInInstrM _ i@LLVM.ILoad {} = return i
mapValInInstrM go (LLVM.IStore type1 val type2 reg) = do
  val' <- go val
  return (LLVM.IStore type1 val' type2 reg)
mapValInInstrM _ i@(LLVM.IAlloca _ _) = return i
mapValInInstrM go (LLVM.IIcmp cond type_ val1 val2 reg) = do
  val1' <- go val1
  val2' <- go val2
  return (LLVM.IIcmp cond type_ val1' val2' reg)
mapValInInstrM go (LLVM.IPhi type_ pairs reg) = do
  pairs' <- mapM (\ (val, label) -> do val' <- go val
                                       return (val', label)) pairs
  return (LLVM.IPhi type_ pairs' reg)
mapValInInstrM _ i@LLVM.IUnreachable = return i
mapValInInstrM go (LLVM.IArithm type_ val1 val2 op resultReg) = do
  t1 <- go val1
  t2 <- go val2
  return $ LLVM.IArithm type_ t1 t2 op resultReg

mapValInInstr :: (LLVM.Value -> LLVM.Value) -> LLVM.Instr -> LLVM.Instr
mapValInInstr go instr = mapValInInstrM (\ val () -> go val) instr ()

mapValInFunc :: (LLVM.Value -> LLVM.Value) -> LLVM.Function -> LLVM.Function
mapValInFunc go = mapFuncInstrs (mapValInInstr go)

removeUnusedAssignments :: LLVM.Function -> LLVM.Function
removeUnusedAssignments function =
    filterInnerInstrs isUsed function
  where
    getUsedValues :: LLVM.Instr -> [LLVM.Value]
    getUsedValues (LLVM.ICall _ _ args _) = map snd args
    getUsedValues (LLVM.IRet _ val) = [val]
    getUsedValues (LLVM.IBrCond _ val _ _) = [val]
    getUsedValues (LLVM.IPhi _ args _) = map fst args
    getUsedValues (LLVM.IIcmp _ _ v1 v2 _) = [v1, v2]
    getUsedValues (LLVM.ILoad _ _ val _) = [LLVM.VRegister val]
    getUsedValues (LLVM.IArithm _ v1 v2 _ _) = [v1, v2]
    getUsedValues (LLVM.IStore _ v1 _ r2) = [v1, LLVM.VRegister r2]
    getUsedValues LLVM.IRetVoid = []
    getUsedValues LLVM.IBr {} = []
    getUsedValues LLVM.ILabel {} = []
    getUsedValues LLVM.IAlloca {} = []
    getUsedValues LLVM.IUnreachable = []

    getUsedRegisters instr = mapMaybe (\ val -> case val of
      LLVM.VRegister reg -> Just reg
      _ -> Nothing) (getUsedValues instr)

    usedAssignments :: S.Set LLVM.Register
    usedAssignments = foldl S.union S.empty (map (S.fromList . getUsedRegisters) (listInstrs function))

    isUsed instr = case getMResult instr of
      Nothing -> True
      Just reg -> reg `S.member` usedAssignments

    getMResult (LLVM.IArithm _ _ _ _ reg) = Just reg
    getMResult (LLVM.IIcmp _ _ _ _ reg) = Just reg
    getMResult (LLVM.IPhi _ _ reg) = Just reg
    getMResult (LLVM.ILoad _ _ _ reg) = Just reg
    getMResult (LLVM.IAlloca _ reg) = Just reg
    getMResult _ = Nothing  -- ICall should return Nothing,
                            -- mind the side effects!

filterInnerInstrs :: (LLVM.Instr -> Bool) -> LLVM.Function -> LLVM.Function
filterInnerInstrs shouldStay function = function {
  LLVM.fBlocks = map (removeBlockInnerInstrs shouldStay) (LLVM.fBlocks function) }
  where
    removeBlockInnerInstrs shouldStay' block = block {
      LLVM.bInnerInstrs = filter shouldStay' (LLVM.bInnerInstrs block) }

listInstrs :: LLVM.Function -> [LLVM.Instr]
listInstrs function = concatMap listBlockInstrs (LLVM.fBlocks function)
listBlockInstrs :: LLVM.Block -> [LLVM.Instr]
listBlockInstrs block = LLVM.bInnerInstrs block ++ [LLVM.bExitInstr block]
mapFuncInstrsM :: (Monad m) => (LLVM.Instr -> m LLVM.Instr) -> LLVM.Function -> m LLVM.Function
mapFuncInstrsM go func =
  do
    blocks <- mapM (mapBlockInstrsM go) (LLVM.fBlocks func)
    return func { LLVM.fBlocks = blocks }
mapFuncInstrs :: (LLVM.Instr -> LLVM.Instr) -> LLVM.Function -> LLVM.Function
mapFuncInstrs go func = mapFuncInstrsM (\ item () -> go item) func ()
mapBlockInstrs :: (LLVM.Instr -> LLVM.Instr) -> LLVM.Block -> LLVM.Block
mapBlockInstrs go block = mapBlockInstrsM (\instr () -> go instr) block ()
mapBlockInstrsM :: Monad m => (LLVM.Instr -> m LLVM.Instr) -> LLVM.Block -> m LLVM.Block
mapBlockInstrsM go block =
  do
    instrs <- mapM go (LLVM.bInnerInstrs block)
    exitInstr <- go (LLVM.bExitInstr block)
    return block { LLVM.bInnerInstrs = instrs, LLVM.bExitInstr = exitInstr }

mem2Reg :: LLVM.Function -> CS.NextRegister -> (LLVM.Function, CS.NextRegister)
mem2Reg function nextRegister0 = (function { LLVM.fBlocks = newBlocks } , nextRegister1)
  where
    (initVars, nextRegister1) = runState
          (mapM (\ _ -> mapM (const getNextRegisterM) allocedPtrs)
          (LLVM.fBlocks function)) nextRegister0
    getNextRegisterM :: State CS.NextRegister LLVM.Register
    getNextRegisterM = do
      state <- get
      let (reg, newState) = CS.getNextRegister state
      put newState
      return reg

    (noMemBlocks, blockPtrValLists) = unzip $ zipWith goBlock (LLVM.fBlocks function) initVars
    blockPtrVal = M.fromList (zip (map LLVM.bLabel noMemBlocks) blockPtrValLists)
    blockPhis :: [[LLVM.Instr]]
    blockPhis = [
      let predLabels = map LLVM.bLabel (predecessors block)
      in if null predLabels then [] else
        map (\ ((t, a), reg) ->
            LLVM.IPhi t [(blockPtrVal M.! predLabel M.! a, predLabel)
                    | predLabel <- predLabels] reg
            ) (zip allocedPtrs blockInitVars)
      | (block, blockInitVars) <- zip noMemBlocks initVars ]
    newBlocks = map (\ (block, phis) ->
      block { LLVM.bInnerInstrs = phis ++ LLVM.bInnerInstrs block} ) $
      zip noMemBlocks blockPhis

    goBlock block blockInitVars =
      let
        ptrRegs = map snd allocedPtrs
        ptrToVal = if null (predecessors block) then
          M.fromList (map (\ptr -> (ptr, LLVM.VUndef)) ptrRegs) else
          M.fromList (zip ptrRegs (map LLVM.VRegister blockInitVars))
        (block1, rest, _) = blockMem2Reg block ptrRegs ptrToVal M.empty
      in (block1, rest)
    allocedPtrs = mapMaybe (\ instr -> case instr of
        LLVM.IAlloca t a -> Just (t, a)
        _ -> Nothing) (listInstrs function)

    predecessors dstBlock = filter (`jumpsTo` dstBlock) (LLVM.fBlocks function)
    jumpsTo blockSrc blockDst = case LLVM.bExitInstr blockSrc of
      LLVM.IBr dstLabel | LLVM.bLabel blockDst == dstLabel -> True
      LLVM.IBrCond _ _ dstLabel1 dstLabel2
        | LLVM.bLabel blockDst == dstLabel1 ||
          LLVM.bLabel blockDst == dstLabel2 -> True
      _ -> False


blockMem2Reg :: LLVM.Block
               -> [LLVM.Register]
               -> M.Map LLVM.Register LLVM.Value
               -> M.Map LLVM.Register LLVM.Value
               -> (LLVM.Block, M.Map LLVM.Register LLVM.Value, M.Map LLVM.Register LLVM.Value)
blockMem2Reg block allocedPtrs ptrVal0 valueUpdate0  =
  ( block { LLVM.bInnerInstrs = innerInstrs, LLVM.bExitInstr = exitInstr }
  , ptrVal3
  , valUpdate3 )

  where
    (ptrVal3, valUpdate3, innerInstrs) = foldl go (ptrVal0, valueUpdate0, []) (LLVM.bInnerInstrs block)
    exitInstr = trans valUpdate3 (LLVM.bExitInstr block)

    go :: (M.Map LLVM.Register LLVM.Value, M.Map LLVM.Register LLVM.Value,
            [LLVM.Instr])
          -> LLVM.Instr
          -> (M.Map LLVM.Register LLVM.Value, M.Map LLVM.Register LLVM.Value,
                [LLVM.Instr])
    go (ptrVal1, valUpdate1, instrs1) instr =
      let tInstr = trans valUpdate1 instr
          (ptrRep, valRep, leaveInstr) = reduceMem ptrVal1 tInstr
          ptrVal2 = maybe ptrVal1 (\ (ptr, val) -> M.insert ptr val ptrVal1) ptrRep
          valUpdate2 = maybe valUpdate1 (\ (oldReg, newVal) -> M.insert oldReg newVal valUpdate1) valRep
          instrs2 = bool instrs1 (instrs1 ++ [tInstr]) leaveInstr
      in (ptrVal2, valUpdate2, instrs2)

    trans valUp = mapValInInstr (transVal valUp)
    transVal valUp (LLVM.VRegister reg) =
      fromMaybe (LLVM.VRegister reg) (M.lookup reg valUp)
    transVal _ other = other

    reduceMem :: M.Map LLVM.Register LLVM.Value
                 -> LLVM.Instr
                 -> ( Maybe (LLVM.Register, LLVM.Value)
                    , Maybe (LLVM.Register, LLVM.Value)
                    , Bool)
    reduceMem ptrVal1 (LLVM.ILoad _ _ ptrReg loadedReg)
       | ptrReg `elem` allocedPtrs = case M.lookup ptrReg ptrVal1 of
      Nothing -> (Nothing, Nothing, True)
      Just replVal -> (Nothing, Just (loadedReg, replVal), False)
    reduceMem _ (LLVM.IStore _ val _ ptrReg) | ptrReg `elem` allocedPtrs =
      (Just (ptrReg, val), Nothing, False)
    reduceMem _ _ = (Nothing, Nothing, True)

removeTrivialPhis :: LLVM.Function -> LLVM.Function
removeTrivialPhis function =
   mapValInFunc (\ val -> fromMaybe val (M.lookup val valUpdateFix)) $
   filterInnerInstrs (isNothing . extractTrivialPhi) function
  where
    extractTrivialPhi (LLVM.IPhi _ pairs reg) =
      case S.toList (S.fromList (map fst pairs)) of
        [value] -> Just (LLVM.VRegister reg, value)
        _ -> Nothing
    extractTrivialPhi _ = Nothing

    valUpdate = M.fromList (mapMaybe extractTrivialPhi (listInstrs function))
    follow mapping = M.map (\x -> fromMaybe x (M.lookup x mapping)) mapping
    valUpdateFix = fixEq follow valUpdate
