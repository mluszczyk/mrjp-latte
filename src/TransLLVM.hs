{-# OPTIONS_GHC -Wall -Werror #-}

-- transformations on LLVM including optimisations and mem2reg algorithm.

module TransLLVM where

import qualified Data.Map as M

import qualified LLVM
import qualified Data.Set as S
import qualified Data.Set.Extra as SE
import Data.Tuple (swap)
import Data.Maybe (fromMaybe, mapMaybe, isNothing, maybeToList)
import Data.Bool (bool)
import Control.Monad.State (evalStateT, runState, get, put, StateT, State, when)
import qualified CompilerErr as CE
import qualified CompilerState as CS
import Control.Arrow (second)

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
    neighbours :: LLVM.Label -> [LLVM.Label]
    neighbours = instrNeighbours . LLVM.bExitInstr . (`blockByLabel` function)
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

blockByLabel :: LLVM.Label -> LLVM.Function -> LLVM.Block
blockByLabel label function = head $
  filter (\ LLVM.Block { LLVM.bLabel = label' } -> (label == label'))
    (LLVM.fBlocks function)

instrNeighbours :: LLVM.Instr -> [LLVM.Label]
instrNeighbours (LLVM.IBr label) = [label]
instrNeighbours (LLVM.IBrCond _ _ label1 label2) = [label1, label2]
instrNeighbours _ = []

fixEq :: (Eq a) => (a -> a) -> a -> a
fixEq follow prev = let next = follow prev in
  if next == prev then next else fixEq follow next

fixEqM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
fixEqM follow prev = do
  next <- follow prev
  if next == prev then return next else fixEqM follow next

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
mapValInInstrM go (LLVM.IGetElementPtr elemType (arrayType, array)
                                       (indexType, index) res) = do
  array' <- go array
  index' <- go index
  return $ LLVM.IGetElementPtr elemType (arrayType, array')
                               (indexType, index') res
mapValInInstrM go (LLVM.IBitcast (srcType, val) dstType reg) = do
  val' <- go val
  return $ LLVM.IBitcast (srcType, val') dstType reg
mapValInInstrM go (LLVM.ISext (srcType, val) dstType reg) = do
  val' <- go val
  return $ LLVM.ISext (srcType, val') dstType reg

mapValInInstr :: (LLVM.Value -> LLVM.Value) -> LLVM.Instr -> LLVM.Instr
mapValInInstr go instr = mapValInInstrM (\ val () -> go val) instr ()

mapValInFunc :: (LLVM.Value -> LLVM.Value) -> LLVM.Function -> LLVM.Function
mapValInFunc go = mapFuncInstrs (mapValInInstr go)

removeUnusedAssignments :: LLVM.Function -> LLVM.Function
removeUnusedAssignments function =
    (filterInnerInstrs isUsed function)
      { LLVM.fArgs = map goArg (LLVM.fArgs function)}
  where
    getUsedValues :: LLVM.Instr -> [LLVM.Value]
    getUsedValues instr = map snd (accessedVals instr)

    getUsedRegisters instr = mapMaybe (\ val -> case val of
      LLVM.VRegister reg -> Just reg
      _ -> Nothing) (getUsedValues instr)

    usedAssignments :: S.Set LLVM.Register
    usedAssignments = foldl S.union S.empty (map (S.fromList . getUsedRegisters) (listInstrs function))

    isUsed instr | hasSideEffect instr = True
    isUsed instr = any (`S.member` usedAssignments)
                       (map snd (setRegisters instr))

    hasSideEffect instr = case instr of
      LLVM.ICall {} -> True
      LLVM.IStore {} -> True
      _ -> False

    goArg :: (LLVM.Type, Maybe String) -> (LLVM.Type, Maybe String)
    goArg (argType, Just name)
      | not (LLVM.RArgument name `S.member` usedAssignments) =
      (argType, Nothing)
    goArg other = other

filterInnerInstrs :: (LLVM.Instr -> Bool) -> LLVM.Function -> LLVM.Function
filterInnerInstrs shouldStay function = function {
  LLVM.fBlocks = map (removeBlockInnerInstrs shouldStay) (LLVM.fBlocks function) }
  where
    removeBlockInnerInstrs shouldStay' block = block {
      LLVM.bInnerInstrs = filter shouldStay' (LLVM.bInnerInstrs block) }

listInstrs :: LLVM.Function -> [LLVM.Instr]
listInstrs function = concatMap listBlockInstrs (LLVM.fBlocks function)

accessedVals :: LLVM.Instr -> [(LLVM.Type, LLVM.Value)]
accessedVals instr = case instr of
  LLVM.ICall _ _ args _ -> args
  LLVM.IRetVoid -> []
  LLVM.IRet type_ v -> [(type_, v)]
  LLVM.IArithm type_ v1 v2 _ _ -> [(type_, v1), (type_, v2)]
  LLVM.IBr _ -> []
  LLVM.IBrCond type_ v _ _-> [(type_, v)]
  LLVM.ILabel _ -> []
  LLVM.ILoad _ typePtr ptr _ -> -- TODO: type inaccurate
    [(typePtr, LLVM.VRegister ptr)]
  LLVM.IStore typeVal val typePtr ptr ->
    [(typeVal, val), (typePtr, LLVM.VRegister ptr)]
  LLVM.IAlloca _ _ -> []
  LLVM.IIcmp _ type_ v1 v2 _ -> [(type_, v1), (type_, v2)]
  LLVM.IPhi type_ pairs _ ->
   map (\p -> (type_, fst p)) pairs
  LLVM.IUnreachable -> []
  LLVM.IGetElementPtr _ arrayPair indexPair _ -> [arrayPair, indexPair]
  LLVM.IBitcast pair _ _ -> [pair]
  LLVM.ISext pair _ _ -> [pair]

setRegisters :: LLVM.Instr -> [(LLVM.Type, LLVM.Register)]
setRegisters instr = case instr of
  LLVM.ICall retType _ _ mReg ->
    maybe [] (\r -> [(retType, r)]) mReg
  LLVM.IRetVoid -> []
  LLVM.IRet _ _ -> []
  LLVM.IArithm type_ _ _ _ r -> [(type_, r)]
  LLVM.IBr _ -> []
  LLVM.IBrCond {} -> []
  LLVM.ILabel _ -> []
  LLVM.ILoad type_ _ _ reg ->
    [(type_, reg)]
  LLVM.IStore _ _ typeReg reg ->  -- TODO: inacurate type
    [(typeReg, reg)]
  LLVM.IAlloca type_ reg ->  -- TODO: inaccurate type
    [(type_, reg)]
  LLVM.IIcmp _ _ _ _ reg -> [(LLVM.Ti1, reg)]
  LLVM.IPhi type_ _ reg -> [(type_, reg)]
  LLVM.IUnreachable -> []
  LLVM.IGetElementPtr type_ _ _ reg -> [(LLVM.Ptr type_, reg)]
  LLVM.IBitcast _ type_ reg -> [(type_, reg)]
  LLVM.ISext _ type_ reg -> [(type_, reg)]

listRegisters :: LLVM.Function -> [(LLVM.Type, LLVM.Register)]
listRegisters function = S.toList $ S.fromList (
  mapMaybe (\(t, mn) -> case mn of Just n -> Just (t, LLVM.RArgument n)
                                   _ -> Nothing) (LLVM.fArgs function) ++
  mapMaybe extractRegister (listValsInFunc function))

registerType :: LLVM.Function -> M.Map LLVM.Register LLVM.Type
registerType function = M.fromList (map swap (listRegisters function))

extractRegister :: (LLVM.Type, LLVM.Value) -> Maybe (LLVM.Type, LLVM.Register)
extractRegister (type_, LLVM.VRegister reg) = Just (type_, reg)
extractRegister _ = Nothing

listValsInInstr :: LLVM.Instr -> [(LLVM.Type, LLVM.Value)]
listValsInInstr instr = accessedVals instr ++
  map (second LLVM.VRegister) (setRegisters instr)

listValsInFunc :: LLVM.Function -> [(LLVM.Type, LLVM.Value)]
listValsInFunc func =
  mapMaybe go (LLVM.fArgs func) ++
  concatMap listValsInInstr (listInstrs func)
  where
    go (type_, Just name) = Just (type_, LLVM.VRegister (LLVM.RArgument name))
    go _ = Nothing

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
mem2Reg function nextRegister0 =
  ( trans update function { LLVM.fBlocks = newBlocks }
  , nextRegister1 )
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

    (noMemBlocks, blockPtrValLists, updates) =
      unzip3 $ zipWith goBlock (LLVM.fBlocks function) initVars
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
      in blockMem2Reg block ptrRegs ptrToVal M.empty
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

    update = M.unions updates

    trans valUp = mapValInFunc (transVal valUp)
    transVal valUp (LLVM.VRegister reg) =
      fromMaybe (LLVM.VRegister reg) (M.lookup reg valUp)
    transVal _ other = other


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
      case S.toList (LLVM.VRegister reg `S.delete` S.fromList (map fst pairs)) of
        [value] -> Just (LLVM.VRegister reg, value)
        _ -> Nothing
    extractTrivialPhi _ = Nothing

    valUpdate = M.fromList (mapMaybe extractTrivialPhi (listInstrs function))
    follow mapping = M.map (\x -> fromMaybe x (M.lookup x mapping)) mapping
    valUpdateFix = fixEq follow valUpdate

collectPhis :: LLVM.Function -> CollectedPhis
collectPhis LLVM.Function { LLVM.fBlocks = blocks } =
  M.unionsWith (++) (concatMap goBlock blocks)
  where
    goBlock block = map goInstr (LLVM.bInnerInstrs block)
      where
        goInstr (LLVM.IPhi type_ pairs reg) = M.fromList
          [ ((srcBlock, LLVM.bLabel block), [(type_, reg, val)])
          | (val, srcBlock) <- pairs]
        goInstr _ = M.empty

type CollectedPhis = M.Map (LLVM.Label, LLVM.Label)
                  [(LLVM.Type, LLVM.Register, LLVM.Value)]


inferenceGraph :: LLVM.Function
                  -> ([LLVM.Register], [(LLVM.Register, LLVM.Register)])
inferenceGraph function = (registers, edges)

 where
   registers = map snd $ listRegisters function

   edges :: [(LLVM.Register, LLVM.Register)]
   edges = S.toList $ S.unions $ concatMap
             (map (\instrPos ->
                    let out = S.toList $ liveIn instrPos in S.fromList
                    [order (reg1, reg2) | reg1 <- out, reg2 <- out,
                                    reg1 /= reg2])
              . enumerateInstructions) (LLVM.fBlocks function)
     where
       order (reg1, reg2) | reg1 < reg2 = (reg1, reg2)
                          | reg1 > reg2 = (reg2, reg1)
                          | otherwise = error "unreachable"
   enumerateInstructions block =
     [(block, instr, num)
        | (num, instr) <- zip [0::Int ..]
          (LLVM.bInnerInstrs block ++ [LLVM.bExitInstr block])]

   extractUntypedRegister val = case val of LLVM.VRegister reg -> Just reg
                                            _ -> Nothing

   liveIn instrPos = blockLiveInTrans (cutBlock instrPos)
                                      allLiveIn
     where
       cutBlock (block, _, pos) =
         block { LLVM.bInnerInstrs = drop pos (LLVM.bInnerInstrs block)}

   instrLiveOutTrans :: LLVM.Instr
                        -> S.Set LLVM.Register
                        -> S.Set LLVM.Register
   instrLiveOutTrans LLVM.IPhi {} inSet = inSet
   instrLiveOutTrans instr inSet =
     gen `S.union` (inSet S.\\ kill)
     where
       gen :: S.Set LLVM.Register
       gen = S.fromList $ map snd $ mapMaybe extractRegister $ accessedVals instr
       kill :: S.Set LLVM.Register
       kill = S.fromList $ map snd (setRegisters instr)

   allLiveIn :: M.Map LLVM.Label (S.Set LLVM.Register)
   allLiveIn = fixEq followLiveIn (M.fromList (
      map (\block -> (LLVM.bLabel block, S.empty)) (LLVM.fBlocks function)))
     where
       followLiveIn :: M.Map LLVM.Label (S.Set LLVM.Register)
                       -> M.Map LLVM.Label (S.Set LLVM.Register)
       followLiveIn cur = M.mapWithKey (\label _ -> blockLiveInTrans (blockByLabel label function) cur) cur
   blockLiveInTrans :: LLVM.Block
                       -> M.Map LLVM.Label (S.Set LLVM.Register)
                       -> S.Set LLVM.Register
   blockLiveInTrans block allLiveIn_ =
     foldr instrLiveOutTrans
           (instrLiveOutTrans (LLVM.bExitInstr block)
                              (blockLiveOutFollow block allLiveIn_))
           (LLVM.bInnerInstrs block)

   collectedPhis = collectPhis function

   blockLiveOutFollow :: LLVM.Block
                       -> M.Map LLVM.Label (S.Set LLVM.Register)
                       -> S.Set LLVM.Register
   blockLiveOutFollow block allLiveIn_ = S.unions $
    map (uncurry processPhis) $
    filter (\(label, _) -> label `elem` instrNeighbours (LLVM.bExitInstr block)) $
    M.toList allLiveIn_
    where
      processPhis :: LLVM.Label -> S.Set LLVM.Register -> S.Set LLVM.Register
      processPhis dstLabel inSet =
        foldr (\ (_, reg, val) phiOut ->
                S.fromList (maybeToList (extractUntypedRegister val))
                `S.union` phiOut S.\\ S.singleton reg) inSet
        (M.findWithDefault [] (LLVM.bLabel block, dstLabel) collectedPhis)
