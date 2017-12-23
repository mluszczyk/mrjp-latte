{-# OPTIONS_GHC -Wall -Werror #-}

module TransLLVM where

import qualified Data.Map as M

import qualified LLVM
import qualified Data.Set as S
import qualified Data.Set.Extra as SE
import Data.Maybe (fromMaybe)
import Control.Monad.State (evalStateT, get, put, StateT, when)
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
      (\ LLVM.Block { LLVM.bLabel = label } -> label `elem` reachable) blocks }

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
    fix prev = let next = follow prev in
      if next == prev then next else fix next
    reachable :: S.Set LLVM.Label
    reachable = fix (S.singleton startingLabel)

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
    mapFuncInstrsM :: (Monad m) => (LLVM.Instr -> m LLVM.Instr) -> LLVM.Function -> m LLVM.Function
    mapFuncInstrsM go func =
      do
        blocks <- mapM (mapBlockInstrsM go) (LLVM.fBlocks func)
        return func { LLVM.fBlocks = blocks }
    mapBlockInstrsM :: Monad m => (LLVM.Instr -> m LLVM.Instr) -> LLVM.Block -> m LLVM.Block
    mapBlockInstrsM go block =
      do
        instrs <- mapM go (LLVM.bInnerInstrs block)
        exitInstr <- go (LLVM.bExitInstr block)
        return block { LLVM.bInnerInstrs = instrs, LLVM.bExitInstr = exitInstr }

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
