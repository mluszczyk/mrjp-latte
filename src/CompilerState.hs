{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleInstances #-}

module CompilerState where

import qualified Data.Map as M
import qualified LLVM
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import CompilerErr (CompilerErrorM, raiseCEUndefinedFunction, raiseCEUndefinedVariable)
import qualified CompilerErr


newtype StatementEnv = StatementEnv { seSignatures :: Signatures }
data StatementState = StatementState { ssNextRegister :: NextRegister
                                     , ssConstCounter :: ConstCounter
                                     , ssValueMap :: ValueMap
                                     }
type StatementWr = ExprWr
type StatementM = ReaderT StatementEnv (StateT StatementState (WriterT StatementWr CompilerErrorM))

data ExprEnv  = ExprEnv { erSignatures :: Signatures
                        , erValueMap :: ValueMap }
data ExprState = ExprState { esNextRegister :: NextRegister
                           , esConstCounter :: ConstCounter }
data ExprWr = ExprWr { ewInstructions :: [LLVM.Instr]
                     , ewGlobals :: [LLVM.Constant] }
type ExprM =  ReaderT ExprEnv (StateT ExprState (WriterT ExprWr CompilerErrorM))

class (Monad m) => CompilerWriter m where
  emitInstruction :: LLVM.Instr -> m ()
  emitGlobal :: LLVM.Constant -> m ()

instance (Monad m) => CompilerWriter (WriterT ExprWr m) where
  emitInstruction instruction =
    tell ExprWr { ewInstructions = [instruction] , ewGlobals = [] }
  emitGlobal global =
    tell ExprWr { ewInstructions = [], ewGlobals = [global] }

instance (CompilerWriter m) => CompilerWriter (ReaderT a m) where
  emitInstruction = lift . emitInstruction
  emitGlobal = lift . emitGlobal

instance (CompilerWriter m) => CompilerWriter (StateT a m) where
  emitInstruction = lift . emitInstruction
  emitGlobal = lift . emitGlobal

class (Monad m) => SignatureReader m where
  readSignatures :: m Signatures

instance (Monad m) => SignatureReader (ReaderT ExprEnv m) where
  readSignatures = do
    ExprEnv { erSignatures = signatures, erValueMap = _ } <- ask
    return signatures
instance (Monad m) => SignatureReader (ReaderT StatementEnv m) where
  readSignatures = do
    StatementEnv { seSignatures = signatures } <- ask
    return signatures


readValueMap :: ExprM ValueMap
readValueMap = do
  ExprEnv { erSignatures = _, erValueMap = valueMap } <- ask
  return valueMap

readValueMapS :: StatementM ValueMap
readValueMapS = do
  st <- get
  return $ ssValueMap st

class (Monad m) => NextRegisterer m where
  getNextRegisterE :: m LLVM.Register
  getNextLabelE :: m LLVM.Label

instance (Monad m) => NextRegisterer (StateT ExprState m) where
  getNextRegisterE = do
    ExprState { esNextRegister = nextRegister0, esConstCounter = constCounter } <- get
    let (register, nextRegister1) = getNextRegister nextRegister0
    put ExprState { esNextRegister = nextRegister1, esConstCounter = constCounter }
    return register
  getNextLabelE = do
    ExprState { esNextRegister = nextRegister0, esConstCounter = constCounter } <- get
    let (label, nextRegister1) = getNextLabel nextRegister0
    put ExprState { esNextRegister = nextRegister1, esConstCounter = constCounter }
    return label
instance (Monad m) => NextRegisterer (StateT StatementState m) where
  getNextRegisterE = do
    StatementState { ssNextRegister = nextRegister0, ssConstCounter = constCounter, ssValueMap = valueMap } <- get
    let (register, nextRegister1) = getNextRegister nextRegister0
    put StatementState { ssNextRegister = nextRegister1, ssConstCounter = constCounter, ssValueMap = valueMap }
    return register
  getNextLabelE = do
    StatementState { ssNextRegister = nextRegister0, ssConstCounter = constCounter, ssValueMap = valueMap } <- get
    let (label, nextRegister1) = getNextLabel nextRegister0
    put StatementState { ssNextRegister = nextRegister1, ssConstCounter = constCounter, ssValueMap = valueMap }
    return label
instance (NextRegisterer m) => NextRegisterer (ReaderT a m) where
  getNextRegisterE = lift getNextRegisterE
  getNextLabelE = lift getNextLabelE


getNextConstE :: ExprM String
getNextConstE = do
  ExprState { esNextRegister = nextRegister, esConstCounter = constCounter0 } <- get
  let (constName, constCounter1 ) = getNextConst constCounter0
  put ExprState { esNextRegister = nextRegister, esConstCounter = constCounter1 }
  return constName

instance Monoid ExprWr where
  mappend ExprWr { ewInstructions = ins1 , ewGlobals = globals1 } ExprWr { ewInstructions = ins2 , ewGlobals = globals2 }
     = ExprWr { ewInstructions = ins1 ++ ins2, ewGlobals = globals1 ++ globals2 }
  mempty = ExprWr { ewInstructions = [], ewGlobals = [] }

lift3 :: (Monad (t m), Monad (t1 (t m)), Monad m, MonadTrans t,
          MonadTrans t1, MonadTrans t2) =>
         m a -> t2 (t1 (t m)) a
lift3 a = lift $ lift $ lift a

newtype ValueMap = ValueMap (M.Map String (LLVM.Type, LLVM.Register))
newtype Signatures = Signatures (M.Map String LLVM.FunctionType)
newtype NextRegister = NextRegister Int

getType :: String -> Signatures -> CompilerErr.Position -> CompilerErrorM LLVM.FunctionType
getType string signatures position =
  maybe (raiseCEUndefinedFunction string position)
  return
  (getMaybeType string signatures)

getMaybeType :: String -> Signatures -> Maybe LLVM.FunctionType
getMaybeType string (Signatures signatures) =
  M.lookup string signatures

getNextRegister :: NextRegister -> (LLVM.Register, NextRegister)
getNextRegister (NextRegister num) = (LLVM.Register num, NextRegister (num + 1))

getNextLabel :: NextRegister -> (LLVM.Label, NextRegister)
getNextLabel (NextRegister num) = (LLVM.Label num, NextRegister (num + 1))

newtype ConstCounter = ConstCounter Int
initConstCounter :: ConstCounter
initConstCounter = ConstCounter 0
getNextConst :: ConstCounter -> (String, ConstCounter)
getNextConst (ConstCounter num) = ("string" ++ show num, ConstCounter (num + 1))

runExprM :: Signatures -> ValueMap -> NextRegister -> ConstCounter -> ExprM r -> CompilerErrorM (r, [LLVM.Instr], [LLVM.Constant], NextRegister, ConstCounter)
runExprM signatures valueMap nextRegister0 constCounter0 monad =
  do ((res,
       ExprState { esNextRegister = nextRegister1, esConstCounter = constCounter1}),
      ExprWr { ewInstructions = instr, ewGlobals = globals }) <-
            runWriterT (
                runStateT (
                  runReaderT monad
                    ExprEnv { erSignatures = signatures, erValueMap = valueMap })
                  ExprState { esNextRegister = nextRegister0 , esConstCounter = constCounter0 })
     return (res, instr, globals, nextRegister1, constCounter1)

runStatementM :: Signatures -> ValueMap -> NextRegister -> ConstCounter -> StatementM () -> CompilerErrorM ([LLVM.Instr], [LLVM.Constant], ValueMap, NextRegister, ConstCounter)
runStatementM signatures valueMap0 nextRegister0 constCounter0 monad =
  do (((),
      StatementState { ssNextRegister = nextRegister1, ssConstCounter = constCounter1, ssValueMap = valueMap1 }),
       ExprWr { ewInstructions = instr, ewGlobals = globals }) <-
        runWriterT (runStateT (runReaderT monad
        StatementEnv { seSignatures = signatures })
         StatementState { ssNextRegister = nextRegister0, ssConstCounter = constCounter0, ssValueMap = valueMap0 })
     return (instr, globals, valueMap1, nextRegister1, constCounter1)

safeValueMap :: ExprM t -> StatementM t
safeValueMap exprM =
  do signatures <- readSignatures
     StatementState { ssValueMap = valueMap, ssConstCounter = constCounter0, ssNextRegister = nextRegister0  } <- get
     (res, ExprState { esNextRegister = nextRegister1, esConstCounter = constCounter1}) <- lift $ lift $
         runStateT (runReaderT exprM
            ExprEnv { erSignatures = signatures, erValueMap = valueMap })
            ExprState { esNextRegister = nextRegister0, esConstCounter = constCounter0 }
     put StatementState { ssNextRegister = nextRegister1, ssConstCounter = constCounter1, ssValueMap = valueMap }
     return res

statementInExpr :: StatementM t -> ExprM t
statementInExpr statementM =
  do ExprEnv { erSignatures = signatures, erValueMap = valueMap } <- ask
     ExprState { esConstCounter = constCounter0, esNextRegister = nextRegister0  } <- get
     (res, StatementState { ssNextRegister = nextRegister1, ssConstCounter = constCounter1}) <- lift $ lift $
         runStateT (runReaderT statementM
            StatementEnv { seSignatures = signatures })
            StatementState { ssNextRegister = nextRegister0, ssConstCounter = constCounter0, ssValueMap = valueMap }
     put ExprState { esNextRegister = nextRegister1, esConstCounter = constCounter1 }
     return res

initNextRegister :: NextRegister
initNextRegister = NextRegister 0

setVariable :: String -> LLVM.Type -> LLVM.Register -> ValueMap -> ValueMap
setVariable name type_ value (ValueMap valueMap) =
    ValueMap (M.insert name (type_, value) valueMap)

lookupVariable :: String -> ValueMap -> CompilerErr.Position -> CompilerErrorM (LLVM.Type, LLVM.Register)
lookupVariable name (ValueMap valueMap) position =
    maybe (raiseCEUndefinedVariable name position) return (M.lookup name valueMap)

setVariableM :: String -> LLVM.Type -> LLVM.Register -> StatementM ()
setVariableM name type_ value =
  do StatementState { ssValueMap = valueMap0, ssConstCounter = constCounter, ssNextRegister = nextRegister } <- get
     put StatementState { ssValueMap = setVariable name type_ value valueMap0, ssConstCounter = constCounter, ssNextRegister = nextRegister }

initValueMap :: ValueMap
initValueMap = ValueMap M.empty
