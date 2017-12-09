{-# OPTIONS_GHC -Wall -Werror #-}

module CompilerState where

import qualified Data.Map as M
import qualified LLVM
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import CompilerErr (CompilerErrorM, raiseCEUndefinedFunction)
import qualified CompilerErr


data ExprEnv  = ExprEnv { erSignatures :: Signatures
                        , erValueMap :: ValueMap }
data ExprState = ExprState { esNextRegister :: NextRegister
                           , esConstCounter :: ConstCounter }
data ExprWr = ExprWr { ewInstructions :: [LLVM.Instr]
                     , ewGlobals :: [LLVM.Constant] }
type ExprM =  ReaderT ExprEnv (StateT ExprState (WriterT ExprWr CompilerErrorM))

emitInstruction :: LLVM.Instr -> ExprM ()
emitInstruction instruction =
  tell ExprWr { ewInstructions = [instruction] , ewGlobals = [] }
emitGlobal :: LLVM.Constant -> ExprM ()
emitGlobal global =
  tell ExprWr { ewInstructions = [], ewGlobals = [global] }

readSignatures :: ExprM Signatures
readSignatures = do
  ExprEnv { erSignatures = signatures, erValueMap = _ } <- ask
  return signatures
readValueMap :: ExprM ValueMap
readValueMap = do
  ExprEnv { erSignatures = _, erValueMap = valueMap } <- ask
  return valueMap
getNextRegisterE :: ExprM LLVM.Register
getNextRegisterE = do
  ExprState { esNextRegister = nextRegister0, esConstCounter = constCounter } <- get
  let (register, nextRegister1) = getNextRegister nextRegister0
  put ExprState { esNextRegister = nextRegister1, esConstCounter = constCounter }
  return register
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

runExprM :: Signatures -> ValueMap -> NextRegister -> ConstCounter -> ExprM (LLVM.Value, LLVM.Type) -> CompilerErrorM (LLVM.Value, LLVM.Type, [LLVM.Instr], [LLVM.Constant], NextRegister, ConstCounter)
runExprM signatures valueMap nextRegister0 constCounter0 monad =
  do (((val, type_), ExprState { esNextRegister = nextRegister1, esConstCounter = constCounter1}), ExprWr { ewInstructions = instr, ewGlobals = globals }) <- runWriterT (runStateT (runReaderT monad ExprEnv { erSignatures = signatures, erValueMap = valueMap }) ExprState { esNextRegister = nextRegister0 , esConstCounter = constCounter0 })
     return (val, type_, instr, globals, nextRegister1, constCounter1)
