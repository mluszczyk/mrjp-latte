{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleInstances #-}

-- monads used in the compiler.

module CompilerState where

import qualified Data.Map as M
import qualified LLVM
import Control.Monad.Reader
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Writer
import qualified CompilerErr
import qualified AbsLatte


type VariableIdent = AbsLatte.CIdent
type FunctionIdent = AbsLatte.CIdent

data StatementEnv = StatementEnv { seSignatures :: Signatures
                                    , seRetType :: LLVM.Type }
data StatementState = StatementState { ssNextRegister :: NextRegister
                                     , ssConstCounter :: ConstCounter
                                     , ssValueMap :: ValueMap
                                     , ssNewScopeVars :: [VariableIdent]
                                     , ssInstructions :: [LLVM.Instr]
                                     , ssCurrentBlock :: LLVM.Label
                                     }
type StatementWr = ExprWr
type StatementM = ReaderT StatementEnv (StateT StatementState (WriterT StatementWr CompilerErrorM))

data ExprEnv  = ExprEnv { erSignatures :: Signatures
                        , erValueMap :: ValueMap
                        , erRetType :: LLVM.Type }
data ExprState = ExprState { esNextRegister :: NextRegister
                           , esConstCounter :: ConstCounter
                           , esInstructions :: [LLVM.Instr]
                           , esCurrentBlock :: LLVM.Label
                           }
newtype ExprWr = ExprWr { ewGlobals :: [LLVM.Constant] }
type ExprM =  ReaderT ExprEnv (StateT ExprState (WriterT ExprWr CompilerErrorM))

type CompilerErrorM = Either CompilerErr.CompilerError

class (Monad m) => Raiser m where
  raise :: CompilerErr.CompilerError -> m a

instance Raiser (Either CompilerErr.CompilerError) where
  raise = Left
instance (Raiser m) => Raiser (WriterT ExprWr m) where
  raise = lift . raise
instance (Raiser m) => Raiser (StateT a m) where
  raise = lift . raise
instance (Raiser m) => Raiser (ReaderT a m) where
  raise = lift . raise

class (Monad m) => CompilerWriter m where
  emitGlobal :: LLVM.Constant -> m ()

instance (Monad m) => CompilerWriter (WriterT ExprWr m) where
  emitGlobal global =
    tell ExprWr { ewGlobals = [global] }

instance (CompilerWriter m) => CompilerWriter (ReaderT a m) where
  emitGlobal = lift . emitGlobal

instance (CompilerWriter m) => CompilerWriter (StateT a m) where
  emitGlobal = lift . emitGlobal

class (Monad m) => SignatureReader m where
  readSignatures :: m Signatures
  readRetType :: m LLVM.Type

instance (Monad m) => SignatureReader (ReaderT ExprEnv m) where
  readSignatures = do
    ExprEnv { erSignatures = signatures, erValueMap = _ } <- ask
    return signatures
  readRetType = do
    env <- ask
    return $ erRetType env
instance (Monad m) => SignatureReader (ReaderT StatementEnv m) where
  readSignatures = do
    StatementEnv { seSignatures = signatures } <- ask
    return signatures
  readRetType = do
    env <- ask
    return $ seRetType env

class (Monad m) => InstructionEmitter m where
  getCurrentBlock :: m LLVM.Label
  emitInstruction :: LLVM.Instr -> m ()

instance (Monad m) => InstructionEmitter (StateT ExprState m) where
  getCurrentBlock = do
    state <- get
    return $ esCurrentBlock state

  emitInstruction instr = do
    state <- get
    let currentBlock = case instr of LLVM.ILabel label -> label
                                     _ -> esCurrentBlock state
    put state { esInstructions = esInstructions state ++ [instr]
              , esCurrentBlock = currentBlock }

instance (Monad m) => InstructionEmitter (StateT StatementState m) where
  getCurrentBlock = do
    state <- get
    return $ ssCurrentBlock state

  emitInstruction instr = do
    state <- get
    let currentBlock = case instr of LLVM.ILabel label -> label
                                     _ -> ssCurrentBlock state
    put state { ssInstructions = ssInstructions state ++ [instr]
              , ssCurrentBlock = currentBlock }

instance (InstructionEmitter m) => InstructionEmitter (ReaderT a m) where
  getCurrentBlock = lift getCurrentBlock
  emitInstruction = lift . emitInstruction

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
    state <- get
    let (register, nextRegister1) = getNextRegister (esNextRegister state)
    put $ state { esNextRegister = nextRegister1 }
    return register
  getNextLabelE = do
    state <- get
    let (label, nextRegister1) = getNextLabel (esNextRegister state)
    put $ state { esNextRegister = nextRegister1 }
    return label
instance (Monad m) => NextRegisterer (StateT StatementState m) where
  getNextRegisterE = do
    state <- get
    let (register, nextRegister1) = getNextRegister (ssNextRegister state)
    put state { ssNextRegister = nextRegister1 }
    return register
  getNextLabelE = do
    state <- get
    let (label, nextRegister1) = getNextLabel (ssNextRegister state)
    put $ state { ssNextRegister = nextRegister1 }
    return label
instance (NextRegisterer m) => NextRegisterer (ReaderT a m) where
  getNextRegisterE = lift getNextRegisterE
  getNextLabelE = lift getNextLabelE


getNextConstE :: ExprM String
getNextConstE = do
  state <- get
  let (constName, constCounter1 ) = getNextConst (esConstCounter state)
  put $ state { esConstCounter = constCounter1 }
  return constName

instance Monoid ExprWr where
  mappend ExprWr { ewGlobals = globals1 } ExprWr { ewGlobals = globals2 }
     = ExprWr { ewGlobals = globals1 ++ globals2 }
  mempty = ExprWr { ewGlobals = [] }

newtype ValueMap = ValueMap (M.Map VariableIdent (LLVM.Type, LLVM.Register))
newtype Signatures = Signatures (M.Map FunctionIdent LLVM.FunctionType)
newtype NextRegister = NextRegister Int

getType :: (Raiser m) => FunctionIdent -> Signatures -> CompilerErr.Position -> m LLVM.FunctionType
getType ident signatures position =
  maybe (raise
    CompilerErr.CEUndefinedFunction { CompilerErr.ceFunctionIdent = ident
                                    , CompilerErr.cePosition = position })
  return
  (getMaybeType ident signatures)

getMaybeType :: FunctionIdent -> Signatures -> Maybe LLVM.FunctionType
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

runStatementM :: Signatures
                 -> LLVM.Type
                 -> LLVM.Label
                 -> [VariableIdent]
                 -> ValueMap
                 -> NextRegister
                 -> ConstCounter
                 -> StatementM ()
                 -> CompilerErrorM (LLVM.Label, [LLVM.Instr], [LLVM.Constant], [VariableIdent], ValueMap, NextRegister, ConstCounter)
runStatementM signatures retType currentBlock0 newScopeVars0 valueMap0 nextRegister0 constCounter0 monad =
  do (((),
      StatementState { ssNextRegister = nextRegister1
                     , ssConstCounter = constCounter1
                     , ssValueMap = valueMap1
                     , ssNewScopeVars = newScopeVars1
                     , ssInstructions = instr
                     , ssCurrentBlock = currentBlock1 }),
       ExprWr { ewGlobals = globals }) <-
        runWriterT (runStateT (runReaderT monad
        StatementEnv { seSignatures = signatures,
                       seRetType = retType })
         StatementState { ssNextRegister = nextRegister0
                        , ssConstCounter = constCounter0
                        , ssValueMap = valueMap0
                        , ssNewScopeVars = newScopeVars0
                        , ssInstructions = initInstructions
                        , ssCurrentBlock = currentBlock0
                        })
     return (currentBlock1, instr, globals, newScopeVars1, valueMap1, nextRegister1, constCounter1)

initInstructions :: [LLVM.Instr]
initInstructions = []

exprInStatement :: ExprM t -> StatementM t
exprInStatement exprM =
  do signatures <- readSignatures
     retType <- readRetType
     StatementState { ssValueMap = valueMap
                    , ssConstCounter = constCounter0
                    , ssNextRegister = nextRegister0
                    , ssNewScopeVars = newScopeVars
                    , ssInstructions = instructions0
                    , ssCurrentBlock = currentBlock0  } <- get
     (res, ExprState { esNextRegister = nextRegister1
                     , esConstCounter = constCounter1
                     , esInstructions = instructions1
                     , esCurrentBlock = currentBlock1 }) <- lift $ lift $
         runStateT (runReaderT exprM
            ExprEnv { erSignatures = signatures
                    , erValueMap = valueMap
                    , erRetType = retType })
            ExprState { esNextRegister = nextRegister0
                      , esConstCounter = constCounter0
                      , esInstructions = instructions0
                      , esCurrentBlock = currentBlock0 }
     put StatementState { ssNextRegister = nextRegister1
                        , ssConstCounter = constCounter1
                        , ssValueMap = valueMap
                        , ssNewScopeVars = newScopeVars
                        , ssInstructions = instructions1
                        , ssCurrentBlock = currentBlock1  }
     return res

statementInExpr :: StatementM t -> ExprM t
statementInExpr statementM =
  do ExprEnv { erSignatures = signatures
             , erValueMap = valueMap
             , erRetType = retType } <- ask
     ExprState { esConstCounter = constCounter0
               , esNextRegister = nextRegister0
               , esInstructions = instructions0
               , esCurrentBlock = currentBlock0 } <- get
     (res, StatementState { ssNextRegister = nextRegister1
                          , ssConstCounter = constCounter1
                          , ssInstructions = instructions1
                          , ssCurrentBlock = currentBlock1 }) <- lift $ lift $
         runStateT (runReaderT statementM
            StatementEnv { seSignatures = signatures
                         , seRetType = retType })
            StatementState { ssNextRegister = nextRegister0
                           , ssConstCounter = constCounter0
                           , ssValueMap = valueMap
                           , ssNewScopeVars = initNewScopeVars
                           , ssInstructions = instructions0
                           , ssCurrentBlock = currentBlock0 }
     put ExprState { esNextRegister = nextRegister1
                   , esConstCounter = constCounter1
                   , esInstructions = instructions1
                   , esCurrentBlock = currentBlock1 }
     return res

initNextRegister :: NextRegister
initNextRegister = NextRegister 0

setVariable :: VariableIdent -> LLVM.Type -> LLVM.Register -> ValueMap -> ValueMap
setVariable name type_ value (ValueMap valueMap) =
    ValueMap (M.insert name (type_, value) valueMap)

lookupVariable :: (Raiser m) => VariableIdent -> ValueMap -> CompilerErr.Position -> m (LLVM.Type, LLVM.Register)
lookupVariable ident (ValueMap valueMap) position =
    maybe (raise
     CompilerErr.CEUndefinedVariable { CompilerErr.ceVariableIdent = ident
                                     , CompilerErr.cePosition = position })
      return (M.lookup ident valueMap)

setVariableM :: CompilerErr.Position -> VariableIdent -> LLVM.Type -> LLVM.Register -> StatementM ()
setVariableM position name type_ value =
  do state <- get
     when (name `elem` ssNewScopeVars state) $
        raise $ CompilerErr.CERedefinitionOfVariable position name
     put $ state { ssValueMap = setVariable name type_ value (ssValueMap state)
                 , ssNewScopeVars = name : ssNewScopeVars state }

initValueMap :: ValueMap
initValueMap = ValueMap M.empty

initNewScopeVars :: [VariableIdent]
initNewScopeVars = []
