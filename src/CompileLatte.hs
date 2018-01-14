{-# OPTIONS_GHC -Wall -Werror #-}

-- translates Latte AST to LLVM

module CompileLatte (compileLatte, Position, compileLatteToX86_64, latteToLLVM) where

import qualified Data.Map as M
import qualified AbsLatte
import qualified CompilerErr as CE
import qualified LatteCommon
import Control.Monad (foldM, when)
import Data.Maybe (mapMaybe)

import qualified LLVM
import qualified TransLLVM
import qualified X86_64
import CompilerState

llvmHeader :: [String]
llvmHeader = [ "target triple = \"x86_64-apple-macosx10.13.0\"" ]

builtins :: [(AbsLatte.CIdent, LatteCommon.FunctionType)]
builtins = [ (AbsLatte.CIdent "printInt"
             , LatteCommon.FunctionType [LatteCommon.Int] LatteCommon.Void)
           , (AbsLatte.CIdent "printString"
             , LatteCommon.FunctionType [LatteCommon.String] LatteCommon.Void)
           , (AbsLatte.CIdent "readInt"
             , LatteCommon.FunctionType [] LatteCommon.Int)
           , (AbsLatte.CIdent "readString"
             , LatteCommon.FunctionType [] LatteCommon.String)
           , (AbsLatte.CIdent "error"
             , LatteCommon.FunctionType [] LatteCommon.Void )
           ]

hiddenBuiltins :: [LLVM.Declare]
hiddenBuiltins =
           [ LLVM.Declare concatName $ LLVM.FunctionType [LLVM.Ptr LLVM.Ti8, LLVM.Ptr LLVM.Ti8] (LLVM.Ptr LLVM.Ti8)
           , LLVM.Declare streqName $ LLVM.FunctionType [LLVM.Ptr LLVM.Ti8, LLVM.Ptr LLVM.Ti8] LLVM.Ti1
           , LLVM.Declare strneName $ LLVM.FunctionType [LLVM.Ptr LLVM.Ti8, LLVM.Ptr LLVM.Ti8] LLVM.Ti1
           , LLVM.Declare mallocName $ LLVM.FunctionType [LLVM.Ti32] (LLVM.Ptr LLVM.Ti8)
           ]

concatName :: String
concatName = "concat"
streqName :: String
streqName = "streq"
strneName :: String
strneName = "strne"
mallocName :: String
mallocName = "malloc"

lengthSize :: Integer
lengthSize = 4  -- used in arrays

emptyStringConst :: LLVM.Constant
emptyStringConst = LLVM.Constant 1 "empty_string" ""

mainFunctionName :: AbsLatte.CIdent
mainFunctionName = AbsLatte.CIdent "main"
mainFunctionType :: LatteCommon.FunctionType
mainFunctionType = LatteCommon.FunctionType [] LatteCommon.Int

type Position = Maybe (Int, Int)

checkDuplicateIdents :: [(AbsLatte.CIdent, CE.Position)] -> Maybe (AbsLatte.CIdent, CE.Position)
checkDuplicateIdents idents = case
  M.toList (M.filter (\l -> length l >= 2) (M.fromListWith (++) (map (\ (a, b) -> (a, [b])) idents))) of
    [] -> Nothing
    (str, pos : _) : _ -> Just (str, pos)
    ((_, []):_) -> error "unreachable"

checkDuplicateFnDefs :: [AbsLatte.TopDef Position] -> CompilerErrorM ()
checkDuplicateFnDefs topDefs = case checkDuplicateIdents positions of
    Nothing -> return ()
    Just (ident, pos) -> raise
        CE.CEDuplicatedFunctionDeclaration { CE.ceFunctionIdent =  ident
                                                    , CE.cePosition = pos }
  where
    positions = [(ident, CE.builtinPosition) | (ident, _) <- builtins] ++
                map getPositionPair topDefs
    getPositionPair (AbsLatte.FnDef position _ ident _ _) =
      (ident, compilePosition position)

compilePosition :: Position -> CE.Position
compilePosition Nothing = error "unknown position passed"
compilePosition (Just (x, y)) =
  CE.Position { CE.row = x, CE.column = y}

checkMainSignature :: Signatures -> Position -> CompilerErrorM ()
checkMainSignature signatures mainPosition =
  case getMaybeType mainFunctionName signatures of
    Nothing -> raise CE.CEMissingMainFunction
    Just type_ | type_ /= mainFunctionType ->
      raise $ CE.CEIncorrectMainFunctionType (compilePosition mainPosition)
    _ -> return ()

collectSignatures :: [AbsLatte.TopDef Position] -> Signatures
collectSignatures topDefs = Signatures $ M.fromList pairs
  where
    pairs = builtins ++
            map getSignaturePair topDefs
    getSignaturePair (AbsLatte.FnDef _ retType ident args _) =
      (ident,
       LatteCommon.FunctionType
       (map (\ (AbsLatte.Arg _ type_ _) -> compileType type_) args)
       (compileType retType))

compileType :: AbsLatte.Type Position -> LatteCommon.Type
compileType (AbsLatte.Int _) = LatteCommon.Int
compileType (AbsLatte.Void _) = LatteCommon.Void
compileType (AbsLatte.Bool _) = LatteCommon.Boolean
compileType (AbsLatte.Str _) = LatteCommon.String
compileType AbsLatte.Fun {} = error "unreachable"
compileType (AbsLatte.Array _ type_) = LatteCommon.Array (compileType type_)

-- must be evaluated after checking that main function exists
getMainPosition :: [AbsLatte.TopDef Position] -> Position
getMainPosition topDefs =
  case mapMaybe go topDefs of
    [pos] -> pos
    _ -> error "unreachable"
  where
    go (AbsLatte.FnDef pos _ ident _ _)
      | ident == AbsLatte.CIdent "main" = Just pos
      | otherwise = Nothing

compileLatte :: AbsLatte.Program Position -> CompilerErrorM String
compileLatte program =
  do
    module_ <- latteToLLVM program
    return $ showModule module_
  where
    showModule (LLVM.Module globals declares functions) =
      unlines (
        llvmHeader ++
        map LLVM.showGlobal globals ++
        concatMap LLVM.showFunc functions ++
        map LLVM.showDeclare declares )

latteToLLVM :: AbsLatte.Program Position -> CompilerErrorM LLVM.Module
latteToLLVM (AbsLatte.Program _ topDefs) =
  do checkDuplicateFnDefs topDefs
     let signatures = collectSignatures topDefs
     checkMainSignature signatures (getMainPosition topDefs)
     (functions, globals, _) <- foldM (go signatures) ([], [emptyStringConst], initConstCounter) topDefs
     return  LLVM.Module { LLVM.mGlobals = globals
                               , LLVM.mDeclares = map builtinToLLVM builtins ++ hiddenBuiltins
                               , LLVM.mFunctions = functions }
  where
    go signatures (functions, constants, constCounter0) topDef =
           do (newFunc, newConstants, constCounter1) <- compileFunc signatures topDef constCounter0
              return (functions ++ [newFunc], constants ++ newConstants, constCounter1)
    builtinToLLVM (ident, type_) = LLVM.Declare (compileFuncIdent ident) (funcTypeToLLVM type_)
      where
        funcTypeToLLVM (LatteCommon.FunctionType argTypes retType) =
          LLVM.FunctionType (map typeToLLVM argTypes) (typeToLLVM retType)

compileLatteToX86_64 :: AbsLatte.Program Position -> CompilerErrorM String
compileLatteToX86_64 program = do
  llvm <- latteToLLVM program
  return $ X86_64.showAsm (X86_64.fromLLVM llvm)

compileFunc :: Signatures -> AbsLatte.TopDef Position -> ConstCounter
      -> CompilerErrorM (LLVM.Function, [LLVM.Constant], ConstCounter)
compileFunc signatures (AbsLatte.FnDef fPosition type_ ident args (AbsLatte.Block _ stmts)) constCounter0 =
   do
      mapM_ (\ (num, AbsLatte.Arg position argType _) ->
            checkNotVoid (compileType argType)
                         CE.CEVoidFunctionArgument
                         { CE.cePosition = compilePosition position
                         , CE.ceArgumentNumber = num })
            (zip [(1 :: Int)..] args)
      (_, instrs, globals, _, _, nextRegister, constCounter1) <-
        runStatementM signatures (compileType type_) (LLVM.Label 0)
                      initNewScopeVars initValueMap
                      initNextRegister constCounter0 makeBody
      let blocks = TransLLVM.instrsToBlocks instrs
          rawFunc = LLVM.Function (typeToLLVM (compileType type_))
                                  (compileFuncIdent ident) llvmArgs blocks
      (func, _) <- optimise rawFunc nextRegister
      when (TransLLVM.hasUnreachableInstruction func) $
        raise $ CE.CEMissingReturn ident (compilePosition fPosition)
      return (func, globals, constCounter1)
   where
     llvmArgs :: [(LLVM.Type, Maybe String)]
     llvmArgs = map (\ (AbsLatte.Arg _ argType argIdent) -> (typeToLLVM (compileType argType), Just (compileVariableIdent argIdent))) args

     makeBody :: StatementM ()
     makeBody =
       do label <- getNextLabelE
          emitInstruction $ LLVM.ILabel label
          mapM_ saveArgument args
          mapM_ compileStmt stmts
          emitInstruction closeFunc

     saveArgument :: AbsLatte.Arg Position -> StatementM ()
     saveArgument (AbsLatte.Arg pos argType argIdent) =
       do ptr <- getNextRegisterE
          let latteType = compileType argType
              llvmType = typeToLLVM latteType
          emitInstruction $ LLVM.IAlloca llvmType ptr
          emitInstruction $ LLVM.IStore llvmType
              (LLVM.VRegister $ LLVM.RArgument (compileVariableIdent argIdent))
              llvmType ptr
          setVariableM (compilePosition pos) argIdent latteType ptr

     closeFunc | lType == LatteCommon.Void = LLVM.IRetVoid
               | otherwise = LLVM.IUnreachable
     lType = compileType type_

     optimise func nextRegister0 = do
       let (func1, nextRegister1) = TransLLVM.mem2Reg func nextRegister0
       func2 <- TransLLVM.fixEqM optimiseStep func1
       return (func2, nextRegister1)

     optimiseStep func1 = do
       func2 <- TransLLVM.constantProp func1
       let func3 = TransLLVM.removeUnreachableBlocks func2
           func4 = TransLLVM.removeTrivialPhis func3
           func5 = TransLLVM.removeUnusedAssignments func4
       return func5

defaultValue :: LatteCommon.Type -> LLVM.Value
defaultValue LatteCommon.Boolean  = LLVM.VFalse
defaultValue LatteCommon.Int = LLVM.VConst 0
defaultValue LatteCommon.Void = error "unreachable"
defaultValue LatteCommon.String = LLVM.VGetElementPtr 1 "empty_string"
defaultValue (LatteCommon.Array _) = LLVM.VNull

typeToLLVM :: LatteCommon.Type -> LLVM.Type
typeToLLVM type_ = case type_ of
  LatteCommon.Void -> LLVM.Tvoid
  LatteCommon.Int -> LLVM.Ti32
  LatteCommon.Boolean -> LLVM.Ti1
  LatteCommon.String -> LLVM.Ptr LLVM.Ti8
  LatteCommon.Array _ -> LLVM.Ptr LLVM.Ti8

compileFlowBlock :: AbsLatte.Stmt Position -> LLVM.Label -> ExprM ()
compileFlowBlock stmt nextBlock =
  do statementInExpr $ compileStmt stmt
     emitInstruction (LLVM.IBr nextBlock)

compileAssign :: Position
                -> AbsLatte.Expr Position
                -> LatteCommon.Type
                -> LLVM.Register
                -> ExprM ()
compileAssign position expr ptrType ptr =
  do (value, type_) <- compileExpr expr
     checkType position ptrType type_ "assignment"
     emitInstruction $ LLVM.IStore (typeToLLVM type_) value
                                   (typeToLLVM type_) ptr

compileStmt :: AbsLatte.Stmt Position -> StatementM ()
compileStmt (AbsLatte.VRet position) =
  do retType <- readRetType
     checkType position retType LatteCommon.Void "return statement"
     emitInstruction LLVM.IRetVoid

compileStmt (AbsLatte.Incr pos ident) = compileIncrDecrHelper pos ident LLVM.OAdd
compileStmt (AbsLatte.Decr pos ident) = compileIncrDecrHelper pos ident LLVM.OSub

compileStmt (AbsLatte.Empty _) = return ()

compileStmt (AbsLatte.SExp _ expr) =
   do (_, _) <- exprInStatement (compileExpr expr)
      return ()

compileStmt (AbsLatte.Ret position expr)=
  do expectedRetType <- readRetType
     when (expectedRetType == LatteCommon.Void) $
        raise $ CE.CEExprReturnInVoid (compilePosition position)
     (value, type_) <- exprInStatement (compileExpr expr)
     checkType position expectedRetType type_ "return"
     emitInstruction $ LLVM.IRet (typeToLLVM type_) value

compileStmt (AbsLatte.Ass pos ident expr) =
  do valueMap <- readValueMapS
     (type_, ptr) <- lookupVariable ident valueMap (compilePosition pos)
     exprInStatement $ compileAssign pos expr type_ ptr

compileStmt (AbsLatte.Decl position type_ decls) =
   do checkNotVoid (compileType type_) (CE.CEVoidDeclaration (compilePosition position))
      ptrs <- exprInStatement $ mapM go decls
      mapM_ (\ (decl, ptr) -> setVariableM
                  (compilePosition (getDeclPos decl))
                  (getIdent decl)
                  latteType ptr)
              (zip decls ptrs)

   where
      latteType = compileType type_
      llvmType = typeToLLVM latteType

      getIdent (AbsLatte.Init _ ident _) = ident
      getIdent (AbsLatte.NoInit _ ident) = ident

      getDeclPos (AbsLatte.Init pos _ _) = pos
      getDeclPos (AbsLatte.NoInit pos _) = pos

      storeValue (AbsLatte.Init itemPos _ expr) ptr =
        compileAssign itemPos expr latteType ptr
      storeValue (AbsLatte.NoInit _ _) ptr =
        emitInstruction $ LLVM.IStore llvmType (defaultValue latteType) llvmType ptr

      go declItem =
           do ptr <- getNextRegisterE
              emitInstruction $ LLVM.IAlloca llvmType ptr
              storeValue declItem ptr
              return ptr

compileStmt (AbsLatte.BStmt _ (AbsLatte.Block _ stmts)) =
  exprInStatement $ statementInExpr $ mapM_ compileStmt stmts

compileStmt (AbsLatte.Cond position expr stmt1) =
  do (cond, type_) <- exprInStatement $ compileExpr expr
     checkType position LatteCommon.Boolean type_ "if condition"
     ifTrueBlock <- getNextLabelE
     contBlock <- getNextLabelE
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock contBlock
     emitInstruction $ LLVM.ILabel ifTrueBlock
     exprInStatement $ compileFlowBlock stmt1 contBlock
     emitInstruction $ LLVM.ILabel contBlock

compileStmt (AbsLatte.CondElse pos expr stmt1 stmt2) =
  do (cond, type_) <- exprInStatement $ compileExpr expr
     checkType pos LatteCommon.Boolean type_ "if-else condition"
     ifTrueBlock <- getNextLabelE
     ifElseBlock <- getNextLabelE
     contBlock <- getNextLabelE
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock ifElseBlock
     emitInstruction $ LLVM.ILabel ifTrueBlock
     exprInStatement $ compileFlowBlock stmt1 contBlock
     emitInstruction $ LLVM.ILabel ifElseBlock
     exprInStatement $ compileFlowBlock stmt2 contBlock
     emitInstruction $ LLVM.ILabel contBlock

compileStmt (AbsLatte.While position expr stmt) =
  do condBlock <- getNextLabelE
     bodyBlock <- getNextLabelE
     contBlock <- getNextLabelE
     emitInstruction $ LLVM.IBr condBlock
     emitInstruction $ LLVM.ILabel bodyBlock
     exprInStatement $ compileFlowBlock stmt condBlock
     emitInstruction $ LLVM.ILabel condBlock
     (cond, type_) <- exprInStatement $ compileExpr expr
     checkType position LatteCommon.Boolean type_ "while loop condition"
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond bodyBlock contBlock
     emitInstruction $ LLVM.ILabel contBlock

compileStmt (AbsLatte.SetItem pos arrayIdent indexExpr rightExpr) =
  do (elemPtr, elemType) <- exprInStatement $
       arrayElementPtr pos arrayIdent indexExpr
     (rightVal, rightValType) <- exprInStatement $ compileExpr rightExpr
     checkType pos elemType rightValType "setting array element"
     let llvmElemType = typeToLLVM elemType
     emitInstruction $ LLVM.IStore llvmElemType rightVal llvmElemType elemPtr
compileStmt AbsLatte.ForEach {} = error "unimplemented for each"

compileIncrDecrHelper :: Position -> AbsLatte.CIdent -> LLVM.ArithmOp -> StatementM ()
compileIncrDecrHelper pos ident arithmOp =
  do valueMap <- readValueMapS
     (type_, ptr) <- lookupVariable ident valueMap (compilePosition pos)
     checkType pos LatteCommon.Int type_ "incrementation"
     operand <- getNextRegisterE
     result <- getNextRegisterE
     emitInstruction $ LLVM.ILoad LLVM.Ti32 LLVM.Ti32 ptr operand
     emitInstruction $ LLVM.IArithm LLVM.Ti32 (LLVM.VRegister operand) (LLVM.VConst 1) arithmOp result
     emitInstruction $ LLVM.IStore LLVM.Ti32 (LLVM.VRegister result) LLVM.Ti32 ptr

compileExpr :: AbsLatte.Expr Position -> ExprM (LLVM.Value, LatteCommon.Type)
compileExpr (AbsLatte.EVar pos ident) =
  do valueMap <- readValueMap
     (type_, ptr) <- lookupVariable ident valueMap (compilePosition pos)
     reg <- getNextRegisterE
     emitInstruction $ LLVM.ILoad (typeToLLVM type_) (typeToLLVM type_) ptr reg
     return (LLVM.VRegister reg, type_)

compileExpr (AbsLatte.EApp pos ident args) =
  do compiledArgs <- mapM compileExpr args
     signatures <- readSignatures
     (LatteCommon.FunctionType expectedArgTypes retType) <- getType ident signatures (compilePosition pos)
     let argTypes = map snd compiledArgs
     mapM_ (\ (num, actType, expType) ->
              (checkType pos expType actType ("argument " ++ show num ++ " in function call")))
           (zip3 [(1::Int)..] argTypes expectedArgTypes)
     when (length argTypes /= length expectedArgTypes) (
        raise $ CE.CEWrongNumberOfFunctionArguments
          (compilePosition pos) (length expectedArgTypes) (length argTypes) ident)
     let llvmArgs = map (\(val, latteType) -> (typeToLLVM latteType, val)) compiledArgs
     if retType == LatteCommon.Void then do
       emitInstruction $ LLVM.ICall (typeToLLVM retType) (compileFuncIdent ident) llvmArgs Nothing
       return (LLVM.VConst 0, retType)
     else do
       register <- getNextRegisterE
       emitInstruction $ LLVM.ICall (typeToLLVM retType) (compileFuncIdent ident) llvmArgs (Just register)
       return (LLVM.VRegister register, retType)

compileExpr (AbsLatte.ELitInt pos int) =
  do
    when (int < lowerBound || upperBound < int) $
      raise $ CE.CEIntLiteralOutOfBounds (compilePosition pos)
       int lowerBound upperBound
    return (LLVM.VConst int, LatteCommon.Int)

  where
    lowerBound = -2147483648
    upperBound = 2147483647
compileExpr (AbsLatte.ELitTrue _) = return (LLVM.VTrue, LatteCommon.Boolean)
compileExpr (AbsLatte.ELitFalse _) = return (LLVM.VFalse, LatteCommon.Boolean)

compileExpr (AbsLatte.EString _ string) =
  do constName <- getNextConstE
     emitGlobal $ LLVM.Constant (length llvmString + 1) constName llvmString
     return (LLVM.VGetElementPtr (length llvmString + 1) constName, LatteCommon.String)

  where
    llvmString = unescape (unquot string)
    unquot :: String -> String
    unquot (pQuot : (rest @ (_ : _))) =
      if (pQuot /= '"') || (last rest /= '"') then
            error "incorrect string, missing quots"
         else
            init rest
    unquot _ = error "incorrect string, too short"

    unescape [] = []
    unescape ('\\' : 'n' : s) = '\n' : unescape s
    unescape ('\\' : 't' : s) = '\t' : unescape s
    unescape ('\\' : '"' : s) = '"' : unescape s
    unescape ('\\' : '\\' : s) = '\\' : unescape s
    unescape ('\\': s) = '\\' : unescape s
    unescape (a : s) = a : unescape s

compileExpr (AbsLatte.Neg pos expr) =
  do (value, type_) <- compileExpr expr
     checkType pos LatteCommon.Int type_ "negation"
     res <- getNextRegisterE
     emitInstruction $ LLVM.IArithm LLVM.Ti32 (LLVM.VConst 0) value LLVM.OSub res
     return (LLVM.VRegister res, LatteCommon.Int)

compileExpr (AbsLatte.Not pos expr) =
  do (value, type_) <- compileExpr expr
     checkType pos LatteCommon.Boolean type_ "boolean not"
     res <- getNextRegisterE
     emitInstruction $ LLVM.IIcmp LLVM.RelOpEQ LLVM.Ti1 LLVM.VFalse value res
     return (LLVM.VRegister res, LatteCommon.Boolean)

compileExpr (AbsLatte.EAnd pos expr1 expr2) =
  compileBooleanOpHelper pos LLVM.VFalse expr1 expr2
compileExpr (AbsLatte.EOr pos expr1 expr2) =
  compileBooleanOpHelper pos LLVM.VTrue expr1 expr2

compileExpr (AbsLatte.EAdd pos exp1 addOp exp2) =
  compileArithm pos exp1 (compileAddOperator addOp) exp2
compileExpr (AbsLatte.EMul pos exp1 mulOp exp2) =
  compileArithm pos exp1 (compileMulOperator mulOp) exp2
compileExpr (AbsLatte.ERel pos exp1 relOp exp2) =
  compileArithm pos exp1 (compileRelOp relOp) exp2

compileExpr (AbsLatte.EAt pos ident numExpr) =
  do (elemPtr, elemType) <- arrayElementPtr pos ident numExpr
     elem_ <- getNextRegisterE
     let llvmElemType = typeToLLVM elemType
     emitInstruction $ LLVM.ILoad llvmElemType
                                  llvmElemType elemPtr
                                  elem_
     return (LLVM.VRegister elem_, elemType)

compileExpr (AbsLatte.ELength pos expr) =
  do (byteArray, exprType) <- compileExpr expr
     _ <- elementType exprType pos
     lengthPtr <- getNextRegisterE
     emitInstruction $ LLVM.IBitcast (LLVM.Ptr LLVM.Ti8, byteArray)
                                     (LLVM.Ptr LLVM.Ti32) lengthPtr
     length_ <- getNextRegisterE
     emitInstruction $ LLVM.ILoad LLVM.Ti32 LLVM.Ti32 lengthPtr length_
     return (LLVM.VRegister length_, LatteCommon.Int)

compileExpr (AbsLatte.ENew pos absType numExpr) =
  -- TODO: assign default value
  do (num, numType) <- compileExpr numExpr
     let latteType = compileType absType
     checkType pos LatteCommon.Int numType "new"
     augNum <- getNextRegisterE
     emitInstruction $ LLVM.IArithm LLVM.Ti32 num (LLVM.VConst lengthSize)
                       LLVM.OAdd augNum
     array <- getNextRegisterE
     emitInstruction $ LLVM.ICall (LLVM.Ptr LLVM.Ti8) mallocName
                       [(LLVM.Ti32, LLVM.VRegister augNum)]
                       (Just array)
     lengthPtr <- getNextRegisterE
     emitInstruction $ LLVM.IBitcast (LLVM.Ptr LLVM.Ti8, LLVM.VRegister array)
                       (LLVM.Ptr LLVM.Ti32) lengthPtr
     emitInstruction $ LLVM.IStore LLVM.Ti32 num
                                   LLVM.Ti32 lengthPtr
     return (LLVM.VRegister array, LatteCommon.Array latteType)

arrayElementPtr :: Position -> AbsLatte.CIdent -> AbsLatte.Expr Position
                   -> ExprM (LLVM.Register, LatteCommon.Type)
arrayElementPtr pos arrayIdent numExpr =
  do (num, numType) <- compileExpr numExpr
     checkType pos LatteCommon.Int numType "array index"
     valueMap <- readValueMap
     (arrayType, arrayPtr) <- lookupVariable arrayIdent valueMap (compilePosition pos)
     array <- getNextRegisterE
     emitInstruction $ LLVM.ILoad (typeToLLVM arrayType) (typeToLLVM arrayType)
                                  arrayPtr array
     elemType <- elementType arrayType pos
     let llvmElemType = typeToLLVM elemType
     elemByteArray <- getNextRegisterE
     emitInstruction $ LLVM.IGetElementPtr LLVM.Ti8
                                           (LLVM.Ptr LLVM.Ti8, LLVM.VRegister array)
                                           (LLVM.Ti32, LLVM.VConst lengthSize)
                                           elemByteArray
     elemArray <- getNextRegisterE
     emitInstruction $ LLVM.IBitcast (LLVM.Ptr LLVM.Ti8, LLVM.VRegister elemByteArray)
                                     (LLVM.Ptr llvmElemType) elemArray
     elemPtr <- getNextRegisterE
     emitInstruction $ LLVM.IGetElementPtr llvmElemType
                                           (LLVM.Ptr llvmElemType, LLVM.VRegister elemArray)
                                           (LLVM.Ti32, num) elemPtr
     return (elemPtr, elemType)


getOpInst :: LatteCommon.Type -> LatteCommon.Operation -> LatteCommon.Type
             -> Maybe (LatteCommon.Type, LLVM.Value -> LLVM.Value -> LLVM.Register -> LLVM.Instr)
getOpInst type1 op type2 = case (type1, op, type2) of
  ( LatteCommon.Int, LatteCommon.Add, LatteCommon.Int ) -> arithm
  ( LatteCommon.Int, LatteCommon.Sub, LatteCommon.Int ) -> arithm
  ( LatteCommon.Int, LatteCommon.Mul, LatteCommon.Int ) -> arithm
  ( LatteCommon.Int, LatteCommon.Div, LatteCommon.Int ) -> arithm
  ( LatteCommon.Int, LatteCommon.Mod, LatteCommon.Int ) -> arithm

  ( LatteCommon.String, LatteCommon.Add, LatteCommon.String ) -> concat_

  ( LatteCommon.Int, LatteCommon.LessThan, LatteCommon.Int ) -> rel
  ( LatteCommon.Int, LatteCommon.GreaterThan, LatteCommon.Int ) -> rel
  ( LatteCommon.Int, LatteCommon.LessEqual, LatteCommon.Int ) -> rel
  ( LatteCommon.Int, LatteCommon.GreaterEqual, LatteCommon.Int ) -> rel
  ( LatteCommon.Int, LatteCommon.Equal, LatteCommon.Int ) -> rel
  ( LatteCommon.Int, LatteCommon.NotEqual, LatteCommon.Int ) -> rel

  ( LatteCommon.Boolean, LatteCommon.Equal, LatteCommon.Boolean ) -> rel
  ( LatteCommon.Boolean, LatteCommon.NotEqual, LatteCommon.Boolean ) -> rel

  ( LatteCommon.String, LatteCommon.Equal, LatteCommon.String ) -> stringRel
  ( LatteCommon.String, LatteCommon.NotEqual, LatteCommon.String ) -> stringRel

  _ -> Nothing

  where
    arithm = Just ( LatteCommon.Int
             , \ v1 v2 reg -> LLVM.IArithm LLVM.Ti32 v1 v2 arithmOp reg )
    arithmOp = case op of LatteCommon.Add -> LLVM.OAdd
                          LatteCommon.Sub -> LLVM.OSub
                          LatteCommon.Mul -> LLVM.OMul
                          LatteCommon.Div -> LLVM.OSDiv
                          LatteCommon.Mod -> LLVM.OSRem
                          _ -> error "unreachable"

    concat_ = Just ( LatteCommon.String
              , \ v1 v2 reg -> LLVM.ICall (LLVM.Ptr LLVM.Ti8) concatName
                           [(LLVM.Ptr LLVM.Ti8, v1), (LLVM.Ptr LLVM.Ti8, v2)]
                           (Just reg) )

    rel = Just ( LatteCommon.Boolean, LLVM.IIcmp relOp (typeToLLVM type1) )
    relOp = case op of LatteCommon.LessThan -> LLVM.RelOpSLT
                       LatteCommon.GreaterThan -> LLVM.RelOpSGT
                       LatteCommon.LessEqual -> LLVM.RelOpSLE
                       LatteCommon.GreaterEqual -> LLVM.RelOpSGE
                       LatteCommon.Equal -> LLVM.RelOpEQ
                       LatteCommon.NotEqual -> LLVM.RelOpNE
                       _ -> error "unreachable"

    stringRel = Just ( LatteCommon.Boolean
                , \ v1 v2 reg -> LLVM.ICall LLVM.Ti1 strPred
                    [(LLVM.Ptr LLVM.Ti8, v1), (LLVM.Ptr LLVM.Ti8, v2)]
                    (Just reg) )
    strPred = case op of LatteCommon.Equal -> streqName
                         LatteCommon.NotEqual -> strneName
                         _ -> error "unreachable"

compileRelOp :: AbsLatte.RelOp a -> LatteCommon.Operation
compileRelOp (AbsLatte.GE _)  = LatteCommon.GreaterEqual
compileRelOp (AbsLatte.GTH _) = LatteCommon.GreaterThan
compileRelOp (AbsLatte.LE _)  = LatteCommon.LessEqual
compileRelOp (AbsLatte.LTH _) = LatteCommon.LessThan
compileRelOp (AbsLatte.EQU _) = LatteCommon.Equal
compileRelOp (AbsLatte.NE _) = LatteCommon.NotEqual

compileArithm :: Position
                -> AbsLatte.Expr Position
                -> LatteCommon.Operation
                -> AbsLatte.Expr Position
                -> ExprM (LLVM.Value, LatteCommon.Type)
compileArithm position exp1 op exp2 =
  do (val1, type1) <- compileExpr exp1
     (val2, type2) <- compileExpr exp2
     (retType, instr) <- getInstr type1 type2
     reg <- getNextRegisterE
     emitInstruction $ instr val1 val2 reg
     return (LLVM.VRegister reg, retType)

  where
    getInstr type1 type2 =
      case getOpInst type1 op type2 of
        Nothing -> raise $ CE.CEInvalidBinaryOp (compilePosition position) type1 op type2
        Just (retType, instr) -> return (retType, instr)

compileBooleanOpHelper :: Position
                          -> LLVM.Value
                          -> AbsLatte.Expr Position
                          -> AbsLatte.Expr Position
                          -> ExprM (LLVM.Value, LatteCommon.Type)
compileBooleanOpHelper position skipValue expr1 expr2 =
  do (val1, type1) <- compileExpr expr1
     checkType position LatteCommon.Boolean type1 "first operand in && expression"
     expr2Block <- getNextLabelE
     andEndBlock <- getNextLabelE
     expr1Block <- getCurrentBlock
     emitInstruction $ jumpOrder (LLVM.IBrCond LLVM.Ti1 val1) expr2Block andEndBlock

     emitInstruction $ LLVM.ILabel expr2Block
     (val2, type2) <- compileExpr expr2
     checkType position LatteCommon.Boolean type2 "second operand in && expression"
     emitInstruction $ LLVM.IBr andEndBlock
     expr2Block' <- getCurrentBlock

     emitInstruction $ LLVM.ILabel andEndBlock
     res <- getNextRegisterE
     emitInstruction $ LLVM.IPhi LLVM.Ti1 [(skipValue, expr1Block), (val2, expr2Block')] res
     return (LLVM.VRegister res, LatteCommon.Boolean)
  where
    jumpOrder f a b | skipValue == LLVM.VFalse = f a b
                    | skipValue == LLVM.VTrue = f b a
                    | otherwise = error "unreachable"

checkType :: (Raiser m) => Position -> LatteCommon.Type -> LatteCommon.Type -> String -> m ()
checkType pos expType actType description | expType == actType = return ()
                                          | otherwise = raise $
                                          CE.CETypeError (compilePosition pos) expType actType description

checkNotVoid :: (Raiser m) => LatteCommon.Type -> CE.CompilerError -> m ()
checkNotVoid LatteCommon.Void ce = raise ce
checkNotVoid _ _ = return ()

elementType :: LatteCommon.Type -> Position -> ExprM LatteCommon.Type
elementType type_ pos_ = case type_ of
  LatteCommon.Array elemType -> return elemType
  other -> raise CE.CEArrayTypeError { CE.cePosition = compilePosition pos_
                                     , CE.ceActualType = other }

compileAddOperator :: AbsLatte.AddOp a -> LatteCommon.Operation
compileAddOperator (AbsLatte.Plus _) = LatteCommon.Add
compileAddOperator (AbsLatte.Minus _) = LatteCommon.Sub

compileMulOperator :: AbsLatte.MulOp a -> LatteCommon.Operation
compileMulOperator (AbsLatte.Div _) = LatteCommon.Div
compileMulOperator (AbsLatte.Mod _) = LatteCommon.Mod
compileMulOperator (AbsLatte.Times _) = LatteCommon.Mul

compileFuncIdent :: AbsLatte.CIdent -> String
compileFuncIdent (AbsLatte.CIdent str) | str == "main" = str
                                            | otherwise = "latte_" ++ str
compileVariableIdent :: AbsLatte.CIdent -> String
compileVariableIdent (AbsLatte.CIdent str) = str
