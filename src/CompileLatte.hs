{-# OPTIONS_GHC -Wall -Werror #-}

module CompileLatte where

import qualified Data.Map as M
import qualified AbsLatte
import CompilerErr (CompilerErrorM)
import qualified CompilerErr
import Control.Monad (foldM, when)
import Data.Tuple (swap)

import qualified LLVM
import CompilerState

latteMain :: [String]
latteMain = [ "target triple = \"x86_64-apple-macosx10.13.0\""
            , "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1"
            , ""
            , "define void @printInt(i32 %num) {"
            , "  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %num)"
            , "  ret void"
            , "}"
            , "define void @printString(i8* %string) {"
            , "  call i32 @puts(i8* %string)"
            , "  ret void"
            , "}"
            , "declare i32 @printf(i8*, ...)"
            , "declare i32 @puts(i8*)"
            , "define i8* @concat(i8*, i8*) local_unnamed_addr #0 {"
              ,   "%3 = tail call i64 @strlen(i8* %0)"
              ,    "%4 = tail call i64 @strlen(i8* %1)"
              ,    "%5 = add i64 %4, %3"
              ,    "%6 = shl i64 %5, 32"
              ,    "%7 = add i64 %6, 4294967296"
              ,    "%8 = ashr exact i64 %7, 29"
              ,    "%9 = tail call i8* @malloc(i64 %8) #6"
              ,    "%10 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %9, i1 false, i1 true)"
              ,    "%11 = tail call i8* @__strcpy_chk(i8* %9, i8* %0, i64 %10) #7"
              ,    "%12 = tail call i8* @__strcat_chk(i8* %9, i8* %1, i64 %10) #7"
              ,    "ret i8* %9"
              ,  "}"
              ,  "declare i64 @strlen(i8* nocapture) local_unnamed_addr #1"
              ,  "declare noalias i8* @malloc(i64) local_unnamed_addr #2"
              ,  "declare i8* @__strcpy_chk(i8*, i8*, i64) local_unnamed_addr #3"
              ,  "declare i64 @llvm.objectsize.i64.p0i8(i8*, i1, i1) #4"
              ,  "declare i8* @__strcat_chk(i8*, i8*, i64) local_unnamed_addr #3"
              ]

emptyStringConst :: LLVM.Constant
emptyStringConst = LLVM.Constant 1 "empty_string" ""

mainFunctionName :: String
mainFunctionName = "main"
mainFunctionType :: LLVM.FunctionType
mainFunctionType = LLVM.FunctionType [] LLVM.Ti32

getPosition :: AbsLatte.CIdent -> CompilerErr.Position
getPosition (AbsLatte.CIdent ((row, col), _)) = CompilerErr.Position { CompilerErr.row = row
                                                   , CompilerErr.column = col }

checkDuplicateIdents :: [(String, CompilerErr.Position)] -> Maybe (String, CompilerErr.Position)
checkDuplicateIdents idents = case
  M.toList (M.filter (\l -> length l >= 2) (M.fromListWith (++) (map (\ (a, b) -> (a, [b])) idents))) of
    [] -> Nothing
    (str, pos : _) : _ -> Just (str, pos)
    ((_, []):_) -> error "unreachable"

checkDuplicateFnDefs :: [AbsLatte.TopDef] -> CompilerErrorM ()
checkDuplicateFnDefs topDefs = case checkDuplicateIdents positions of
    Nothing -> return ()
    Just (str, pos) -> CompilerErr.raiseCEDuplicatedFunctionDeclaration str pos
  where
    positions = ("printInt", CompilerErr.builtinPosition) :
                ("printString", CompilerErr.builtinPosition) :
                map getPositionPair topDefs
    getPositionPair (AbsLatte.FnDef _ ident _ _) =
      (compileFuncIdent ident, getPosition ident)

checkMainSignature :: Signatures -> CompilerErrorM ()
checkMainSignature signatures =
  case getMaybeType mainFunctionName signatures of
    Nothing -> CompilerErr.raiseCEMissingMainFunction
    Just type_ | type_ /= mainFunctionType -> CompilerErr.raiseCEIncorrectMainFunctionType
    _ -> return ()

collectSignatures :: [AbsLatte.TopDef] -> Signatures
collectSignatures topDefs = Signatures $ M.fromList pairs
  where
    pairs = ("printInt", LLVM.FunctionType [LLVM.Ti32] LLVM.Tvoid) :
            ("printString", LLVM.FunctionType [LLVM.Ti8Ptr] LLVM.Tvoid) :
            map getSignaturePair topDefs
    getSignaturePair (AbsLatte.FnDef retType ident args _) =
      (compileFuncIdent ident,
       LLVM.FunctionType
       (map (\ (AbsLatte.Arg type_ _) -> compileType type_) args)
       (compileType retType))

compileType :: AbsLatte.Type -> LLVM.Type
compileType AbsLatte.Int = LLVM.Ti32
compileType AbsLatte.Void = LLVM.Tvoid
compileType AbsLatte.Bool = LLVM.Ti1
compileType AbsLatte.Str = LLVM.Ti8Ptr
compileType (AbsLatte.Fun _ _) = error "unreachable"

compileLatte :: AbsLatte.Program -> CompilerErrorM String
compileLatte (AbsLatte.Program topDefs) =
   do checkDuplicateFnDefs topDefs
      let signatures = collectSignatures topDefs
      checkMainSignature signatures
      (funcLines, globalsLines, _) <- foldM (go signatures) ([], [emptyStringConst], initConstCounter) topDefs
      let allLines = concatMap LLVM.showFunc funcLines
      return $ unlines $ latteMain ++ map LLVM.showGlobal globalsLines ++ allLines

   where go signatures (functions, constants, constCounter0) topDef =
           do (newFunc, newConstants, constCounter1) <- compileFunc signatures topDef constCounter0
              return (functions ++ [newFunc], constants ++ newConstants, constCounter1)

compileFunc :: Signatures -> AbsLatte.TopDef -> ConstCounter -> CompilerErrorM (LLVM.Function, [LLVM.Constant], ConstCounter)
compileFunc signatures (AbsLatte.FnDef type_ ident args block) constCounter0 =
   do
      mapM_ (\ (num, AbsLatte.Arg argType _) ->
            checkNotVoid argType $ "invalid void function argument at position " ++ show num)
            (zip [(1 :: Int)..] args)
      (instrs, globals, _, _, constCounter1) <- runStatementM signatures initValueMap initNextRegister constCounter0 makeBody
      return (LLVM.Function (compileType type_) (compileFuncIdent ident) llvmArgs instrs, globals, constCounter1)
   where
     llvmArgs :: [(LLVM.Type, String)]
     llvmArgs = map (\ (AbsLatte.Arg argType argIdent) -> (compileType argType, compileVariableIdent argIdent)) args

     makeBody :: StatementM ()
     makeBody =
       do mapM_ saveArgument args
          safeValueMap $ nCompileBlock block
          emitInstruction nullRet

     saveArgument :: AbsLatte.Arg -> StatementM ()
     saveArgument (AbsLatte.Arg argType argIdent) =
       do ptr <- getNextRegisterE
          let llvmType = compileType argType
          emitInstruction $ LLVM.IAlloca llvmType ptr
          emitInstruction $ LLVM.IStore llvmType (LLVM.VRegister $ LLVM.RArgument (compileVariableIdent argIdent)) llvmType ptr
          setVariableM (compileVariableIdent argIdent) llvmType ptr

     nullRet | lType == LLVM.Tvoid = LLVM.IRetVoid
             | otherwise = LLVM.IRet lType (defaultValue lType)
     lType = compileType type_

defaultValue :: LLVM.Type -> LLVM.Value
defaultValue LLVM.Ti1  = LLVM.VFalse
defaultValue LLVM.Ti32 = LLVM.VConst 0
defaultValue LLVM.Tvoid = error "unreachable"
defaultValue LLVM.Ti8Ptr = LLVM.VGetElementPtr 1 "empty_string"

compileBlock :: Signatures -> AbsLatte.Block -> ValueMap -> NextRegister -> ConstCounter -> CompilerErrorM ([LLVM.Instr], [LLVM.Constant], NextRegister, ConstCounter)
compileBlock signatures block valueMap0 nextRegister0 constCounter0 =
  do (_, instr, const_, nextRegister1, nextRegister2) <- runExprM signatures valueMap0 nextRegister0 constCounter0 (nCompileBlock block)
     return (instr, const_, nextRegister1, nextRegister2)

nCompileBlock :: AbsLatte.Block -> ExprM ()
nCompileBlock (AbsLatte.Block stmts) =
  statementInExpr $ mapM_ nCompileStmt stmts

nCompileFlowBlock :: AbsLatte.Stmt -> LLVM.Label -> ExprM ()
nCompileFlowBlock stmt nextBlock =
  do statementInExpr $ nCompileStmt stmt
     emitInstruction (LLVM.IBr nextBlock)

compileAssign :: AbsLatte.Expr
                -> LLVM.Type
                -> LLVM.Register
                -> ExprM ()
compileAssign expr ptrType ptr =
  do (value, type_) <- nCompileExpr expr
     lift3 $ checkType type_ ptrType "wrong type at assignment"
     emitInstruction $ LLVM.IStore type_ value type_ ptr

nCompileStmt :: AbsLatte.Stmt -> StatementM ()
nCompileStmt AbsLatte.VRet =
  emitInstruction LLVM.IRetVoid

nCompileStmt (AbsLatte.Incr _) = error "not yet implemented"
nCompileStmt (AbsLatte.Decr _) = error "not yet implemented"

nCompileStmt AbsLatte.Empty = return ()

nCompileStmt (AbsLatte.SExp expr) =
   do (_, _) <- safeValueMap (nCompileExpr expr)
      return ()

nCompileStmt (AbsLatte.Ret expr)=
  do (value, type_) <- safeValueMap (nCompileExpr expr)
     emitInstruction $ LLVM.IRet type_ value

nCompileStmt (AbsLatte.Ass ident expr) =
  do valueMap <- readValueMapS
     (type_, ptr) <- lift3 $ lookupVariable (compileVariableIdent ident) valueMap (getPosition ident)
     safeValueMap $ compileAssign expr type_ ptr

nCompileStmt (AbsLatte.Decl type_ decls)=
   do lift3 $ checkNotVoid type_ "void variable declarations illegal"
      ptrs <- mapM go decls
      mapM_ (\ (decl, ptr) -> setVariableM (compileVariableIdent (getIdent decl)) llvmType ptr) (zip decls ptrs)

   where
      llvmType = compileType type_

      getIdent (AbsLatte.Init ident _) = ident
      getIdent (AbsLatte.NoInit ident) = ident

      storeValue (AbsLatte.Init _ expr) ptr =
        safeValueMap (compileAssign expr llvmType ptr)
      storeValue (AbsLatte.NoInit _) ptr =
        emitInstruction $ LLVM.IStore llvmType (defaultValue llvmType) llvmType ptr

      go declItem =
           do ptr <- getNextRegisterE
              emitInstruction $ LLVM.IAlloca llvmType ptr
              storeValue declItem ptr
              return ptr

nCompileStmt (AbsLatte.BStmt block) = safeValueMap $ nCompileBlock block

nCompileStmt (AbsLatte.Cond expr stmt1) =
  do (cond, type_) <- safeValueMap $ nCompileExpr expr
     lift3 $ checkType type_ LLVM.Ti1 "if condition"
     ifTrueBlock <- getNextLabelE
     contBlock <- getNextLabelE
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock contBlock
     emitInstruction $ LLVM.ILabel ifTrueBlock
     safeValueMap $ nCompileFlowBlock stmt1 contBlock
     emitInstruction $ LLVM.ILabel contBlock

nCompileStmt (AbsLatte.CondElse expr stmt1 stmt2) =
  do (cond, type_) <- safeValueMap $ nCompileExpr expr
     lift3 $ checkType type_ LLVM.Ti1 "if-else condition"
     ifTrueBlock <- getNextLabelE
     ifElseBlock <- getNextLabelE
     contBlock <- getNextLabelE
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock ifElseBlock
     emitInstruction $ LLVM.ILabel ifTrueBlock
     safeValueMap $ nCompileFlowBlock stmt1 contBlock
     emitInstruction $ LLVM.ILabel ifElseBlock
     safeValueMap $ nCompileFlowBlock stmt2 contBlock
     emitInstruction $ LLVM.ILabel contBlock

nCompileStmt (AbsLatte.While expr stmt) =
  do condBlock <- getNextLabelE
     bodyBlock <- getNextLabelE
     contBlock <- getNextLabelE
     emitInstruction $ LLVM.IBr condBlock
     emitInstruction $ LLVM.ILabel bodyBlock
     safeValueMap $ nCompileFlowBlock stmt condBlock
     emitInstruction $ LLVM.ILabel condBlock
     (cond, type_) <- safeValueMap $ nCompileExpr expr
     lift3 $ checkType type_ LLVM.Ti1 "while loop condition"
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond bodyBlock contBlock
     emitInstruction $ LLVM.ILabel contBlock

compileStmt :: Signatures
                 -> AbsLatte.Stmt
                 -> ValueMap
                 -> NextRegister
                 -> ConstCounter
                 -> Either
                      CompilerErr.CompilerError
                      ([LLVM.Instr], [LLVM.Constant], ValueMap, NextRegister,
                       ConstCounter)
compileStmt signatures stmt valueMap nextReg constCounter =
  runStatementM signatures valueMap nextReg constCounter (nCompileStmt stmt)

nCompileExpr :: AbsLatte.Expr -> ExprM (LLVM.Value, LLVM.Type)
nCompileExpr (AbsLatte.EVar ident) =
  do valueMap <- readValueMap
     (type_, ptr) <- lift3 $ lookupVariable (compileVariableIdent ident) valueMap (getPosition ident)
     reg <- getNextRegisterE
     emitInstruction $ LLVM.ILoad type_ type_ ptr reg
     return (LLVM.VRegister reg, type_)

nCompileExpr (AbsLatte.EApp ident args) =
  do compiledArgs <- mapM nCompileExpr args
     signatures <- readSignatures
     (LLVM.FunctionType expectedArgTypes retType) <- lift3 $ getType (compileFuncIdent ident) signatures (getPosition ident)
     let argTypes = map snd compiledArgs
     mapM_ (\ (num, actType, expType) -> (lift3 $ checkType actType expType ("argument " ++ show num ++ " in function call")))
           (zip3 [(1::Int)..] argTypes expectedArgTypes)
     when (length argTypes /= length expectedArgTypes) (lift3 $ CompilerErr.raiseCETypeError "wrong number of function arguments")
     if retType == LLVM.Tvoid then do
       emitInstruction $ LLVM.ICall retType (compileFuncIdent ident) (map swap compiledArgs) Nothing
       return (LLVM.VConst 0, retType)
     else do
       register <- getNextRegisterE
       emitInstruction $ LLVM.ICall retType (compileFuncIdent ident) (map swap compiledArgs) (Just register)
       return (LLVM.VRegister register, retType)

nCompileExpr (AbsLatte.ELitInt int) = return (LLVM.VConst int, LLVM.Ti32)
nCompileExpr AbsLatte.ELitTrue = return (LLVM.VTrue, LLVM.Ti1)
nCompileExpr AbsLatte.ELitFalse = return (LLVM.VFalse, LLVM.Ti1)

nCompileExpr (AbsLatte.EString string) =
  do constName <- getNextConstE
     emitGlobal $ LLVM.Constant (length string + 1) constName string
     return (LLVM.VGetElementPtr (length string + 1) constName, LLVM.Ti8Ptr)

nCompileExpr (AbsLatte.Neg _) = error "not yet implemented"
nCompileExpr (AbsLatte.Not _) = error "not yet implemented"
nCompileExpr (AbsLatte.EAnd _ _) = error "not yet implemented"
nCompileExpr (AbsLatte.EOr _ _) = error "not yet implemented"

nCompileExpr (AbsLatte.EAdd exp1 addOp exp2) =
  compileArithm exp1 (compileAddOperator addOp) exp2
nCompileExpr (AbsLatte.EMul exp1 mulOp exp2) =
  compileArithm exp1 (compileMulOperator mulOp) exp2
nCompileExpr (AbsLatte.ERel exp1 relOp exp2) =
  compileArithm exp1 (compileRelOp relOp) exp2

data Operation = Add | Sub | Mul | Div | Mod
                 | LessThan | LessEqual
                 | GreaterThan | GreaterEqual | Equal | NotEqual
                 deriving Eq

operations :: [ (LLVM.Type, Operation, LLVM.Type, LLVM.Type, LLVM.Value -> LLVM.Value -> LLVM.Register -> LLVM.Instr)]
operations = [ (LLVM.Ti32, op, LLVM.Ti32, LLVM.Ti32,
                  \ v1 v2 reg -> LLVM.IArithm LLVM.Ti32 v1 v2 llvmOp reg
                  )   | (op, llvmOp) <- [ (Add, LLVM.OAdd), (Sub, LLVM.OSub)
                                       , (Mul, LLVM.OMul), (Div, LLVM.OSDiv),
                                         (Mod, LLVM.OSRem)
                                   ]] ++
              [ (LLVM.Ti8Ptr, Add, LLVM.Ti8Ptr, LLVM.Ti8Ptr,
                  \ v1 v2 reg -> LLVM.ICall LLVM.Ti8Ptr "concat"
                           [(LLVM.Ti8Ptr, v1), (LLVM.Ti8Ptr, v2)]
                           (Just reg)) ] ++
              [ (LLVM.Ti32, op, LLVM.Ti32, LLVM.Ti1,
                  LLVM.IIcmp relOp LLVM.Ti32)
                  | (op, relOp) <- [ (LessThan, LLVM.RelOpSLT)
                                   , (GreaterThan, LLVM.RelOpSGT)
                                   , (LessEqual, LLVM.RelOpSLE)
                                   , (GreaterEqual, LLVM.RelOpSGE)
                                   , (Equal, LLVM.RelOpEQ)
                                   , (NotEqual, LLVM.RelOpNE)
                                   ]
              ]

compileRelOp :: AbsLatte.RelOp -> Operation
compileRelOp AbsLatte.GE  = GreaterEqual
compileRelOp AbsLatte.GTH = GreaterThan
compileRelOp AbsLatte.LE  = LessEqual
compileRelOp AbsLatte.LTH = LessThan
compileRelOp AbsLatte.EQU = Equal
compileRelOp AbsLatte.NE = NotEqual

compileArithm :: AbsLatte.Expr -> Operation -> AbsLatte.Expr -> ExprM (LLVM.Value, LLVM.Type)
compileArithm exp1 op exp2 =
  do (val1, type1) <- nCompileExpr exp1
     (val2, type2) <- nCompileExpr exp2
     (retType, instr) <- lift3 $ getInstr type1 type2
     reg <- getNextRegisterE
     emitInstruction $ instr val1 val2 reg
     return (LLVM.VRegister reg, retType)

  where
    getInstr type1 type2 =
      case filter (\ (a, b, c, _, _) -> (a, b, c) == (type1, op, type2)) operations of
        [] -> CompilerErr.raiseCETypeError "incorrect binary operation"
        (_, _, _, retType, instr) : _ -> return (retType, instr)

checkType :: LLVM.Type -> LLVM.Type -> String -> CompilerErrorM ()
checkType type1 type2 description | type1 == type2 = return ()
                                  | otherwise = CompilerErr.raiseCETypeError description

checkNotVoid :: AbsLatte.Type -> String -> CompilerErrorM ()
checkNotVoid type1 description | type1 == AbsLatte.Void = CompilerErr.raiseCETypeError description
                               | otherwise = return ()

compileAddOperator :: AbsLatte.AddOp -> Operation
compileAddOperator AbsLatte.Plus = Add
compileAddOperator AbsLatte.Minus = Sub

compileMulOperator :: AbsLatte.MulOp -> Operation
compileMulOperator AbsLatte.Div = Div
compileMulOperator AbsLatte.Mod = Mod
compileMulOperator AbsLatte.Times = Mul

compileFuncIdent :: AbsLatte.CIdent -> String
compileFuncIdent (AbsLatte.CIdent (_, str)) = str
compileVariableIdent :: AbsLatte.CIdent -> String
compileVariableIdent (AbsLatte.CIdent (_, str)) = str
