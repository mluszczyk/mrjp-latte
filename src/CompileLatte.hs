{-# OPTIONS_GHC -Wall -Werror #-}

module CompileLatte (compileLatte) where

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
            , "declare i8* @concat(i8*, i8*)"
            , "declare i1 @streq(i8* nocapture readonly, i8* nocapture readonly)"
            , "declare i1 @strne(i8* nocapture readonly, i8* nocapture readonly)"
            ]

builtins :: [(String, LLVM.FunctionType)]
builtins = [ ("printInt", LLVM.FunctionType [LLVM.Ti32] LLVM.Tvoid)
           , ("printString", LLVM.FunctionType [LLVM.Ti8Ptr] LLVM.Tvoid)
           , ("readInt", LLVM.FunctionType [] LLVM.Ti32)
           , ("readString", LLVM.FunctionType [] LLVM.Ti8Ptr)
           , ("error", LLVM.FunctionType [] LLVM.Tvoid)
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
    positions = [(name, CompilerErr.builtinPosition) | (name, _) <- builtins] ++
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
    pairs = builtins ++
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
      return $ unlines $
        latteMain ++
        map LLVM.showGlobal globalsLines ++
        allLines ++
        map (uncurry LLVM.showGlobalDecl) builtins

   where go signatures (functions, constants, constCounter0) topDef =
           do (newFunc, newConstants, constCounter1) <- compileFunc signatures topDef constCounter0
              return (functions ++ [newFunc], constants ++ newConstants, constCounter1)

compileFunc :: Signatures -> AbsLatte.TopDef -> ConstCounter -> CompilerErrorM (LLVM.Function, [LLVM.Constant], ConstCounter)
compileFunc signatures (AbsLatte.FnDef type_ ident args (AbsLatte.Block stmts)) constCounter0 =
   do
      mapM_ (\ (num, AbsLatte.Arg argType _) ->
            checkNotVoid argType $ "invalid void function argument at position " ++ show num)
            (zip [(1 :: Int)..] args)
      (_, instrs, globals, _, _, _, constCounter1) <- runStatementM signatures (LLVM.Label 0) initNewScopeVars initValueMap initNextRegister constCounter0 makeBody
      return (LLVM.Function (compileType type_) (compileFuncIdent ident) llvmArgs instrs, globals, constCounter1)
   where
     llvmArgs :: [(LLVM.Type, String)]
     llvmArgs = map (\ (AbsLatte.Arg argType argIdent) -> (compileType argType, compileVariableIdent argIdent)) args

     makeBody :: StatementM ()
     makeBody =
       do label <- getNextLabelE
          emitInstruction $ LLVM.ILabel label
          mapM_ saveArgument args
          mapM_ compileStmt stmts
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

compileFlowBlock :: AbsLatte.Stmt -> LLVM.Label -> ExprM ()
compileFlowBlock stmt nextBlock =
  do statementInExpr $ compileStmt stmt
     emitInstruction (LLVM.IBr nextBlock)

compileAssign :: AbsLatte.Expr
                -> LLVM.Type
                -> LLVM.Register
                -> ExprM ()
compileAssign expr ptrType ptr =
  do (value, type_) <- compileExpr expr
     lift3 $ checkType type_ ptrType "wrong type at assignment"
     emitInstruction $ LLVM.IStore type_ value type_ ptr

compileStmt :: AbsLatte.Stmt -> StatementM ()
compileStmt AbsLatte.VRet =
  emitInstruction LLVM.IRetVoid

compileStmt (AbsLatte.Incr ident) = compileIncrDecrHelper ident LLVM.OAdd
compileStmt (AbsLatte.Decr ident) = compileIncrDecrHelper ident LLVM.OSub

compileStmt AbsLatte.Empty = return ()

compileStmt (AbsLatte.SExp expr) =
   do (_, _) <- exprInStatement (compileExpr expr)
      return ()

compileStmt (AbsLatte.Ret expr)=
  do (value, type_) <- exprInStatement (compileExpr expr)
     emitInstruction $ LLVM.IRet type_ value

compileStmt (AbsLatte.Ass ident expr) =
  do valueMap <- readValueMapS
     (type_, ptr) <- lift3 $ lookupVariable (compileVariableIdent ident) valueMap (getPosition ident)
     exprInStatement $ compileAssign expr type_ ptr

compileStmt (AbsLatte.Decl type_ decls)=
   do lift3 $ checkNotVoid type_ "void variable declarations illegal"
      ptrs <- exprInStatement $ mapM go decls
      mapM_ (\ (decl, ptr) -> setVariableM (compileVariableIdent (getIdent decl)) llvmType ptr) (zip decls ptrs)

   where
      llvmType = compileType type_

      getIdent (AbsLatte.Init ident _) = ident
      getIdent (AbsLatte.NoInit ident) = ident

      storeValue (AbsLatte.Init _ expr) ptr =
        compileAssign expr llvmType ptr
      storeValue (AbsLatte.NoInit _) ptr =
        emitInstruction $ LLVM.IStore llvmType (defaultValue llvmType) llvmType ptr

      go declItem =
           do ptr <- getNextRegisterE
              emitInstruction $ LLVM.IAlloca llvmType ptr
              storeValue declItem ptr
              return ptr

compileStmt (AbsLatte.BStmt (AbsLatte.Block stmts)) =
  exprInStatement $ statementInExpr $ mapM_ compileStmt stmts -- TODO: refactor

compileStmt (AbsLatte.Cond expr stmt1) =
  do (cond, type_) <- exprInStatement $ compileExpr expr
     lift3 $ checkType type_ LLVM.Ti1 "if condition"
     ifTrueBlock <- getNextLabelE
     contBlock <- getNextLabelE
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock contBlock
     emitInstruction $ LLVM.ILabel ifTrueBlock
     exprInStatement $ compileFlowBlock stmt1 contBlock
     emitInstruction $ LLVM.ILabel contBlock

compileStmt (AbsLatte.CondElse expr stmt1 stmt2) =
  do (cond, type_) <- exprInStatement $ compileExpr expr
     lift3 $ checkType type_ LLVM.Ti1 "if-else condition"
     ifTrueBlock <- getNextLabelE
     ifElseBlock <- getNextLabelE
     contBlock <- getNextLabelE
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock ifElseBlock
     emitInstruction $ LLVM.ILabel ifTrueBlock
     exprInStatement $ compileFlowBlock stmt1 contBlock
     emitInstruction $ LLVM.ILabel ifElseBlock
     exprInStatement $ compileFlowBlock stmt2 contBlock
     emitInstruction $ LLVM.ILabel contBlock

compileStmt (AbsLatte.While expr stmt) =
  do condBlock <- getNextLabelE
     bodyBlock <- getNextLabelE
     contBlock <- getNextLabelE
     emitInstruction $ LLVM.IBr condBlock
     emitInstruction $ LLVM.ILabel bodyBlock
     exprInStatement $ compileFlowBlock stmt condBlock
     emitInstruction $ LLVM.ILabel condBlock
     (cond, type_) <- exprInStatement $ compileExpr expr
     lift3 $ checkType type_ LLVM.Ti1 "while loop condition"
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond bodyBlock contBlock
     emitInstruction $ LLVM.ILabel contBlock

compileIncrDecrHelper :: AbsLatte.CIdent -> LLVM.ArithmOp -> StatementM ()
compileIncrDecrHelper ident arithmOp =
  do valueMap <- readValueMapS
     (type_, ptr) <- lift3 $ lookupVariable (compileVariableIdent ident) valueMap (getPosition ident)
     lift3 $ checkType LLVM.Ti32 type_ "incrementation (must be int)"
     operand <- getNextRegisterE
     result <- getNextRegisterE
     emitInstruction $ LLVM.ILoad LLVM.Ti32 LLVM.Ti32 ptr operand
     emitInstruction $ LLVM.IArithm LLVM.Ti32 (LLVM.VRegister operand) (LLVM.VConst 1) arithmOp result
     emitInstruction $ LLVM.IStore LLVM.Ti32 (LLVM.VRegister result) LLVM.Ti32 ptr

compileExpr :: AbsLatte.Expr -> ExprM (LLVM.Value, LLVM.Type)
compileExpr (AbsLatte.EVar ident) =
  do valueMap <- readValueMap
     (type_, ptr) <- lift3 $ lookupVariable (compileVariableIdent ident) valueMap (getPosition ident)
     reg <- getNextRegisterE
     emitInstruction $ LLVM.ILoad type_ type_ ptr reg
     return (LLVM.VRegister reg, type_)

compileExpr (AbsLatte.EApp ident args) =
  do compiledArgs <- mapM compileExpr args
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

compileExpr (AbsLatte.ELitInt int) = return (LLVM.VConst int, LLVM.Ti32)
compileExpr AbsLatte.ELitTrue = return (LLVM.VTrue, LLVM.Ti1)
compileExpr AbsLatte.ELitFalse = return (LLVM.VFalse, LLVM.Ti1)

compileExpr (AbsLatte.EString string) =
  do constName <- getNextConstE
     emitGlobal $ LLVM.Constant (length string + 1) constName string
     return (LLVM.VGetElementPtr (length string + 1) constName, LLVM.Ti8Ptr)

compileExpr (AbsLatte.Neg expr) =
  do (value, type_) <- compileExpr expr
     lift3 $ checkType type_ LLVM.Ti32 "negation (integer required)"
     res <- getNextRegisterE
     emitInstruction $ LLVM.IArithm LLVM.Ti32 (LLVM.VConst 0) value LLVM.OSub res
     return (LLVM.VRegister res, LLVM.Ti32)

compileExpr (AbsLatte.Not expr) =
  do (value, type_) <- compileExpr expr
     lift3 $ checkType type_ LLVM.Ti1 "boolean not (boolean required)"
     res <- getNextRegisterE
     emitInstruction $ LLVM.IIcmp LLVM.RelOpEQ LLVM.Ti1 LLVM.VFalse value res
     return (LLVM.VRegister res, LLVM.Ti1)

compileExpr (AbsLatte.EAnd expr1 expr2) =
  compileBooleanOpHelper LLVM.VFalse expr1 expr2
compileExpr (AbsLatte.EOr expr1 expr2) =
  compileBooleanOpHelper LLVM.VTrue expr1 expr2

compileExpr (AbsLatte.EAdd exp1 addOp exp2) =
  compileArithm exp1 (compileAddOperator addOp) exp2
compileExpr (AbsLatte.EMul exp1 mulOp exp2) =
  compileArithm exp1 (compileMulOperator mulOp) exp2
compileExpr (AbsLatte.ERel exp1 relOp exp2) =
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
              ] ++
              [ (LLVM.Ti1, op, LLVM.Ti1, LLVM.Ti1,
                  LLVM.IIcmp relOp LLVM.Ti1)
                  | (op, relOp) <- [ (Equal, LLVM.RelOpEQ)
                                   , (NotEqual, LLVM.RelOpNE)
                                   ]
              ] ++
              [ (LLVM.Ti8Ptr, Equal, LLVM.Ti8Ptr, LLVM.Ti1,
                  \ v1 v2 reg -> LLVM.ICall LLVM.Ti1 "streq"
                    [(LLVM.Ti8Ptr, v1), (LLVM.Ti8Ptr, v2)]
                    (Just reg))
              , (LLVM.Ti8Ptr, NotEqual, LLVM.Ti8Ptr, LLVM.Ti1,
                  \ v1 v2 reg -> LLVM.ICall LLVM.Ti1 "strne"
                    [(LLVM.Ti8Ptr, v1), (LLVM.Ti8Ptr, v2)]
                    (Just reg))
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
  do (val1, type1) <- compileExpr exp1
     (val2, type2) <- compileExpr exp2
     (retType, instr) <- lift3 $ getInstr type1 type2
     reg <- getNextRegisterE
     emitInstruction $ instr val1 val2 reg
     return (LLVM.VRegister reg, retType)

  where
    getInstr type1 type2 =
      case filter (\ (a, b, c, _, _) -> (a, b, c) == (type1, op, type2)) operations of
        [] -> CompilerErr.raiseCETypeError "incorrect binary operation"
        (_, _, _, retType, instr) : _ -> return (retType, instr)

compileBooleanOpHelper :: LLVM.Value -> AbsLatte.Expr -> AbsLatte.Expr -> ExprM (LLVM.Value, LLVM.Type)
compileBooleanOpHelper skipValue expr1 expr2 =
  do (val1, type1) <- compileExpr expr1
     lift3 $ checkType type1 LLVM.Ti1 "first operand in && expression"
     expr2Block <- getNextLabelE
     andEndBlock <- getNextLabelE
     expr1Block <- getCurrentBlock
     emitInstruction $ jumpOrder (LLVM.IBrCond LLVM.Ti1 val1) expr2Block andEndBlock

     emitInstruction $ LLVM.ILabel expr2Block
     (val2, type2) <- compileExpr expr2
     lift3 $ checkType type2 LLVM.Ti1 "second operand in && expression"
     emitInstruction $ LLVM.IBr andEndBlock
     expr2Block' <- getCurrentBlock

     emitInstruction $ LLVM.ILabel andEndBlock
     res <- getNextRegisterE
     emitInstruction $ LLVM.IPhi LLVM.Ti1 [(skipValue, expr1Block), (val2, expr2Block')] res
     return (LLVM.VRegister res, LLVM.Ti1)
  where
    jumpOrder f a b | skipValue == LLVM.VFalse = f a b
                    | skipValue == LLVM.VTrue = f b a
                    | otherwise = error "unreachable"

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
