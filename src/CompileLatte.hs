{-# OPTIONS_GHC -Wall -Werror #-}

module CompileLatte (compileLatte, Position) where

import qualified Data.Map as M
import qualified AbsLatte
import qualified CompilerErr as CE
import qualified LatteCommon
import Control.Monad (foldM, when)
import Data.Tuple (swap)
import Data.Maybe (mapMaybe)

import qualified LLVM
import CompilerState

latteMain :: [String]
latteMain = [ "target triple = \"x86_64-apple-macosx10.13.0\""
            , "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1"
            ]

builtins :: [(AbsLatte.CIdent, LLVM.FunctionType)]
builtins = [ (AbsLatte.CIdent "printInt", LLVM.FunctionType [LLVM.Ti32] LLVM.Tvoid)
           , (AbsLatte.CIdent "printString", LLVM.FunctionType [LLVM.Ti8Ptr] LLVM.Tvoid)
           , (AbsLatte.CIdent "readInt", LLVM.FunctionType [] LLVM.Ti32)
           , (AbsLatte.CIdent "readString", LLVM.FunctionType [] LLVM.Ti8Ptr)
           , (AbsLatte.CIdent "error", LLVM.FunctionType [] LLVM.Tvoid)
           ]

hiddenBuiltins :: [(String, LLVM.FunctionType)]
hiddenBuiltins =
           [ (concatName, LLVM.FunctionType [LLVM.Ti8Ptr, LLVM.Ti8Ptr] LLVM.Ti8Ptr)
           , (streqName, LLVM.FunctionType [LLVM.Ti8Ptr, LLVM.Ti8Ptr] LLVM.Ti1)
           , (strneName, LLVM.FunctionType [LLVM.Ti8Ptr, LLVM.Ti8Ptr] LLVM.Ti1)
           ]

concatName :: String
concatName = "concat"
streqName :: String
streqName = "streq"
strneName :: String
strneName = "strne"

emptyStringConst :: LLVM.Constant
emptyStringConst = LLVM.Constant 1 "empty_string" ""

mainFunctionName :: AbsLatte.CIdent
mainFunctionName = AbsLatte.CIdent "main"
mainFunctionType :: LLVM.FunctionType
mainFunctionType = LLVM.FunctionType [] LLVM.Ti32

type Position = Maybe (Int, Int)

checkDuplicateIdents :: [(AbsLatte.CIdent, CE.Position)] -> Maybe (AbsLatte.CIdent, CE.Position)
checkDuplicateIdents idents = case
  M.toList (M.filter (\l -> length l >= 2) (M.fromListWith (++) (map (\ (a, b) -> (a, [b])) idents))) of
    [] -> Nothing
    (str, pos : _) : _ -> Just (str, pos)
    ((_, []):_) -> error "unreachable"

checkDuplicateFnDefs :: [AbsLatte.TopDef Position] -> CE.CompilerErrorM ()
checkDuplicateFnDefs topDefs = case checkDuplicateIdents positions of
    Nothing -> return ()
    Just (ident, pos) -> CE.raise
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

checkMainSignature :: Signatures -> Position -> CE.CompilerErrorM ()
checkMainSignature signatures mainPosition =
  case getMaybeType mainFunctionName signatures of
    Nothing -> CE.raise CE.CEMissingMainFunction
    Just type_ | type_ /= mainFunctionType ->
      CE.raise $ CE.CEIncorrectMainFunctionType (compilePosition mainPosition)
    _ -> return ()

collectSignatures :: [AbsLatte.TopDef Position] -> Signatures
collectSignatures topDefs = Signatures $ M.fromList pairs
  where
    pairs = builtins ++
            map getSignaturePair topDefs
    getSignaturePair (AbsLatte.FnDef _ retType ident args _) =
      (ident,
       LLVM.FunctionType
       (map (\ (AbsLatte.Arg _ type_ _) -> compileType type_) args)
       (compileType retType))

compileType :: AbsLatte.Type Position -> LLVM.Type
compileType (AbsLatte.Int _) = LLVM.Ti32
compileType (AbsLatte.Void _) = LLVM.Tvoid
compileType (AbsLatte.Bool _) = LLVM.Ti1
compileType (AbsLatte.Str _) = LLVM.Ti8Ptr
compileType AbsLatte.Fun {} = error "unreachable"

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

compileLatte :: AbsLatte.Program Position -> CE.CompilerErrorM String
compileLatte (AbsLatte.Program _ topDefs) =
   do checkDuplicateFnDefs topDefs
      let signatures = collectSignatures topDefs
      checkMainSignature signatures (getMainPosition topDefs)
      (funcLines, globalsLines, _) <- foldM (go signatures) ([], [emptyStringConst], initConstCounter) topDefs
      let allLines = concatMap LLVM.showFunc funcLines
      return $ unlines $
        latteMain ++
        map LLVM.showGlobal globalsLines ++
        allLines ++
        map (uncurry LLVM.showGlobalDecl) (map builtinToLLVM builtins ++ hiddenBuiltins)

   where go signatures (functions, constants, constCounter0) topDef =
           do (newFunc, newConstants, constCounter1) <- compileFunc signatures topDef constCounter0
              return (functions ++ [newFunc], constants ++ newConstants, constCounter1)
         builtinToLLVM (ident, type_) = (compileFuncIdent ident, type_)

compileFunc :: Signatures -> AbsLatte.TopDef Position -> ConstCounter
      -> CE.CompilerErrorM (LLVM.Function, [LLVM.Constant], ConstCounter)
compileFunc signatures (AbsLatte.FnDef _ type_ ident args (AbsLatte.Block _ stmts)) constCounter0 =
   do
      mapM_ (\ (num, AbsLatte.Arg position argType _) ->
            checkNotVoid argType CE.CEVoidFunctionArgument { CE.cePosition = compilePosition position
                                                           , CE.ceArgumentNumber = num })
            (zip [(1 :: Int)..] args)
      (_, instrs, globals, _, _, _, constCounter1) <- runStatementM signatures (LLVM.Label 0) initNewScopeVars initValueMap initNextRegister constCounter0 makeBody
      return (LLVM.Function (compileType type_) (compileFuncIdent ident) llvmArgs instrs, globals, constCounter1)
   where
     llvmArgs :: [(LLVM.Type, String)]
     llvmArgs = map (\ (AbsLatte.Arg _ argType argIdent) -> (compileType argType, compileVariableIdent argIdent)) args

     makeBody :: StatementM ()
     makeBody =
       do label <- getNextLabelE
          emitInstruction $ LLVM.ILabel label
          mapM_ saveArgument args
          mapM_ compileStmt stmts
          emitInstruction nullRet

     saveArgument :: AbsLatte.Arg Position -> StatementM ()
     saveArgument (AbsLatte.Arg pos argType argIdent) =
       do ptr <- getNextRegisterE
          let llvmType = compileType argType
          emitInstruction $ LLVM.IAlloca llvmType ptr
          emitInstruction $ LLVM.IStore llvmType
              (LLVM.VRegister $ LLVM.RArgument (compileVariableIdent argIdent))
              llvmType ptr
          setVariableM (compilePosition pos) argIdent llvmType ptr

     nullRet | lType == LLVM.Tvoid = LLVM.IRetVoid
             | otherwise = LLVM.IRet lType (defaultValue lType)
     lType = compileType type_

defaultValue :: LLVM.Type -> LLVM.Value
defaultValue LLVM.Ti1  = LLVM.VFalse
defaultValue LLVM.Ti32 = LLVM.VConst 0
defaultValue LLVM.Tvoid = error "unreachable"
defaultValue LLVM.Ti8Ptr = LLVM.VGetElementPtr 1 "empty_string"

compileFlowBlock :: AbsLatte.Stmt Position -> LLVM.Label -> ExprM ()
compileFlowBlock stmt nextBlock =
  do statementInExpr $ compileStmt stmt
     emitInstruction (LLVM.IBr nextBlock)

compileAssign :: Position
                -> AbsLatte.Expr Position
                -> LLVM.Type
                -> LLVM.Register
                -> ExprM ()
compileAssign position expr ptrType ptr =
  do (value, type_) <- compileExpr expr
     lift3 $ checkType position ptrType type_ "assignment"
     emitInstruction $ LLVM.IStore type_ value type_ ptr

compileStmt :: AbsLatte.Stmt Position -> StatementM ()
compileStmt (AbsLatte.VRet _) =
  emitInstruction LLVM.IRetVoid

compileStmt (AbsLatte.Incr pos ident) = compileIncrDecrHelper pos ident LLVM.OAdd
compileStmt (AbsLatte.Decr pos ident) = compileIncrDecrHelper pos ident LLVM.OSub

compileStmt (AbsLatte.Empty _) = return ()

compileStmt (AbsLatte.SExp _ expr) =
   do (_, _) <- exprInStatement (compileExpr expr)
      return ()

compileStmt (AbsLatte.Ret _ expr)=
  do (value, type_) <- exprInStatement (compileExpr expr)
     emitInstruction $ LLVM.IRet type_ value

compileStmt (AbsLatte.Ass pos ident expr) =
  do valueMap <- readValueMapS
     (type_, ptr) <- lift3 $ lookupVariable ident valueMap (compilePosition pos)
     exprInStatement $ compileAssign pos expr type_ ptr

compileStmt (AbsLatte.Decl position type_ decls) =
   do lift3 $ checkNotVoid type_ (CE.CEVoidDeclaration (compilePosition position))
      ptrs <- exprInStatement $ mapM go decls
      mapM_ (\ (decl, ptr) -> setVariableM
                  (compilePosition (getDeclPos decl))
                  (getIdent decl)
                   llvmType ptr)
              (zip decls ptrs)

   where
      llvmType = compileType type_

      getIdent (AbsLatte.Init _ ident _) = ident
      getIdent (AbsLatte.NoInit _ ident) = ident

      getDeclPos (AbsLatte.Init pos _ _) = pos
      getDeclPos (AbsLatte.NoInit pos _) = pos

      storeValue (AbsLatte.Init itemPos _ expr) ptr =
        compileAssign itemPos expr llvmType ptr
      storeValue (AbsLatte.NoInit _ _) ptr =
        emitInstruction $ LLVM.IStore llvmType (defaultValue llvmType) llvmType ptr

      go declItem =
           do ptr <- getNextRegisterE
              emitInstruction $ LLVM.IAlloca llvmType ptr
              storeValue declItem ptr
              return ptr

compileStmt (AbsLatte.BStmt _ (AbsLatte.Block _ stmts)) =
  exprInStatement $ statementInExpr $ mapM_ compileStmt stmts -- TODO: refactor

compileStmt (AbsLatte.Cond position expr stmt1) =
  do (cond, type_) <- exprInStatement $ compileExpr expr
     lift3 $ checkType position LLVM.Ti1 type_ "if condition"
     ifTrueBlock <- getNextLabelE
     contBlock <- getNextLabelE
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock contBlock
     emitInstruction $ LLVM.ILabel ifTrueBlock
     exprInStatement $ compileFlowBlock stmt1 contBlock
     emitInstruction $ LLVM.ILabel contBlock

compileStmt (AbsLatte.CondElse pos expr stmt1 stmt2) =
  do (cond, type_) <- exprInStatement $ compileExpr expr
     lift3 $ checkType pos LLVM.Ti1 type_ "if-else condition"
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
     lift3 $ checkType position LLVM.Ti1 type_ "while loop condition"
     emitInstruction $ LLVM.IBrCond LLVM.Ti1 cond bodyBlock contBlock
     emitInstruction $ LLVM.ILabel contBlock

compileIncrDecrHelper :: Position -> AbsLatte.CIdent -> LLVM.ArithmOp -> StatementM ()
compileIncrDecrHelper pos ident arithmOp =
  do valueMap <- readValueMapS
     (type_, ptr) <- lift3 $ lookupVariable ident valueMap (compilePosition pos)
     lift3 $ checkType pos LLVM.Ti32 type_ "incrementation"
     operand <- getNextRegisterE
     result <- getNextRegisterE
     emitInstruction $ LLVM.ILoad LLVM.Ti32 LLVM.Ti32 ptr operand
     emitInstruction $ LLVM.IArithm LLVM.Ti32 (LLVM.VRegister operand) (LLVM.VConst 1) arithmOp result
     emitInstruction $ LLVM.IStore LLVM.Ti32 (LLVM.VRegister result) LLVM.Ti32 ptr

compileExpr :: AbsLatte.Expr Position -> ExprM (LLVM.Value, LLVM.Type)
compileExpr (AbsLatte.EVar pos ident) =
  do valueMap <- readValueMap
     (type_, ptr) <- lift3 $ lookupVariable ident valueMap (compilePosition pos)
     reg <- getNextRegisterE
     emitInstruction $ LLVM.ILoad type_ type_ ptr reg
     return (LLVM.VRegister reg, type_)

compileExpr (AbsLatte.EApp pos ident args) =
  do compiledArgs <- mapM compileExpr args
     signatures <- readSignatures
     (LLVM.FunctionType expectedArgTypes retType) <- lift3 $ getType ident signatures (compilePosition pos)
     let argTypes = map snd compiledArgs
     mapM_ (\ (num, actType, expType) ->
              (lift3 $ checkType pos expType actType ("argument " ++ show num ++ " in function call")))
           (zip3 [(1::Int)..] argTypes expectedArgTypes)
     when (length argTypes /= length expectedArgTypes) (
        lift3 $ CE.raise $ CE.CEWrongNumberOfFunctionArguments
          (compilePosition pos) (length expectedArgTypes) (length argTypes) ident)
     if retType == LLVM.Tvoid then do
       emitInstruction $ LLVM.ICall retType (compileFuncIdent ident) (map swap compiledArgs) Nothing
       return (LLVM.VConst 0, retType)
     else do
       register <- getNextRegisterE
       emitInstruction $ LLVM.ICall retType (compileFuncIdent ident) (map swap compiledArgs) (Just register)
       return (LLVM.VRegister register, retType)

compileExpr (AbsLatte.ELitInt _ int) = return (LLVM.VConst int, LLVM.Ti32)
compileExpr (AbsLatte.ELitTrue _) = return (LLVM.VTrue, LLVM.Ti1)
compileExpr (AbsLatte.ELitFalse _) = return (LLVM.VFalse, LLVM.Ti1)

compileExpr (AbsLatte.EString _ string) =
  do constName <- getNextConstE
     emitGlobal $ LLVM.Constant (length llvmString + 1) constName llvmString
     return (LLVM.VGetElementPtr (length llvmString + 1) constName, LLVM.Ti8Ptr)

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
     lift3 $ checkType pos LLVM.Ti32 type_ "negation"
     res <- getNextRegisterE
     emitInstruction $ LLVM.IArithm LLVM.Ti32 (LLVM.VConst 0) value LLVM.OSub res
     return (LLVM.VRegister res, LLVM.Ti32)

compileExpr (AbsLatte.Not pos expr) =
  do (value, type_) <- compileExpr expr
     lift3 $ checkType pos LLVM.Ti1 type_ "boolean not"
     res <- getNextRegisterE
     emitInstruction $ LLVM.IIcmp LLVM.RelOpEQ LLVM.Ti1 LLVM.VFalse value res
     return (LLVM.VRegister res, LLVM.Ti1)

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

operations :: [ (LLVM.Type, LatteCommon.Operation, LLVM.Type, LLVM.Type, LLVM.Value -> LLVM.Value -> LLVM.Register -> LLVM.Instr)]
operations = [ (LLVM.Ti32, op, LLVM.Ti32, LLVM.Ti32,
                  \ v1 v2 reg -> LLVM.IArithm LLVM.Ti32 v1 v2 llvmOp reg
                  )   | (op, llvmOp) <- [ (LatteCommon.Add, LLVM.OAdd)
                                        , (LatteCommon.Sub, LLVM.OSub)
                                        , (LatteCommon.Mul, LLVM.OMul)
                                        , (LatteCommon.Div, LLVM.OSDiv)
                                        , (LatteCommon.Mod, LLVM.OSRem)
                                   ]] ++
              [ (LLVM.Ti8Ptr, LatteCommon.Add, LLVM.Ti8Ptr, LLVM.Ti8Ptr,
                  \ v1 v2 reg -> LLVM.ICall LLVM.Ti8Ptr concatName
                           [(LLVM.Ti8Ptr, v1), (LLVM.Ti8Ptr, v2)]
                           (Just reg)) ] ++
              [ (LLVM.Ti32, op, LLVM.Ti32, LLVM.Ti1,
                  LLVM.IIcmp relOp LLVM.Ti32)
                  | (op, relOp) <- [ (LatteCommon.LessThan, LLVM.RelOpSLT)
                                   , (LatteCommon.GreaterThan, LLVM.RelOpSGT)
                                   , (LatteCommon.LessEqual, LLVM.RelOpSLE)
                                   , (LatteCommon.GreaterEqual, LLVM.RelOpSGE)
                                   , (LatteCommon.Equal, LLVM.RelOpEQ)
                                   , (LatteCommon.NotEqual, LLVM.RelOpNE)
                                   ]
              ] ++
              [ (LLVM.Ti1, op, LLVM.Ti1, LLVM.Ti1,
                  LLVM.IIcmp relOp LLVM.Ti1)
                  | (op, relOp) <- [ (LatteCommon.Equal, LLVM.RelOpEQ)
                                   , (LatteCommon.NotEqual, LLVM.RelOpNE)
                                   ]
              ] ++
              [ (LLVM.Ti8Ptr, LatteCommon.Equal, LLVM.Ti8Ptr, LLVM.Ti1,
                  \ v1 v2 reg -> LLVM.ICall LLVM.Ti1 streqName
                    [(LLVM.Ti8Ptr, v1), (LLVM.Ti8Ptr, v2)]
                    (Just reg))
              , (LLVM.Ti8Ptr, LatteCommon.NotEqual, LLVM.Ti8Ptr, LLVM.Ti1,
                  \ v1 v2 reg -> LLVM.ICall LLVM.Ti1 strneName
                    [(LLVM.Ti8Ptr, v1), (LLVM.Ti8Ptr, v2)]
                    (Just reg))
              ]

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
                -> AbsLatte.Expr Position -> ExprM (LLVM.Value, LLVM.Type)
compileArithm position exp1 op exp2 =
  do (val1, type1) <- compileExpr exp1
     (val2, type2) <- compileExpr exp2
     (retType, instr) <- lift3 $ getInstr type1 type2
     reg <- getNextRegisterE
     emitInstruction $ instr val1 val2 reg
     return (LLVM.VRegister reg, retType)

  where
    getInstr type1 type2 =
      case filter (\ (a, b, c, _, _) -> (a, b, c) == (type1, op, type2)) operations of
        [] -> CE.raise $ CE.CEInvalidBinaryOp (compilePosition position) type1 op type2
        (_, _, _, retType, instr) : _ -> return (retType, instr)

compileBooleanOpHelper :: Position
                          -> LLVM.Value
                          -> AbsLatte.Expr Position
                          -> AbsLatte.Expr Position
                          -> ExprM (LLVM.Value, LLVM.Type)
compileBooleanOpHelper position skipValue expr1 expr2 =
  do (val1, type1) <- compileExpr expr1
     lift3 $ checkType position LLVM.Ti1 type1 "first operand in && expression"
     expr2Block <- getNextLabelE
     andEndBlock <- getNextLabelE
     expr1Block <- getCurrentBlock
     emitInstruction $ jumpOrder (LLVM.IBrCond LLVM.Ti1 val1) expr2Block andEndBlock

     emitInstruction $ LLVM.ILabel expr2Block
     (val2, type2) <- compileExpr expr2
     lift3 $ checkType position LLVM.Ti1 type2 "second operand in && expression"
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

checkType :: Position -> LLVM.Type -> LLVM.Type -> String -> CE.CompilerErrorM ()
checkType pos expType actType description | expType == actType = return ()
                                      | otherwise = CE.raise $
                                          CE.CETypeError (compilePosition pos) expType actType description

checkNotVoid :: AbsLatte.Type Position -> CE.CompilerError -> CE.CompilerErrorM ()
checkNotVoid (AbsLatte.Void _) ce = CE.raise ce
checkNotVoid _ _ = return ()

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
