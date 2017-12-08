module CompileLatte where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified AbsLatte
import CompilerErr (CompilerErrorM, raiseCEUndefinedVariable, raiseCEUndefinedFunction)
import qualified CompilerErr
import Control.Monad (foldM, when)

import qualified LLVM

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
            ]

mainFunctionName = "main"
mainFunctionType = LLVM.FunctionType [] LLVM.Ti32

newtype ValueMap = ValueMap (M.Map String (LLVM.Type, LLVM.Register))
newtype Signatures = Signatures (M.Map String LLVM.FunctionType)
newtype NextRegister = NextRegister Int

getPosition :: AbsLatte.CIdent -> CompilerErr.Position
getPosition (AbsLatte.CIdent ((row, col), _)) = CompilerErr.Position { CompilerErr.row = row
                                                   , CompilerErr.column = col }

initNextRegister :: NextRegister
initNextRegister = NextRegister 0

setVariable :: String -> LLVM.Type -> LLVM.Register -> ValueMap -> ValueMap
setVariable name type_ value (ValueMap valueMap) =
    ValueMap (M.insert name (type_, value) valueMap)

lookupVariable :: String -> ValueMap -> CompilerErr.Position -> CompilerErrorM (LLVM.Type, LLVM.Register)
lookupVariable name (ValueMap valueMap) position =
    maybe (raiseCEUndefinedVariable name position) return (M.lookup name valueMap)

initValueMap :: ValueMap
initValueMap = ValueMap M.empty

checkDuplicateIdents :: [(String, CompilerErr.Position)] -> Maybe (String, CompilerErr.Position)
checkDuplicateIdents idents = case
  M.toList (M.filter (\l -> length l >= 2) (M.fromListWith (++) (map (\ (a, b) -> (a, [b])) idents))) of
    [] -> Nothing
    (str, pos : _) : _ -> Just (str, pos)

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

compileLatte :: AbsLatte.Program -> CompilerErrorM String
compileLatte (AbsLatte.Program topDefs) =
   do checkDuplicateFnDefs topDefs
      let signatures = collectSignatures topDefs
      checkMainSignature signatures
      (funcLines, globalsLines, _) <- foldM (go signatures) ([], [], initConstCounter) topDefs
      let allLines = concatMap LLVM.showFunc funcLines
      return $ unlines $ latteMain ++ (map LLVM.showGlobal globalsLines) ++ allLines

   where go signatures (functions, constants, constCounter0) topDef =
           do (newFunc, newConstants, constCounter1) <- compileFunc signatures topDef constCounter0
              return (functions ++ [newFunc], constants ++ newConstants, constCounter1)

compileFunc :: Signatures -> AbsLatte.TopDef -> ConstCounter -> CompilerErrorM (LLVM.Function, [LLVM.Constant], ConstCounter)
compileFunc signatures (AbsLatte.FnDef type_ ident args block) constCounter0 =
   do
      mapM_ (\ (num, AbsLatte.Arg argType _) ->
            checkNotVoid argType "function argument must not be void")
            (zip [1..] args)
      let (argInstrs, argGlobals, valueMap2, nextReg2, constCounter3) =
            foldl (\ (instrs, globals, valueMap0, nextReg0, constCounter1) arg ->
                            let (newInstrs, newGlobals, valueMap1, nextReg1, constCounter2) = saveArgument arg valueMap0 nextReg0 constCounter1 in
                            (instrs ++ newInstrs, globals ++ newGlobals, valueMap1, nextReg1, constCounter2))
                         ([], [], initValueMap, initNextRegister, constCounter0) args

      (blockLines, blockGlobals, _, constCounter4) <- compileBlock signatures block valueMap2 nextReg2 constCounter3
      return (LLVM.Function (compileType type_) (compileFuncIdent ident) llvmArgs (argInstrs ++ blockLines ++ [nullRet]), argGlobals ++ blockGlobals, constCounter4)
   where

     llvmArgs :: [(LLVM.Type, String)]
     llvmArgs = map (\ (AbsLatte.Arg type_ ident) -> (compileType type_, compileVariableIdent ident)) args

     saveArgument :: AbsLatte.Arg -> ValueMap -> NextRegister -> ConstCounter -> ([LLVM.Instr], [LLVM.Constant], ValueMap, NextRegister, ConstCounter)
     saveArgument (AbsLatte.Arg type_ ident) valueMap nextReg constCounter1 =
       do let (ptr, nextReg1) = getNextRegister nextReg
          let llvmType = compileType type_
          let alloc = LLVM.IAlloca llvmType ptr
          let valueMap1 = setVariable (compileVariableIdent ident) llvmType ptr valueMap
          ([alloc, LLVM.IStore llvmType (LLVM.VRegister $ LLVM.RArgument (compileVariableIdent ident)) llvmType ptr], [], valueMap1, nextReg1, constCounter1)

     nullRet | lType == LLVM.Tvoid = LLVM.IRetVoid
             | otherwise = LLVM.IRet lType (defaultValue lType)
     lType = compileType type_

defaultValue :: LLVM.Type -> LLVM.Value
defaultValue LLVM.Ti1  = LLVM.VFalse
defaultValue LLVM.Ti32 = LLVM.VConst 0

compileBlock :: Signatures -> AbsLatte.Block -> ValueMap -> NextRegister -> ConstCounter -> CompilerErrorM ([LLVM.Instr], [LLVM.Constant], NextRegister, ConstCounter)
compileBlock signatures (AbsLatte.Block stmts) valueMap0 nextRegister0 constCounter0 =
  do (instrs, globals, _, reg, constCounter3) <- foldM go ([], [], valueMap0, nextRegister0, constCounter0) stmts
     return (instrs, globals, reg, constCounter3)
  where go (sLines, globals, valueMap, nextRegister, constCounter1) statement =
          do (newLines, newGlobals, newValueMap, newNextRegister, constCounter2) <- compileStmt signatures statement valueMap nextRegister constCounter1
             return (sLines ++ newLines, globals ++ newGlobals, newValueMap, newNextRegister, constCounter2)

compileFlowBlock :: Signatures -> AbsLatte.Stmt -> ValueMap -> NextRegister -> LLVM.Label -> ConstCounter -> CompilerErrorM ([LLVM.Instr], [LLVM.Constant], NextRegister, ConstCounter)
compileFlowBlock signatures stmt valueMap0 nextReg0 nextBlock constCounter0 =
  do (stmts, globals, valueMap1, nextReg1, constCounter1) <- compileStmt signatures stmt valueMap0 nextReg0 constCounter0
     return (stmts ++ [LLVM.IBr nextBlock], globals, nextReg1, constCounter1)

compileAssign signatures expr valueMap nextReg ptrType ptr constCounter0 =
  do (value, type_, stmts, globals, newNextReg, constCounter1) <- compileExpr signatures expr valueMap nextReg constCounter0
     checkType type_ ptrType "wrong type at assignment"
     return (stmts ++ [LLVM.IStore type_ value type_ ptr], globals, newNextReg, constCounter1)

compileStmt :: Signatures -> AbsLatte.Stmt -> ValueMap -> NextRegister -> ConstCounter -> CompilerErrorM ([LLVM.Instr], [LLVM.Constant], ValueMap, NextRegister, ConstCounter)
compileStmt signatures (AbsLatte.SExp expr) valueMap nextReg constCounter0 =
   do (_, _, stmts, globals, newNextReg, constCounter1) <- compileExpr signatures expr valueMap nextReg constCounter0
      return (stmts, globals, valueMap, newNextReg, constCounter1)

compileStmt signatures (AbsLatte.Ass ident expr) valueMap nextReg constCounter0 =
   do (type_, ptr) <- lookupVariable (compileVariableIdent ident) valueMap (getPosition ident)
      (stmts, globals, nextReg1, constCounter1) <- compileAssign signatures expr valueMap nextReg type_ ptr constCounter0
      return (stmts, globals, valueMap, nextReg1, constCounter1)

compileStmt signatures (AbsLatte.Decl type_ decls) valueMap nextReg constCounter0 =
  do checkNotVoid type_ "void variable declarations illegal"
     foldM go ([], [], valueMap, nextReg, constCounter0) decls

  where
     llvmType = compileType type_
     go (stmts0, globals0, valueMap1, nextReg1, constCounter1) (AbsLatte.Init ident expr) =
          do let (ptr, nextReg2) = getNextRegister nextReg1
             let instr = [LLVM.IAlloca llvmType ptr]
             (stmts, globals, nextReg3, constCounter2) <- compileAssign signatures expr valueMap nextReg2 llvmType ptr constCounter1
             let valueMap2 = setVariable (compileVariableIdent ident) llvmType ptr valueMap1
             return (stmts0 ++ instr ++ stmts, globals0 ++ globals, valueMap2, nextReg3, constCounter1)
     go (stmts0, globals0, valueMap1, nextReg1, constCounter1) (AbsLatte.NoInit ident) =
          do let (ptr, nextReg2) = getNextRegister nextReg1
             let instr = [LLVM.IAlloca llvmType ptr]
             let stmts = [LLVM.IStore llvmType (defaultValue llvmType) llvmType ptr]
             let valueMap2 = setVariable (compileVariableIdent ident) llvmType ptr valueMap1
             return (stmts0 ++ instr ++ stmts, globals0, valueMap2, nextReg2, constCounter1)

compileStmt signatures (AbsLatte.Ret expr) valueMap nextReg constCounter0 =
  do (value, type_, stmts, globals, newNextReg, constCounter1) <- compileExpr signatures expr valueMap nextReg constCounter0
     return (stmts ++ [LLVM.IRet type_ value], globals, valueMap, newNextReg, constCounter1)

compileStmt signatures AbsLatte.VRet valueMap nextReg constCounter0 =
  return ([LLVM.IRetVoid], [], valueMap, nextReg, constCounter0)

compileStmt signatures (AbsLatte.Cond expr stmt1) valueMap0 nextReg0 constCounter0 =
  do (cond, type_, condStmts, condGlobals, nextReg1, constCounter1) <- compileExpr signatures expr valueMap0 nextReg0 constCounter0
     checkType type_ LLVM.Ti1 "if condition"
     let (ifTrueBlock, nextReg2) = getNextLabel nextReg1
     let (contBlock, nextReg3) = getNextLabel nextReg2
     let branch = LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock contBlock
     (ifBlockStmts, ifBlockGlobals, nextReg4, constCounter2) <- compileFlowBlock signatures stmt1 valueMap0 nextReg3 contBlock constCounter1
     return (condStmts ++ [branch, LLVM.ILabel ifTrueBlock] ++ ifBlockStmts ++ [LLVM.ILabel contBlock], condGlobals ++ ifBlockGlobals, valueMap0, nextReg4, constCounter1)

compileStmt signatures (AbsLatte.CondElse expr stmt1 stmt2) valueMap0 nextReg0 constCounter0 =
  do (cond, type_, condStmts, condGlobals, nextReg1, constCounter1) <- compileExpr signatures expr valueMap0 nextReg0 constCounter0
     checkType type_ LLVM.Ti1 "if-else condition"
     let (ifTrueBlock, nextReg2) = getNextLabel nextReg1
     let (ifElseBlock, nextReg3) = getNextLabel nextReg2
     let (contBlock, nextReg4) = getNextLabel nextReg3
     let branch = LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock ifElseBlock
     (ifTrueBlockStmts, ifTrueGlobals, nextReg5, constCounter2) <- compileFlowBlock signatures stmt1 valueMap0 nextReg4 contBlock constCounter1
     (ifElseBlockStmts, ifElseGlobals, nextReg6, constCounter3) <- compileFlowBlock signatures stmt2 valueMap0 nextReg5 contBlock constCounter2
     return (condStmts ++ [branch, LLVM.ILabel ifTrueBlock] ++ ifTrueBlockStmts ++ [LLVM.ILabel ifElseBlock] ++ ifElseBlockStmts ++ [LLVM.ILabel contBlock],
             condGlobals ++ ifTrueGlobals ++ ifElseGlobals,
             valueMap0, nextReg6, constCounter3)

compileStmt signatures (AbsLatte.While expr stmt) valueMap0 nextReg0 constCounter0 =
  do let (condBlock, nextReg1) = getNextLabel nextReg0
     let (bodyBlock, nextReg2) = getNextLabel nextReg1
     let (contBlock, nextReg3) = getNextLabel nextReg2
     (bodyBlockInstrs, bodyBlockGlobals, nextReg4, constCounter1) <- compileFlowBlock signatures stmt valueMap0 nextReg3 condBlock constCounter0
     (cond, type_, condInstrs, condGlobals, nextReg5, constCounter2) <- compileExpr signatures expr valueMap0 nextReg4 constCounter1
     checkType type_ LLVM.Ti1 "while loop condition"
     let condBlockInstrs = condInstrs ++ [LLVM.IBrCond LLVM.Ti1 cond bodyBlock contBlock]
     return ([LLVM.IBr condBlock, LLVM.ILabel bodyBlock] ++ bodyBlockInstrs ++ [LLVM.ILabel condBlock]  ++ condBlockInstrs ++ [LLVM.ILabel contBlock],
             bodyBlockGlobals ++ condGlobals,
             valueMap0, nextReg5, constCounter2)

compileStmt signatures (AbsLatte.BStmt block) valueMap0 nextReg0 constCounter0 =
  do (instrs, globals, nextReg1, constCounter1) <- compileBlock signatures block valueMap0 nextReg0 constCounter0
     return (instrs, globals, valueMap0, nextReg1, constCounter1)

compileExpr :: Signatures -> AbsLatte.Expr -> ValueMap -> NextRegister -> ConstCounter -> CompilerErrorM (LLVM.Value, LLVM.Type, [LLVM.Instr], [LLVM.Constant], NextRegister, ConstCounter)
compileExpr signatures (AbsLatte.EApp ident args) valueMap nextReg constCounter =
   do (sLines, globals, argVals, argTypes, nextReg1, constCounter1) <- foldM go ([], [], [], [], nextReg, constCounter) args
      (LLVM.FunctionType expectedArgTypes retType) <- getType (compileFuncIdent ident) signatures (getPosition ident)
      mapM_ (\ (num, actType, expType) -> (checkType actType expType ("argument " ++ show num ++ " in function call")))
            (zip3 [1..] argTypes expectedArgTypes)
      when (length argTypes /= length expectedArgTypes) (CompilerErr.raiseCETypeError "wrong number of function arguments")
      if retType == LLVM.Tvoid then
        return (LLVM.VConst 0, retType, sLines ++ [LLVM.ICall retType (compileFuncIdent ident) (zip argTypes argVals) Nothing], globals, nextReg1, constCounter1)
      else do
        let (register, nextReg2) = getNextRegister nextReg1
        return (LLVM.VRegister register, retType, sLines ++ [LLVM.ICall retType (compileFuncIdent ident) (zip argTypes argVals) (Just register)], globals, nextReg2, constCounter1)

   where
     go (sLines, gLines, argValues, argTypes, nextReg1, constCounter1) argExpr =
           do (val, type_, newLines, newGLines, nextReg2, constCounter2) <- compileExpr signatures argExpr valueMap nextReg1 constCounter1
              return (sLines ++ newLines, gLines ++ newGLines, argValues ++ [val], argTypes ++ [type_], nextReg2, constCounter2)

compileExpr _ (AbsLatte.ELitInt int) valueMap nextReg constCounter =
    return (LLVM.VConst int, LLVM.Ti32, [], [], nextReg, constCounter)

compileExpr _ (AbsLatte.EVar ident) valueMap nextReg constCounter =
  do (type_, ptr) <- lookupVariable (compileVariableIdent ident) valueMap (getPosition ident)
     let (reg, nextReg1) = getNextRegister nextReg
     return (LLVM.VRegister reg, type_, [LLVM.ILoad type_ type_ ptr reg], [], nextReg1, constCounter)

compileExpr signatures (AbsLatte.EAdd exp1 addOp exp2) valueMap nextReg0 constCounter =
  compileArithm signatures exp1 (compileAddOperator addOp) exp2 valueMap nextReg0 constCounter

compileExpr signatures (AbsLatte.EMul exp1 mulOp exp2) valueMap nextReg0 constCounter =
  compileArithm signatures exp1 (compileMulOperator mulOp) exp2 valueMap nextReg0 constCounter

compileExpr signatures AbsLatte.ELitTrue _ nextReg constCounter =
  return (LLVM.VTrue, LLVM.Ti1, [], [], nextReg, constCounter)

compileExpr signatures AbsLatte.ELitFalse _ nextReg constCounter =
  return (LLVM.VFalse, LLVM.Ti1, [], [], nextReg, constCounter)

compileExpr signatures (AbsLatte.ERel exp1 relOp exp2) valueMap nextReg0 constCounter0 =
  do (val1, type1, instr1, globals1, nextReg1, constCounter1) <- compileExpr signatures exp1 valueMap nextReg0 constCounter0
     (val2, type2, instr2, globals2, nextReg2, constCounter2) <- compileExpr signatures exp2 valueMap nextReg1 constCounter1
     checkType type1 LLVM.Ti32 "left operand to relational operator"
     checkType type2 LLVM.Ti32 "right operand to relational operator"
     let (reg, nextReg3) = getNextRegister nextReg2
     return (LLVM.VRegister reg, LLVM.Ti1, instr1 ++ instr2 ++ [LLVM.IIcmp (compileRelOp relOp) LLVM.Ti32 val1 val2 reg], globals1 ++ globals2, nextReg3, constCounter2)

compileExpr signatures (AbsLatte.EString string) valueMap nextReg0 constCounter =
  do let (constName, constCounter1) = getNextConst constCounter
     return (LLVM.VGetElementPtr (length string + 1) constName, LLVM.Ti8Ptr, [], [LLVM.Constant (length string + 1) constName string], nextReg0, constCounter1)

newtype ConstCounter = ConstCounter Int
initConstCounter :: ConstCounter
initConstCounter = ConstCounter 0
getNextConst :: ConstCounter -> (String, ConstCounter)
getNextConst (ConstCounter num) = ("string" ++ show num, ConstCounter (num + 1))

compileRelOp AbsLatte.GE  = LLVM.RelOpSGE
compileRelOp AbsLatte.GTH = LLVM.RelOpSGT
compileRelOp AbsLatte.LE  = LLVM.RelOpSLE
compileRelOp AbsLatte.LTH = LLVM.RelOpSLT
compileRelOp AbsLatte.EQU = LLVM.RelOpEQ
compileRelOp AbsLatte.NE = LLVM.RelOpNE

compileArithm signatures exp1 op exp2 valueMap nextReg0 constCounter0 =
  do (val1, type1, instr1, globals1, nextReg1, constCounter1) <- compileExpr signatures exp1 valueMap nextReg0 constCounter0
     (val2, type2, instr2, globals2, nextReg2, constCounter2) <- compileExpr signatures exp2 valueMap nextReg1 constCounter1
     checkType type1 LLVM.Ti32 "arithmetic operation"
     checkType type2 LLVM.Ti32 "arithmetic operation"
     let (reg, nextReg3) = getNextRegister nextReg2
     return (LLVM.VRegister reg, LLVM.Ti32, instr1 ++ instr2 ++ [LLVM.IArithm LLVM.Ti32 val1 val2 op reg], globals1 ++ globals2, nextReg3, constCounter2)

checkType :: LLVM.Type -> LLVM.Type -> String -> CompilerErrorM ()
checkType type1 type2 description | type1 == type2 = return ()
                                  | otherwise = CompilerErr.raiseCETypeError description

checkNotVoid :: AbsLatte.Type -> String -> CompilerErrorM ()
checkNotVoid type1 description | type1 == AbsLatte.Void = CompilerErr.raiseCETypeError description
                               | otherwise = return ()

compileAddOperator :: AbsLatte.AddOp -> LLVM.ArithmOp
compileAddOperator AbsLatte.Plus = LLVM.OAdd
compileAddOperator AbsLatte.Minus = LLVM.OSub

compileMulOperator :: AbsLatte.MulOp -> LLVM.ArithmOp
compileMulOperator AbsLatte.Div = LLVM.OSDiv
compileMulOperator AbsLatte.Mod = LLVM.OSRem
compileMulOperator AbsLatte.Times = LLVM.OMul

compileFuncIdent (AbsLatte.CIdent (_, str)) = str
compileVariableIdent (AbsLatte.CIdent (_, str)) = str

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
