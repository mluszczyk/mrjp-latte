module CompileLatte where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified AbsLatte
import CompilerErr (CompilerErrorM, raiseCEUndefinedVariable, raiseCEUndefinedFunction)
import Control.Monad (foldM)

import qualified LLVM

latteMain = [ "target triple = \"x86_64-apple-macosx10.13.0\""
            , "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1"
            , "define i32 @main() #0 {"
            , "  call i32 @lat_main()"
            , "  ret i32 0"
            , "}"
            , ""
            , "define void @printInt(i32 %num) #1 {"
            , "  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %num)"
            , "  ret void"
            , "}"
            , "declare i32 @printf(i8*, ...) #2"
            ]

newtype ValueMap = ValueMap (M.Map String LLVM.Register)
newtype Signatures = Signatures (M.Map String LLVM.FunctionType)
newtype NextRegister = NextRegister Int

initNextRegister :: NextRegister
initNextRegister = NextRegister 0

setVariable :: String -> LLVM.Register -> ValueMap -> ValueMap
setVariable name value (ValueMap valueMap) =
    ValueMap (M.insert name value valueMap)

lookupVariable :: String -> ValueMap -> CompilerErrorM LLVM.Register
lookupVariable name (ValueMap valueMap) =
    maybe (raiseCEUndefinedVariable name 0 0) return (M.lookup name valueMap)

initValueMap :: ValueMap
initValueMap = ValueMap M.empty

collectSignatures :: [AbsLatte.TopDef] -> Signatures
collectSignatures topDefs = Signatures $ M.fromList pairs
  where
    pairs = ("printInt", LLVM.FunctionType [LLVM.Ti32] LLVM.Tvoid) :
            map getSignaturePair topDefs
    getSignaturePair (AbsLatte.FnDef retType ident args _) =
      (compileFuncIdent ident,
       LLVM.FunctionType
       (map (\ (AbsLatte.Arg type_ _) -> compileType type_) args)
       (compileType retType))

compileType :: AbsLatte.Type -> LLVM.Type
compileType AbsLatte.Int = LLVM.Ti32
compileType AbsLatte.Void = LLVM.Tvoid

compileLatte :: AbsLatte.Program -> CompilerErrorM String
compileLatte (AbsLatte.Program topDefs) =
   let signatures = collectSignatures topDefs in
   do funcLines <- mapM (compileFunc signatures) topDefs
      let allLines = concatMap LLVM.showFunc funcLines
      return $ unlines $ latteMain ++ allLines

compileFunc :: Signatures -> AbsLatte.TopDef -> CompilerErrorM LLVM.Function
compileFunc signatures (AbsLatte.FnDef type_ ident args block) =
   do
      let (argInstrs, valueMap2, nextReg2) =
            foldl (\ (instrs, valueMap0, nextReg0) arg ->
                            let (newInstrs, valueMap1, nextReg1) = saveArgument arg valueMap0 nextReg0 in
                            (instrs ++ newInstrs, valueMap1, nextReg1))
                         ([], initValueMap, initNextRegister) args

      (blockLines, _) <- compileBlock signatures block valueMap2 nextReg2
      return $ LLVM.Function (compileType type_) (compileFuncIdent ident) llvmArgs (argInstrs ++ blockLines ++ [LLVM.IRetVoid | type_ == AbsLatte.Void])
   where

     llvmArgs :: [(LLVM.Type, String)]
     llvmArgs = map (\ (AbsLatte.Arg type_ ident) -> (compileType type_, compileVariableIdent ident)) args

     saveArgument :: AbsLatte.Arg -> ValueMap -> NextRegister -> ([LLVM.Instr], ValueMap, NextRegister)
     saveArgument (AbsLatte.Arg type_ ident) valueMap nextReg =
       do let (ptr, nextReg1) = getNextRegister nextReg
          let llvmType = compileType type_
          let alloc = LLVM.IAlloca llvmType ptr
          let valueMap1 = setVariable (compileVariableIdent ident) ptr valueMap
          ([alloc, LLVM.IStore llvmType (LLVM.VRegister $ LLVM.RArgument (compileVariableIdent ident)) llvmType ptr], valueMap1, nextReg1)

compileBlock :: Signatures -> AbsLatte.Block -> ValueMap -> NextRegister -> CompilerErrorM ([LLVM.Instr], NextRegister)
compileBlock signatures (AbsLatte.Block stmts) valueMap0 nextRegister0 =
  do (instrs, _, reg) <- foldM go ([], valueMap0, nextRegister0) stmts
     return (instrs, reg)
  where go (sLines, valueMap, nextRegister) statement =
          do (newLines, newValueMap, newNextRegister) <- compileStmt signatures statement valueMap nextRegister
             return (sLines ++ newLines, newValueMap, newNextRegister)

compileFlowBlock :: Signatures -> AbsLatte.Stmt -> ValueMap -> NextRegister -> LLVM.Label -> CompilerErrorM ([LLVM.Instr], NextRegister)
compileFlowBlock signatures stmt valueMap0 nextReg0 nextBlock =
  do (stmts, valueMap1, nextReg1) <- compileStmt signatures stmt valueMap0 nextReg0
     return (stmts ++ [LLVM.IBr nextBlock], nextReg1)

compileAssign signatures expr valueMap nextReg ptr =
  do (value, stmts, newNextReg) <- compileExpr signatures expr valueMap nextReg
     return (stmts ++ [LLVM.IStore LLVM.Ti32 value LLVM.Ti32 ptr], newNextReg)

compileStmt :: Signatures -> AbsLatte.Stmt -> ValueMap -> NextRegister -> CompilerErrorM ([LLVM.Instr], ValueMap, NextRegister)
compileStmt signatures (AbsLatte.SExp expr) valueMap nextReg =
   do (_, stmts, newNextReg) <- compileExpr signatures expr valueMap nextReg
      return (stmts, valueMap, newNextReg)

compileStmt signatures (AbsLatte.Ass ident expr) valueMap nextReg =
   do ptr <- lookupVariable (compileVariableIdent ident) valueMap
      (stmts, nextReg1) <- compileAssign signatures expr valueMap nextReg ptr
      return (stmts, valueMap, nextReg1)

compileStmt signatures (AbsLatte.Decl type_ [AbsLatte.Init ident expr]) valueMap nextReg =
  do let (ptr, nextReg1) = getNextRegister nextReg
     let instr = [LLVM.IAlloca (compileType type_) ptr]
     (stmts, nextReg2) <- compileAssign signatures expr valueMap nextReg1 ptr
     let valueMap1 = setVariable (compileVariableIdent ident) ptr valueMap
     return (instr ++ stmts, valueMap1, nextReg2)

compileStmt signatures (AbsLatte.Ret expr) valueMap nextReg =
  do (value, stmts, newNextReg) <- compileExpr signatures expr valueMap nextReg
     return (stmts ++ [LLVM.IRet LLVM.Ti32 value], valueMap, newNextReg)

compileStmt signatures (AbsLatte.Cond expr stmt1) valueMap0 nextReg0 =
  do (cond, condStmts, nextReg1) <- compileExpr signatures expr valueMap0 nextReg0
     let (ifTrueBlock, nextReg2) = getNextLabel nextReg1
     let (contBlock, nextReg3) = getNextLabel nextReg2
     let branch = LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock contBlock
     (ifBlockStmts, nextReg4) <- compileFlowBlock signatures stmt1 valueMap0 nextReg3 contBlock
     return (condStmts ++ [branch, LLVM.ILabel ifTrueBlock] ++ ifBlockStmts ++ [LLVM.ILabel contBlock], valueMap0, nextReg4)

compileStmt signatures (AbsLatte.CondElse expr stmt1 stmt2) valueMap0 nextReg0 =
  do (cond, condStmts, nextReg1) <- compileExpr signatures expr valueMap0 nextReg0
     let (ifTrueBlock, nextReg2) = getNextLabel nextReg1
     let (ifElseBlock, nextReg3) = getNextLabel nextReg2
     let (contBlock, nextReg4) = getNextLabel nextReg3
     let branch = LLVM.IBrCond LLVM.Ti1 cond ifTrueBlock ifElseBlock
     (ifTrueBlockStmts, nextReg5) <- compileFlowBlock signatures stmt1 valueMap0 nextReg4 contBlock
     (ifElseBlockStmts, nextReg6) <- compileFlowBlock signatures stmt2 valueMap0 nextReg5 contBlock
     return (condStmts ++ [branch, LLVM.ILabel ifTrueBlock] ++ ifTrueBlockStmts ++ [LLVM.ILabel ifElseBlock] ++ ifElseBlockStmts ++ [LLVM.ILabel contBlock], valueMap0, nextReg6)

compileStmt signatures (AbsLatte.While expr stmt) valueMap0 nextReg0 =
  do let (condBlock, nextReg1) = getNextLabel nextReg0
     let (bodyBlock, nextReg2) = getNextLabel nextReg1
     let (contBlock, nextReg3) = getNextLabel nextReg2
     (bodyBlockInstrs, nextReg4) <- compileFlowBlock signatures stmt valueMap0 nextReg3 condBlock
     (cond, condInstrs, nextReg5) <- compileExpr signatures expr valueMap0 nextReg4
     let condBlockInstrs = condInstrs ++ [LLVM.IBrCond LLVM.Ti1 cond bodyBlock contBlock]
     return ([LLVM.IBr condBlock, LLVM.ILabel bodyBlock] ++ bodyBlockInstrs ++ [LLVM.ILabel condBlock]  ++ condBlockInstrs ++ [LLVM.ILabel contBlock], valueMap0, nextReg5)

compileStmt signatures (AbsLatte.BStmt block) valueMap0 nextReg0 =
  do (instrs, nextReg1) <- compileBlock signatures block valueMap0 nextReg0
     return (instrs, valueMap0, nextReg1)

compileExpr :: Signatures -> AbsLatte.Expr -> ValueMap -> NextRegister -> CompilerErrorM (LLVM.Value, [LLVM.Instr], NextRegister)
compileExpr signatures (AbsLatte.EApp ident args) valueMap nextReg =
   do (sLines, argVals, nextReg1) <- foldM go ([], [], nextReg) args
      retType <- getReturnedType (compileFuncIdent ident) signatures
      if retType == LLVM.Tvoid then
        return (LLVM.VConst 0, sLines ++ [LLVM.ICall retType (compileFuncIdent ident) [(LLVM.Ti32, value) | value <- argVals] Nothing] , nextReg1)
      else do
        let (register, nextReg2) = getNextRegister nextReg1
        return (LLVM.VRegister register, sLines ++ [LLVM.ICall retType (compileFuncIdent ident) [(LLVM.Ti32, value) | value <- argVals] (Just register)], nextReg2)

   where
     go (sLines, argValues, nextReg1) argExpr =
           do (val, newLines, nextReg2) <- compileExpr signatures argExpr valueMap nextReg1
              return (sLines ++ newLines, argValues ++ [val], nextReg2)

compileExpr _ (AbsLatte.ELitInt int) valueMap nextReg =
    return (LLVM.VConst int, [], nextReg)

compileExpr _ (AbsLatte.EVar ident) valueMap nextReg =
  do ptr <- lookupVariable (compileVariableIdent ident) valueMap
     let (reg, nextReg1) = getNextRegister nextReg
     return (LLVM.VRegister reg, [LLVM.ILoad LLVM.Ti32 LLVM.Ti32 ptr reg], nextReg1)

compileExpr signatures (AbsLatte.EAdd exp1 addOp exp2) valueMap nextReg0 =
  compileArithm signatures exp1 (compileAddOperator addOp) exp2 valueMap nextReg0

compileExpr signatures (AbsLatte.EMul exp1 mulOp exp2) valueMap nextReg0 =
  compileArithm signatures exp1 (compileMulOperator mulOp) exp2 valueMap nextReg0

compileExpr signatures AbsLatte.ELitTrue _ nextReg =
  return (LLVM.VTrue, [], nextReg)

compileExpr signatures AbsLatte.ELitFalse _ nextReg =
  return (LLVM.VFalse, [], nextReg)

compileExpr signatures (AbsLatte.ERel exp1 relOp exp2) valueMap nextReg0 =
  do (val1, instr1, nextReg1) <- compileExpr signatures exp1 valueMap nextReg0
     (val2, instr2, nextReg2) <- compileExpr signatures exp2 valueMap nextReg1
     let (reg, nextReg3) = getNextRegister nextReg2
     return (LLVM.VRegister reg, instr1 ++ instr2 ++ [LLVM.IIcmp (compileRelOp relOp) LLVM.Ti32 val1 val2 reg], nextReg3)

compileRelOp AbsLatte.GE  = LLVM.RelOpSGE
compileRelOp AbsLatte.GTH = LLVM.RelOpSGT
compileRelOp AbsLatte.LE  = LLVM.RelOpSLE
compileRelOp AbsLatte.LTH = LLVM.RelOpSLT
compileRelOp AbsLatte.EQU = LLVM.RelOpEQ
compileRelOp AbsLatte.NE = LLVM.RelOpNE

compileArithm signatures exp1 op exp2 valueMap nextReg0 =
  do (val1, instr1, nextReg1) <- compileExpr signatures exp1 valueMap nextReg0
     (val2, instr2, nextReg2) <- compileExpr signatures exp2 valueMap nextReg1
     let (reg, nextReg3) = getNextRegister nextReg2
     return (LLVM.VRegister reg, instr1 ++ instr2 ++ [LLVM.IArithm LLVM.Ti32 val1 val2 op reg], nextReg3)


compileAddOperator :: AbsLatte.AddOp -> LLVM.ArithmOp
compileAddOperator AbsLatte.Plus = LLVM.OAdd
compileAddOperator AbsLatte.Minus = LLVM.OSub

compileMulOperator :: AbsLatte.MulOp -> LLVM.ArithmOp
compileMulOperator AbsLatte.Div = LLVM.OSDiv
compileMulOperator AbsLatte.Mod = LLVM.OSRem
compileMulOperator AbsLatte.Times = LLVM.OMul

compileFuncIdent (AbsLatte.Ident str) | str == "printInt" = "printInt"
                                      | otherwise = "lat_" ++ str

compileVariableIdent (AbsLatte.Ident str) = str

getReturnedType :: String -> Signatures -> CompilerErrorM LLVM.Type
getReturnedType string (Signatures signatures) =
  maybe (raiseCEUndefinedFunction string 0 0)
  (\ (LLVM.FunctionType _ retType) -> return retType )
  (M.lookup string signatures)

getNextRegister :: NextRegister -> (LLVM.Register, NextRegister)
getNextRegister (NextRegister num) = (LLVM.Register num, NextRegister (num + 1))

getNextLabel :: NextRegister -> (LLVM.Label, NextRegister)
getNextLabel (NextRegister num) = (LLVM.Label num, NextRegister (num + 1))
