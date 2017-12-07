module CompileLatte where

import qualified Data.Map as M
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified AbsLatte
import CompilerErr (CompilerErrorM, raiseCEUndefinedVariable, raiseCEUndefinedFunction)
import Control.Monad (foldM)

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

data Register = Register Int | RArgument String

data LLVMValue = VConst Integer
               | VRegister Register
               | VTrue
               | VFalse

data LLVMType = Ti32 | Tvoid | Ti1 deriving Eq
data LLVMInstr = ICall LLVMType String [(LLVMType, LLVMValue)] (Maybe Register)
               | IRetVoid
               | IRet LLVMType LLVMValue
               | IArithm LLVMType LLVMValue LLVMValue LLVMArithmOp Register
               | IBr Label
               | IBrCond LLVMType LLVMValue Label Label
               | ILabel Label
               | ILoad LLVMType LLVMType Register Register
               | IStore LLVMType LLVMValue LLVMType Register
               | IAlloca LLVMType Register
               | IIcmp LLVMCond LLVMType LLVMValue LLVMValue Register

data LLVMCond = RelOpEQ | RelOpNE | RelOpSGT | RelOpSGE | RelOpSLT | RelOpSLE

newtype Label = Label Int

newtype ValueMap = ValueMap (M.Map String Register)

data LLVMFunctionType = LLVMFunctionType [LLVMType] LLVMType
data LLVMFunction = LLVMFunction LLVMType String [(LLVMType, String)] [LLVMInstr]

newtype Signatures = Signatures (M.Map String LLVMFunctionType)

newtype NextRegister = NextRegister Int

data LLVMArithmOp = OAdd | OSub | OMul | OSDiv | OSRem

initNextRegister :: NextRegister
initNextRegister = NextRegister 0

setVariable :: String -> Register -> ValueMap -> ValueMap
setVariable name value (ValueMap valueMap) =
    ValueMap (M.insert name value valueMap)

lookupVariable :: String -> ValueMap -> CompilerErrorM Register
lookupVariable name (ValueMap valueMap) =
    maybe (raiseCEUndefinedVariable name 0 0) return (M.lookup name valueMap)

initValueMap :: ValueMap
initValueMap = ValueMap M.empty

collectSignatures :: [AbsLatte.TopDef] -> Signatures
collectSignatures topDefs = Signatures $ M.fromList pairs
  where
    pairs = ("printInt", LLVMFunctionType [Ti32] Tvoid) :
            map getSignaturePair topDefs
    getSignaturePair (AbsLatte.FnDef retType ident args _) =
      (compileFuncIdent ident,
       LLVMFunctionType
       (map (\ (AbsLatte.Arg type_ _) -> compileType type_) args)
       (compileType retType))

compileType :: AbsLatte.Type -> LLVMType
compileType AbsLatte.Int = Ti32
compileType AbsLatte.Void = Tvoid

compileLatte :: AbsLatte.Program -> CompilerErrorM String
compileLatte (AbsLatte.Program topDefs) =
   let signatures = collectSignatures topDefs in
   do funcLines <- mapM (compileFunc signatures) topDefs
      let allLines = concatMap showLLVMFunc funcLines
      return $ unlines $ latteMain ++ allLines

compileFunc :: Signatures -> AbsLatte.TopDef -> CompilerErrorM LLVMFunction
compileFunc signatures (AbsLatte.FnDef type_ ident args block) =
   do
      let (argInstrs, valueMap2, nextReg2) =
            foldl (\ (instrs, valueMap0, nextReg0) arg ->
                            let (newInstrs, valueMap1, nextReg1) = saveArgument arg valueMap0 nextReg0 in
                            (instrs ++ newInstrs, valueMap1, nextReg1))
                         ([], initValueMap, initNextRegister) args

      (blockLines, _) <- compileBlock signatures block valueMap2 nextReg2
      return $ LLVMFunction (compileType type_) (compileFuncIdent ident) llvmArgs (argInstrs ++ blockLines ++ [IRetVoid | type_ == AbsLatte.Void])
   where

     llvmArgs :: [(LLVMType, String)]
     llvmArgs = map (\ (AbsLatte.Arg type_ ident) -> (compileType type_, compileVariableIdent ident)) args

     saveArgument :: AbsLatte.Arg -> ValueMap -> NextRegister -> ([LLVMInstr], ValueMap, NextRegister)
     saveArgument (AbsLatte.Arg type_ ident) valueMap nextReg =
       do let (ptr, nextReg1) = getNextRegister nextReg
          let llvmType = compileType type_
          let alloc = IAlloca llvmType ptr
          let valueMap1 = setVariable (compileVariableIdent ident) ptr valueMap
          ([alloc, IStore llvmType (VRegister $ RArgument (compileVariableIdent ident)) llvmType ptr], valueMap1, nextReg1)

compileBlock :: Signatures -> AbsLatte.Block -> ValueMap -> NextRegister -> CompilerErrorM ([LLVMInstr], NextRegister)
compileBlock signatures (AbsLatte.Block stmts) valueMap0 nextRegister0 =
  do (instrs, _, reg) <- foldM go ([], valueMap0, nextRegister0) stmts
     return (instrs, reg)
  where go (sLines, valueMap, nextRegister) statement =
          do (newLines, newValueMap, newNextRegister) <- compileStmt signatures statement valueMap nextRegister
             return (sLines ++ newLines, newValueMap, newNextRegister)

data Cont = Return | Jump Label

compileFlowBlock :: Signatures -> AbsLatte.Stmt -> ValueMap -> NextRegister -> Cont -> CompilerErrorM ([LLVMInstr], NextRegister)
compileFlowBlock signatures stmt valueMap0 nextReg0 (Jump nextBlock) =
  do (stmts, valueMap1, nextReg1) <- compileStmt signatures stmt valueMap0 nextReg0
     return (stmts ++ [IBr nextBlock], nextReg1)

compileAssign signatures expr valueMap nextReg ptr =
  do (value, stmts, newNextReg) <- compileExpr signatures expr valueMap nextReg
     return (stmts ++ [IStore Ti32 value Ti32 ptr], newNextReg)

compileStmt :: Signatures -> AbsLatte.Stmt -> ValueMap -> NextRegister -> CompilerErrorM ([LLVMInstr], ValueMap, NextRegister)
compileStmt signatures (AbsLatte.SExp expr) valueMap nextReg =
   do (_, stmts, newNextReg) <- compileExpr signatures expr valueMap nextReg
      return (stmts, valueMap, newNextReg)

compileStmt signatures (AbsLatte.Ass ident expr) valueMap nextReg =
   do ptr <- lookupVariable (compileVariableIdent ident) valueMap
      (stmts, nextReg1) <- compileAssign signatures expr valueMap nextReg ptr
      return (stmts, valueMap, nextReg1)

compileStmt signatures (AbsLatte.Decl type_ [AbsLatte.Init ident expr]) valueMap nextReg =
  do let (ptr, nextReg1) = getNextRegister nextReg
     let instr = [IAlloca (compileType type_) ptr]
     (stmts, nextReg2) <- compileAssign signatures expr valueMap nextReg1 ptr
     let valueMap1 = setVariable (compileVariableIdent ident) ptr valueMap
     return (instr ++ stmts, valueMap1, nextReg2)

compileStmt signatures (AbsLatte.Ret expr) valueMap nextReg =
  do (value, stmts, newNextReg) <- compileExpr signatures expr valueMap nextReg
     return (stmts ++ [IRet Ti32 value], valueMap, newNextReg)

compileStmt signatures (AbsLatte.Cond expr stmt1) valueMap0 nextReg0 =
  do (cond, condStmts, nextReg1) <- compileExpr signatures expr valueMap0 nextReg0
     let (ifTrueBlock, nextReg2) = getNextLabel nextReg1
     let (contBlock, nextReg3) = getNextLabel nextReg2
     let branch = IBrCond Ti1 cond ifTrueBlock contBlock
     (ifBlockStmts, nextReg4) <- compileFlowBlock signatures stmt1 valueMap0 nextReg3 (Jump contBlock)
     return (condStmts ++ [branch, ILabel ifTrueBlock] ++ ifBlockStmts ++ [ILabel contBlock], valueMap0, nextReg4)

compileStmt signatures (AbsLatte.CondElse expr stmt1 stmt2) valueMap0 nextReg0 =
  do (cond, condStmts, nextReg1) <- compileExpr signatures expr valueMap0 nextReg0
     let (ifTrueBlock, nextReg2) = getNextLabel nextReg1
     let (ifElseBlock, nextReg3) = getNextLabel nextReg2
     let (contBlock, nextReg4) = getNextLabel nextReg3
     let branch = IBrCond Ti1 cond ifTrueBlock ifElseBlock
     (ifTrueBlockStmts, nextReg5) <- compileFlowBlock signatures stmt1 valueMap0 nextReg4 (Jump contBlock)
     (ifElseBlockStmts, nextReg6) <- compileFlowBlock signatures stmt2 valueMap0 nextReg5 (Jump contBlock)
     return (condStmts ++ [branch, ILabel ifTrueBlock] ++ ifTrueBlockStmts ++ [ILabel ifElseBlock] ++ ifElseBlockStmts ++ [ILabel contBlock], valueMap0, nextReg6)

compileStmt signatures (AbsLatte.While expr stmt) valueMap0 nextReg0 =
  do let (condBlock, nextReg1) = getNextLabel nextReg0
     let (bodyBlock, nextReg2) = getNextLabel nextReg1
     let (contBlock, nextReg3) = getNextLabel nextReg2
     (bodyBlockInstrs, nextReg4) <- compileFlowBlock signatures stmt valueMap0 nextReg3 (Jump condBlock)
     (cond, condInstrs, nextReg5) <- compileExpr signatures expr valueMap0 nextReg4
     let condBlockInstrs = condInstrs ++ [IBrCond Ti1 cond bodyBlock contBlock]
     return ([IBr condBlock, ILabel bodyBlock] ++ bodyBlockInstrs ++ [ILabel condBlock]  ++ condBlockInstrs ++ [ILabel contBlock], valueMap0, nextReg5)

compileStmt signatures (AbsLatte.BStmt block) valueMap0 nextReg0 =
  do (instrs, nextReg1) <- compileBlock signatures block valueMap0 nextReg0
     return (instrs, valueMap0, nextReg1)

compileExpr :: Signatures -> AbsLatte.Expr -> ValueMap -> NextRegister -> CompilerErrorM (LLVMValue, [LLVMInstr], NextRegister)
compileExpr signatures (AbsLatte.EApp ident args) valueMap nextReg =
   do (sLines, argVals, nextReg1) <- foldM go ([], [], nextReg) args
      retType <- getReturnedType (compileFuncIdent ident) signatures
      if retType == Tvoid then
        return (VConst 0, sLines ++ [ICall retType (compileFuncIdent ident) [(Ti32, value) | value <- argVals] Nothing] , nextReg1)
      else do
        let (register, nextReg2) = getNextRegister nextReg1
        return (VRegister register, sLines ++ [ICall retType (compileFuncIdent ident) [(Ti32, value) | value <- argVals] (Just register)], nextReg2)

   where
     go (sLines, argValues, nextReg1) argExpr =
           do (val, newLines, nextReg2) <- compileExpr signatures argExpr valueMap nextReg1
              return (sLines ++ newLines, argValues ++ [val], nextReg2)

compileExpr _ (AbsLatte.ELitInt int) valueMap nextReg =
    return (VConst int, [], nextReg)

compileExpr _ (AbsLatte.EVar ident) valueMap nextReg =
  do ptr <- lookupVariable (compileVariableIdent ident) valueMap
     let (reg, nextReg1) = getNextRegister nextReg
     return (VRegister reg, [ILoad Ti32 Ti32 ptr reg], nextReg1)

compileExpr signatures (AbsLatte.EAdd exp1 addOp exp2) valueMap nextReg0 =
  compileArithm signatures exp1 (compileAddOperator addOp) exp2 valueMap nextReg0

compileExpr signatures (AbsLatte.EMul exp1 mulOp exp2) valueMap nextReg0 =
  compileArithm signatures exp1 (compileMulOperator mulOp) exp2 valueMap nextReg0

compileExpr signatures AbsLatte.ELitTrue _ nextReg =
  return (VTrue, [], nextReg)

compileExpr signatures AbsLatte.ELitFalse _ nextReg =
  return (VFalse, [], nextReg)

compileExpr signatures (AbsLatte.ERel exp1 relOp exp2) valueMap nextReg0 =
  do (val1, instr1, nextReg1) <- compileExpr signatures exp1 valueMap nextReg0
     (val2, instr2, nextReg2) <- compileExpr signatures exp2 valueMap nextReg1
     let (reg, nextReg3) = getNextRegister nextReg2
     return (VRegister reg, instr1 ++ instr2 ++ [IIcmp (compileRelOp relOp) Ti32 val1 val2 reg], nextReg3)

compileRelOp AbsLatte.GE  = RelOpSGE
compileRelOp AbsLatte.GTH = RelOpSGT
compileRelOp AbsLatte.LE  = RelOpSLE
compileRelOp AbsLatte.LTH = RelOpSLT
compileRelOp AbsLatte.EQU = RelOpEQ
compileRelOp AbsLatte.NE = RelOpNE

compileArithm signatures exp1 op exp2 valueMap nextReg0 =
  do (val1, instr1, nextReg1) <- compileExpr signatures exp1 valueMap nextReg0
     (val2, instr2, nextReg2) <- compileExpr signatures exp2 valueMap nextReg1
     let (reg, nextReg3) = getNextRegister nextReg2
     return (VRegister reg, instr1 ++ instr2 ++ [IArithm Ti32 val1 val2 op reg], nextReg3)


compileAddOperator :: AbsLatte.AddOp -> LLVMArithmOp
compileAddOperator AbsLatte.Plus = OAdd
compileAddOperator AbsLatte.Minus = OSub

compileMulOperator :: AbsLatte.MulOp -> LLVMArithmOp
compileMulOperator AbsLatte.Div = OSDiv
compileMulOperator AbsLatte.Mod = OSRem
compileMulOperator AbsLatte.Times = OMul

compileFuncIdent (AbsLatte.Ident str) | str == "printInt" = "printInt"
                                      | otherwise = "lat_" ++ str

compileVariableIdent (AbsLatte.Ident str) = str

showValue :: LLVMValue -> String
showValue (VConst num) = show num
showValue (VRegister reg) = showRegister reg
showValue VTrue = "1"
showValue VFalse = "0"

showRegister :: Register -> String
showRegister (Register num) =  "%unnamed_" ++ show num
showRegister (RArgument string) = "%arg_" ++ string

showLLVMType :: LLVMType -> String
showLLVMType Ti32 = "i32"
showLLVMType Tvoid = "void"
showLLVMType Ti1 = "i1"

indent :: String -> String
indent = ("  " ++)

showLabel :: Label -> String
showLabel (Label num) = "label_" ++ show num

showLLVMInst :: LLVMInstr -> String
showLLVMInst (ICall retType ident args Nothing) = showCall retType ident args
showLLVMInst (ICall retType ident args (Just register)) = showRegister register ++ " = " ++ showCall retType ident args
showLLVMInst (IRet type_ value) = "ret " ++ showLLVMType type_ ++ " " ++ showValue value
showLLVMInst IRetVoid = "ret void"
showLLVMInst (IArithm type_ v1 v2 op reg) =
  showRegister reg ++ " = " ++ showLLVMArithmOp op ++ " " ++ showLLVMType type_ ++ " " ++ showValue v1 ++ ", " ++ showValue v2
showLLVMInst (IBr label) = "br label %" ++ showLabel label
showLLVMInst (ILabel label) = showLabel label ++ ":"
showLLVMInst (IBrCond type_ value label1 label2) =
  "br " ++ showLLVMType type_ ++ " " ++ showValue value ++
  ", label %" ++ showLabel label1 ++
  ", label %" ++ showLabel label2
showLLVMInst (ILoad typeVal typePtr ptrReg targetReg) =
  showRegister targetReg ++ " = load " ++ showLLVMType typeVal ++
  ", " ++ showLLVMType typePtr ++ "* " ++ showRegister ptrReg
showLLVMInst (IAlloca type_ reg) =
  showRegister reg ++ " = alloca " ++ showLLVMType type_
showLLVMInst (IStore valType val ptrType ptr) =
  "store " ++ showLLVMType valType ++ " " ++ showValue val ++ ", " ++
  showLLVMType ptrType ++ "* " ++ showRegister ptr
showLLVMInst (IIcmp cond valType val1 val2 reg) =
  showRegister reg ++ " = " ++
  "icmp " ++ showLLVMCond cond ++ " " ++ showLLVMType valType ++ " " ++
  showValue val1 ++ ", " ++ showValue val2

showCall retType ident args =
  "call " ++ showLLVMType retType ++ " @" ++ ident ++ " (" ++
  intercalate ", " (map showArgPair args) ++
  ")"

  where showArgPair (type_, value) =
          showLLVMType type_ ++ " " ++ showValue value

showLLVMArithmOp OAdd = "add"
showLLVMArithmOp OMul = "mul"
showLLVMArithmOp OSub = "sub"
showLLVMArithmOp OSDiv = "sdiv"
showLLVMArithmOp OSRem = "srem"

showLLVMCond :: LLVMCond -> String
showLLVMCond RelOpSLE = "sle"
showLLVMCond RelOpSLT = "slt"
showLLVMCond RelOpSGE = "sge"
showLLVMCond RelOpSGT = "sgt"
showLLVMCond RelOpEQ = "eq"
showLLVMCond RelOpNE = "ne"

showLLVMFunc :: LLVMFunction -> [String]
showLLVMFunc (LLVMFunction retType ident args body) =
          ["define " ++ showLLVMType retType ++
          " @" ++ ident ++ "(" ++
          intercalate ", " (map (\ (t, n) -> showLLVMType t ++ " " ++ showRegister (RArgument n)) args) ++
          ") {"] ++
          map (indent . showLLVMInst) body ++
          ["}"]

getReturnedType :: String -> Signatures -> CompilerErrorM LLVMType
getReturnedType string (Signatures signatures) =
  maybe (raiseCEUndefinedFunction string 0 0)
  (\ (LLVMFunctionType _ retType) -> return retType )
  (M.lookup string signatures)

getNextRegister :: NextRegister -> (Register, NextRegister)
getNextRegister (NextRegister num) = (Register num, NextRegister (num + 1))

getNextLabel :: NextRegister -> (Label, NextRegister)
getNextLabel (NextRegister num) = (Label num, NextRegister (num + 1))
