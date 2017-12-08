module LLVM where

import Data.List (intercalate)


data Value = VConst Integer
               | VRegister Register
               | VTrue
               | VFalse

data Register = Register Int | RArgument String

data Type = Ti32 | Tvoid | Ti1 deriving Eq
data Instr = ICall Type String [(Type, Value)] (Maybe Register)
               | IRetVoid
               | IRet Type Value
               | IArithm Type Value Value ArithmOp Register
               | IBr Label
               | IBrCond Type Value Label Label
               | ILabel Label
               | ILoad Type Type Register Register
               | IStore Type Value Type Register
               | IAlloca Type Register
               | IIcmp Cond Type Value Value Register

data Cond = RelOpEQ | RelOpNE | RelOpSGT | RelOpSGE | RelOpSLT | RelOpSLE

newtype Label = Label Int

data FunctionType = FunctionType [Type] Type deriving Eq
data Function = Function Type String [(Type, String)] [Instr]
data ArithmOp = OAdd | OSub | OMul | OSDiv | OSRem


showValue :: Value -> String
showValue (VConst num) = show num
showValue (VRegister reg) = showRegister reg
showValue VTrue = "1"
showValue VFalse = "0"

showRegister :: Register -> String
showRegister (Register num) =  "%unnamed_" ++ show num
showRegister (RArgument string) = "%arg_" ++ string

showType :: Type -> String
showType Ti32 = "i32"
showType Tvoid = "void"
showType Ti1 = "i1"

indent :: String -> String
indent = ("  " ++)

showLabel :: Label -> String
showLabel (Label num) = "label_" ++ show num

showInst :: Instr -> String
showInst (ICall retType ident args Nothing) = showCall retType ident args
showInst (ICall retType ident args (Just register)) = showRegister register ++ " = " ++ showCall retType ident args
showInst (IRet type_ value) = "ret " ++ showType type_ ++ " " ++ showValue value
showInst IRetVoid = "ret void"
showInst (IArithm type_ v1 v2 op reg) =
  showRegister reg ++ " = " ++ showArithmOp op ++ " " ++ showType type_ ++ " " ++ showValue v1 ++ ", " ++ showValue v2
showInst (IBr label) = "br label %" ++ showLabel label
showInst (ILabel label) = showLabel label ++ ":"
showInst (IBrCond type_ value label1 label2) =
  "br " ++ showType type_ ++ " " ++ showValue value ++
  ", label %" ++ showLabel label1 ++
  ", label %" ++ showLabel label2
showInst (ILoad typeVal typePtr ptrReg targetReg) =
  showRegister targetReg ++ " = load " ++ showType typeVal ++
  ", " ++ showType typePtr ++ "* " ++ showRegister ptrReg
showInst (IAlloca type_ reg) =
  showRegister reg ++ " = alloca " ++ showType type_
showInst (IStore valType val ptrType ptr) =
  "store " ++ showType valType ++ " " ++ showValue val ++ ", " ++
  showType ptrType ++ "* " ++ showRegister ptr
showInst (IIcmp cond valType val1 val2 reg) =
  showRegister reg ++ " = " ++
  "icmp " ++ showCond cond ++ " " ++ showType valType ++ " " ++
  showValue val1 ++ ", " ++ showValue val2

showCall retType ident args =
  "call " ++ showType retType ++ " @" ++ ident ++ " (" ++
  intercalate ", " (map showArgPair args) ++
  ")"

  where showArgPair (type_, value) =
          showType type_ ++ " " ++ showValue value

showArithmOp OAdd = "add"
showArithmOp OMul = "mul"
showArithmOp OSub = "sub"
showArithmOp OSDiv = "sdiv"
showArithmOp OSRem = "srem"

showCond :: Cond -> String
showCond RelOpSLE = "sle"
showCond RelOpSLT = "slt"
showCond RelOpSGE = "sge"
showCond RelOpSGT = "sgt"
showCond RelOpEQ = "eq"
showCond RelOpNE = "ne"

showFunc :: Function -> [String]
showFunc (Function retType ident args body) =
          ["define " ++ showType retType ++
          " @" ++ ident ++ "(" ++
          intercalate ", " (map (\ (t, n) -> showType t ++ " " ++ showRegister (RArgument n)) args) ++
          ") {"] ++
          map (indent . showInst) body ++
          ["}"]
