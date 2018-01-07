{-# OPTIONS_GHC -Wall -Werror #-}

-- definition of LLVM IR AST and pretty printing

module LLVM where

import Data.List (intercalate)


data Value = VConst Integer
               | VRegister Register
               | VTrue
               | VFalse
               | VGetElementPtr Int String
               | VUndef
               deriving (Eq, Ord)

data Register = Register Int | RArgument String
              deriving (Eq, Ord, Show)

data Type = Ti32 | Tvoid | Ti1 | Ti8Ptr deriving (Eq, Ord)
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
               | IPhi Type [(LLVM.Value, LLVM.Label)] Register
               | IUnreachable
               deriving Eq

data Block =  Block { bLabel :: Label
                    , bInnerInstrs :: [Instr]
                    , bExitInstr :: Instr }
              deriving Eq

data Cond = RelOpEQ | RelOpNE | RelOpSGT | RelOpSGE | RelOpSLT | RelOpSLE
          deriving Eq

newtype Label = Label Int deriving (Eq, Ord)

data FunctionType = FunctionType [Type] Type deriving Eq
data Function = Function { fType :: Type
                         , fName :: String
                         , fArgs :: [(Type, String)]
                         , fBlocks :: [Block] }
                deriving Eq

data ArithmOp = OAdd | OSub | OMul | OSDiv | OSRem deriving Eq

data Constant = Constant Int String String

data Declare = Declare String FunctionType

data Module = Module { mGlobals :: [Constant]
                     , mDeclares :: [Declare]
                     , mFunctions :: [Function] }

-- data Program =

showValue :: Value -> String
showValue (VConst num) = show num
showValue (VRegister reg) = showRegister reg
showValue VTrue = "1"
showValue VFalse = "0"
showValue (VGetElementPtr num string) =
  "getelementptr inbounds ([" ++ show num ++ " x i8], [" ++
  show num ++ " x i8]* @" ++ string ++ ", i32 0, i32 0)"
showValue VUndef = "undef"

showRegister :: Register -> String
showRegister (Register num) =  "%unnamed_" ++ show num
showRegister (RArgument string) = "%arg_" ++ string

showType :: Type -> String
showType Ti32 = "i32"
showType Tvoid = "void"
showType Ti1 = "i1"
showType Ti8Ptr = "i8*"

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
showInst (IPhi type_ items res) =
  showRegister res ++ " = phi " ++ showType type_ ++ " " ++ intercalate ", " (map
      (\ (value, label) -> "[" ++ showValue value ++ ", %" ++ showLabel label ++ "]")
      items)
showInst IUnreachable = "unreachable"

showCall :: Type -> String -> [(Type, Value)] -> String
showCall retType ident args =
  "call " ++ showType retType ++ " @" ++ ident ++ " (" ++
  intercalate ", " (map showArgPair args) ++
  ")"

  where showArgPair (type_, value) =
          showType type_ ++ " " ++ showValue value

showArithmOp :: ArithmOp -> String
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
showFunc (Function retType ident args blocks) =
          ["define " ++ showType retType ++
          " @" ++ ident ++ "(" ++
          intercalate ", " (map (\ (t, n) -> showType t ++ " " ++ showRegister (RArgument n)) args) ++
          ") {"] ++
          concatMap showBlock blocks ++
          ["}"]

showBlock :: Block -> [String]
showBlock (Block label innerInstrs exitInstr) =
    (showLabel label ++ ":") :
    map (indent . showInst) (innerInstrs ++ [exitInstr])

showGlobal :: Constant -> String
showGlobal (Constant size name string) =
   "@" ++ name ++ " = private unnamed_addr constant [" ++
   show size ++ " x i8] c\"" ++ escape string ++ "\\00\", align 1"

   where
     escape [] = []
     escape ('\\' : s) = '\\': '5' : 'C' : escape s
     escape ('\n' : s) = '\\' : '0' : 'A' : escape s
     escape ('\t' : s) = '\\' : '0' : '9' : escape s
     escape ('"' : s) = '\\' : '2' : '2' : escape s
     escape (a : s) = a : escape s

showDeclare :: Declare -> String
showDeclare (Declare name (FunctionType args ret)) =
  "declare " ++ showType ret ++ " @" ++ name ++ "(" ++
  intercalate ", " (map showType args) ++
  ")"
