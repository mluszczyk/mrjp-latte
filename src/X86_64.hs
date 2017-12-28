module X86_64 (fromLLVM, showAsm) where

import qualified LLVM

newtype Module = Module [TopDef]
data TopDef = Globl String | Block String [Instr]
data Instr = IPushq Value
             | IMovq Value Value
             | ISubq Value Value
             | ICall String
             | IMovl Value Value
             | ILeave
             | IRet

data Value = VRegister Register
           | VConst Int
           | VAddress Int Register
data Register = Rrbp | Rrsp | Redi | Reax

fromLLVM :: LLVM.Module -> Module
fromLLVM _ = Module [ Globl "_main"
                    , Block "_main" [ IPushq (VRegister Rrbp)
                                    , IMovq (VRegister Rrsp) (VRegister Rrbp)
                                    , ISubq (VConst 16) (VRegister Rrsp)
                                    , IMovl (VRegister Redi) (VAddress (-4) Rrbp)
                                    , IMovl (VConst 123) (VRegister Redi)
                                    , ICall "_latte_printInt"
                                    , IMovl (VConst 42) (VRegister Reax)
                                    , ILeave
                                    , IRet
                                    ]
                    ]

showAsm :: Module -> String
showAsm (Module topDefs) =
  unlines $ concatMap showTopDef topDefs

showTopDef :: TopDef -> [String]
showTopDef (Globl string) = [".globl " ++ string]
showTopDef (Block name instrs) = (name ++ ":") : map (ident . showInstr) instrs
  where
    ident = ("  " ++)

showInstr :: Instr -> String
showInstr (IPushq val) = "pushq " ++ showVal val
showInstr (IMovq val1 val2) = "movq " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (ISubq val1 val2) = "subq " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (IMovl val1 val2) = "movl " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (ICall string) = "call " ++ string
showInstr ILeave = "leave"
showInstr IRet = "ret"

showVal :: Value -> String
showVal (VRegister reg) = "%" ++ showRegister reg
showVal (VConst num) = "$" ++ show num
showVal (VAddress displ base) = show displ ++ "(%" ++ showRegister base ++ ")"

showRegister :: Register -> String
showRegister reg = case reg of
  Rrbp -> "rbp"
  Rrsp -> "rsp"
  Redi -> "edi"
  Reax -> "eax"
