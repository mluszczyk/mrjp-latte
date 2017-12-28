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
           | VConst Integer
           | VAddress Int Register
data Register = Rrbp | Rrsp | Redi | Resi | Reax | Redx | Recx | R8d | R9d

fromLLVM :: LLVM.Module -> Module
fromLLVM (LLVM.Module _ _ functions) = Module $ concatMap transFunc functions

transFunc :: LLVM.Function -> [TopDef]
transFunc (LLVM.Function _ name _ blocks) =
  Globl ("_" ++ name) :
  Block ("_" ++ name) [ IPushq (VRegister Rrbp)
                      , IMovq (VRegister Rrsp) (VRegister Rrbp)
                      ] :
  concatMap (`transBlock` name) blocks

transLabel :: LLVM.Label -> String -> String
transLabel (LLVM.Label num) fName = "label_" ++ fName ++ "_" ++ show num

transBlock :: LLVM.Block -> String -> [TopDef]
transBlock (LLVM.Block label innerInstrs exitInstr) functionName =
  [Block (transLabel label functionName)
   (concatMap transInstr (innerInstrs ++ [exitInstr]))]

transInstr :: LLVM.Instr -> [Instr]
transInstr (LLVM.ICall _ name args _) =
  argPushInstrs ++ [ICall $ "_" ++ name]
  where
    argPassingRegisters = [Redi, Resi, Redx, Recx, R8d, R9d]
    argPushInstrs = -- TODO: types, more than 6
      [IMovl (transVal value) (VRegister reg) | ((type_, value), reg) <- zip args argPassingRegisters]
transInstr (LLVM.IRet _ val) = [ IMovl (transVal val) (VRegister Reax)
                               , ILeave
                               , IRet
                               ]
transInstr LLVM.IRetVoid = [ ILeave, IRet ]
transInstr instr = error "unimplemented"

transVal :: LLVM.Value -> Value
transVal (LLVM.VConst num) = VConst num
transVal _ = error "unimplemented"

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
