{-# OPTIONS_GHC -Wall -Werror #-}

module X86_64 (fromLLVM, showAsm) where

import qualified LLVM
import qualified TransLLVM

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)

newtype Module = Module [TopDef]
data TopDef = Globl String | Block String [Instr]
data Instr = IPushq Value
             | IMovq Value Value
             | IMovb Value Value
             | ISubl Value Value
             | ISubq Value Value
             | IImull Value Value
             | IAddl Value Value
             | IDivl Value
             | ICdq
             | ICall String
             | IMovl Value Value
             | IJmp String
             | IJe String
             | IJnz String
             | ISete Value
             | ISetne Value
             | ISetg Value
             | ISetge Value
             | ISetl Value
             | ISetle Value
             | ITestb Value Value
             | ICmpl Value Value
             | ILeave
             | IRet

data Value = VRegister Register
           | VConst Integer
           | VAddress Int Register
data Register = Rrbp | Rrsp | Redi | Resi
                | Reax | Redx | Recx
                | Rr8d | Rr9d
                | Rrdx

type Reg2Mem = M.Map LLVM.Register Value

argPassingRegisters :: [Register]
argPassingRegisters = [Redi, Resi, Redx, Recx, Rr8d, Rr9d]

fromLLVM :: LLVM.Module -> Module
fromLLVM (LLVM.Module _ _ functions) = Module $ concatMap transFunc functions

transFunc :: LLVM.Function -> [TopDef]
transFunc function@(LLVM.Function _ name args blocks) =
  Globl ("_" ++ name) :
  Block ("_" ++ name) ([ IPushq (VRegister Rrbp)
                       , IMovq (VRegister Rrsp) (VRegister Rrbp)
                       ] ++ storeArgs) :
  concatMap (\block -> transBlock block name reg2Mem) blocks

  where
    usedRegisters = S.toList $ S.fromList $
      mapMaybe extractRegister $
      TransLLVM.listValsInFunc function
    storeArgs = [IMovl (VRegister passReg) (reg2Mem M.! LLVM.RArgument (snd arg))
                 | (passReg, arg) <- zip argPassingRegisters args ] ++
                [ISubq (VConst (toInteger (align16 (4 + 4 * length usedRegisters))))
                       (VRegister Rrsp)]
    align16 num = num + ((16 - num `mod` 16) `mod` 16)
    extractRegister (LLVM.VRegister reg) = Just reg
    extractRegister _ = Nothing
    reg2Mem =
      M.fromList (zip usedRegisters
                      (map (\num -> VAddress (-4 * num) Rrbp) [1::Int ..]))

transLabel :: LLVM.Label -> String -> String
transLabel (LLVM.Label num) fName = "label_" ++ fName ++ "_" ++ show num

transBlock :: LLVM.Block -> String -> Reg2Mem -> [TopDef]
transBlock (LLVM.Block label innerInstrs exitInstr) functionName reg2Mem =
  [Block (transLabel label functionName)
   (concatMap (\instr -> transInstr instr functionName reg2Mem)
              (innerInstrs ++ [exitInstr]))]

transInstr :: LLVM.Instr -> String -> Reg2Mem -> [Instr]
transInstr (LLVM.ICall _ name args mResultReg) _ reg2Mem =
  argPushInstrs ++ [ICall $ "_" ++ name] ++ saveResult
  where
    argPushInstrs = -- TODO: types, more than 6
      [IMovl (transVal value reg2Mem) (VRegister reg)
        | ((_, value), reg) <- zip args argPassingRegisters]
    saveResult = maybe [] (\reg -> [IMovl (VRegister Reax) (transVal (LLVM.VRegister reg) reg2Mem)]) mResultReg
transInstr (LLVM.IRet _ val) _ reg2Mem =
   [ IMovl (transVal val reg2Mem) (VRegister Reax)
   , ILeave
   , IRet
   ]
transInstr LLVM.IRetVoid _ _ = [ ILeave, IRet ]
transInstr (LLVM.IArithm _ v1 v2 op reg) _ mem2Reg
    | op == LLVM.OSDiv
    , op == LLVM.OSRem =
  [ IMovl (transVal v1 mem2Reg) (VRegister Reax)
  , ICdq
  , IDivl (transVal v2 mem2Reg)
  , IMovl (VRegister (if op == LLVM.OSDiv then Reax else Rrdx))
          (transVal (LLVM.VRegister reg) mem2Reg)
  ]

transInstr (LLVM.IArithm _ v1 v2 op reg) _ mem2Reg =
  [ IMovl (transVal v1 mem2Reg) (VRegister Reax)
  , asmOp (transVal v2 mem2Reg) (VRegister Reax)
  , IMovl (VRegister Reax) (transVal (LLVM.VRegister reg) mem2Reg)
  ]
  where
    asmOp = case op of
      LLVM.OSub -> ISubl
      LLVM.OMul -> IImull
      LLVM.OAdd -> IAddl
      _ -> error "unreachable"

transInstr (LLVM.IBr label) fName _ = [IJmp (transLabel label fName)]
transInstr (LLVM.IIcmp cond _ val1 val2 reg) _ mem2Reg =
  [ IMovl (transVal val1 mem2Reg) (VRegister Reax)
  , ICmpl (transVal val2 mem2Reg) (VRegister Reax)
  , asmOp (transVal (LLVM.VRegister reg) mem2Reg)
  ]
  where
    asmOp = case cond of
      LLVM.RelOpEQ -> ISete
      LLVM.RelOpNE -> ISetne
      LLVM.RelOpSGE -> ISetge
      LLVM.RelOpSGT -> ISetg
      LLVM.RelOpSLE -> ISetle
      LLVM.RelOpSLT -> ISetl

transInstr (LLVM.IBrCond _ val label1 label2) fName mem2Reg =
  [ ITestb (VConst 1) (transVal val mem2Reg)
  , IJnz (transLabel label1 fName)
  , IJmp (transLabel label2 fName)
  ]
transInstr LLVM.IPhi {} _ _ = error "unimplemented"
transInstr LLVM.ILabel {} _ _ = error "unreachable"
transInstr LLVM.ILoad {} _ _ = error "unreachable"
transInstr LLVM.IStore {} _ _ = error "unreachable"
transInstr LLVM.IAlloca {} _ _ = error "unreachable"
transInstr LLVM.IUnreachable _ _ = error "unreachable"

transVal :: LLVM.Value -> Reg2Mem -> Value
transVal (LLVM.VConst num) _ = VConst num
transVal (LLVM.VRegister reg) reg2Mem = reg2Mem M.! reg
transVal LLVM.VTrue _ = VConst 1
transVal LLVM.VFalse _ = VConst 0
transVal LLVM.VUndef _ = error "unreachable"
transVal (LLVM.VGetElementPtr _ _) _ = error "unimplemented"

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
showInstr (ISubl val1 val2) = "subl " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (ISubq val1 val2) = "subq " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (IImull val1 val2) = "imull " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (IAddl val1 val2) = "addl " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (IDivl val) = "divq " ++ showVal val
showInstr (IMovl val1 val2) = "movl " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (ICall string) = "call " ++ string
showInstr ILeave = "leave"
showInstr IRet = "ret"
showInstr ICdq = "cdq"
showInstr (IJmp label) = "jmp " ++ label
showInstr (IJe label) = "je " ++ label
showInstr (IJnz label) = "jnz " ++ label
showInstr (ISete val) = "sete " ++ showVal val
showInstr (ISetne val) = "setne " ++ showVal val
showInstr (ISetl val) = "setl " ++ showVal val
showInstr (ISetg val) = "setg " ++ showVal val
showInstr (ISetle val) = "setle " ++ showVal val
showInstr (ISetge val) = "setge " ++ showVal val
showInstr (ITestb val1 val2) = "testb " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (IMovb val1 val2) = "movb " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (ICmpl val1 val2) = "cmpl " ++ showVal val1 ++ ", " ++ showVal val2

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
  Resi -> "esi"
  Redx -> "edx"
  Recx -> "ecx"
  Rr8d -> "r8d"
  Rr9d -> "r9d"
  Rrdx -> "rdx"
