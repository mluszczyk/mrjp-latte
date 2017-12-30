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
  concatMap (\block -> transBlock block name reg2Mem collectedPhis) blocks ++
  map (uncurry transSplitEdge) splitEdges

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

    splitEdges =
      mapMaybe goBlock blocks
      where
        goBlock LLVM.Block { LLVM.bLabel = srcLabel
                           , LLVM.bExitInstr = LLVM.IBrCond _ _ dstLabel _ } =
          Just (srcLabel, dstLabel)
        goBlock _ = Nothing
    transSplitEdge srcLabel dstLabel =
      Block (phiLabel srcLabel dstLabel name) $
        transPhis collectedPhis srcLabel dstLabel reg2Mem ++
        [IJmp (transLabel dstLabel name)]
    collectedPhis :: CollectedPhis
    collectedPhis =
      M.unionsWith (++) (concatMap goBlock blocks)
      where
        goBlock block = map goInstr (LLVM.bInnerInstrs block)
          where
            goInstr (LLVM.IPhi type_ pairs reg) = M.fromList
              [ ((srcBlock, LLVM.bLabel block), [(type_, reg, val)])
              | (val, srcBlock) <- pairs]
            goInstr _ = M.empty

type CollectedPhis = M.Map (LLVM.Label, LLVM.Label)
                        [(LLVM.Type, LLVM.Register, LLVM.Value)]

transLabel :: LLVM.Label -> String -> String
transLabel (LLVM.Label num) fName = "label_" ++ fName ++ "_" ++ show num

phiLabel :: LLVM.Label -> LLVM.Label -> String -> String
phiLabel (LLVM.Label src) (LLVM.Label dst) fName =
  "phi_" ++ fName ++ "_" ++ show src ++ "_" ++ show dst

transBlock :: LLVM.Block -> String -> Reg2Mem -> CollectedPhis -> [TopDef]
transBlock (LLVM.Block label innerInstrs exitInstr) functionName reg2Mem collectedPhis =
  [Block (transLabel label functionName)
   (concatMap (\instr -> transInstr instr functionName reg2Mem collectedPhis label)
              (innerInstrs ++ [exitInstr]))]

transInstr :: LLVM.Instr -> String -> Reg2Mem -> CollectedPhis -> LLVM.Label -> [Instr]
transInstr (LLVM.ICall _ name args mResultReg) _ reg2Mem _ _ =
  argPushInstrs ++ [ICall $ "_" ++ name] ++ saveResult
  where
    argPushInstrs = -- TODO: types, more than 6
      [IMovl (transVal value reg2Mem) (VRegister reg)
        | ((_, value), reg) <- zip args argPassingRegisters]
    saveResult = maybe [] (\reg -> [IMovl (VRegister Reax) (transVal (LLVM.VRegister reg) reg2Mem)]) mResultReg
transInstr (LLVM.IRet _ val) _ reg2Mem _ _ =
   [ IMovl (transVal val reg2Mem) (VRegister Reax)
   , ILeave
   , IRet
   ]
transInstr LLVM.IRetVoid _ _ _ _ = [ ILeave, IRet ]
transInstr (LLVM.IArithm _ v1 v2 op reg) _ mem2Reg _ _
    | op == LLVM.OSDiv
    , op == LLVM.OSRem =
  [ IMovl (transVal v1 mem2Reg) (VRegister Reax)
  , ICdq
  , IDivl (transVal v2 mem2Reg)
  , IMovl (VRegister (if op == LLVM.OSDiv then Reax else Rrdx))
          (transVal (LLVM.VRegister reg) mem2Reg)
  ]

transInstr (LLVM.IArithm _ v1 v2 op reg) _ mem2Reg _ _ =
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

transInstr (LLVM.IBr label) fName reg2Mem collectedPhis curLabel =
  transPhis collectedPhis curLabel label reg2Mem ++
  [ IJmp (transLabel label fName) ]
transInstr (LLVM.IIcmp cond _ val1 val2 reg) _ mem2Reg _ _ =
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

transInstr (LLVM.IBrCond _ val label1 label2) fName reg2Mem collectedPhis curLabel =
  [ ITestb (VConst 1) (transVal val reg2Mem)
  , IJnz (phiLabel curLabel label1 fName) ] ++
  transPhis collectedPhis curLabel label2 reg2Mem ++
  [ IJmp (transLabel label2 fName)
  ]
transInstr LLVM.IPhi {} _ _ _ _ = []
transInstr LLVM.ILabel {} _ _ _ _ = error "unreachable"
transInstr LLVM.ILoad {} _ _ _ _ = error "unreachable"
transInstr LLVM.IStore {} _ _ _ _ = error "unreachable"
transInstr LLVM.IAlloca {} _ _ _ _ = error "unreachable"
transInstr LLVM.IUnreachable _ _ _ _ = error "unreachable"

transPhis :: CollectedPhis -> LLVM.Label -> LLVM.Label -> Reg2Mem -> [Instr]
transPhis collectedPhis srcLabel dstLabel reg2Mem = concat
  [ [ IMovl (transVal val reg2Mem) (VRegister Reax)
    , IMovl (VRegister Reax) (transVal (LLVM.VRegister reg) reg2Mem)
    ]
  | (_, reg, val) <- M.findWithDefault [] (srcLabel, dstLabel) collectedPhis
  ]

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
