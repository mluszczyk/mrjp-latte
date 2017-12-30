{-# OPTIONS_GHC -Wall -Werror #-}

-- ADT representation of x86_64 assembly, transformation of LLVM to assembly
-- and pretty printing.

module X86_64 (fromLLVM, showAsm) where

import qualified LLVM
import qualified TransLLVM

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Writer (tell, Writer, execWriter, when)
import Data.Maybe (mapMaybe)
import Control.Arrow (second)

newtype Module = Module [TopDef]
data TopDef = Globl String
              | Block String [Instr]
              | Asciz String String
data Instr = IPushq Value
             | IMov Size Value Value
             | ILeaq Value Value
             | ISubl Value Value
             | ISubq Value Value
             | IImull Value Value
             | IAddl Value Value
             | IAddq Value Value
             | IDivl Value
             | ICdq
             | ICall String
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
             | ICmp Size Value Value
             | ILeave
             | IRet

data Size = SByte | SLong | SQuad

data Value = VRegister Register
           | VConst Integer
           | VAddress Int Register
           | VGlobal String
data Register = Rrbp | Rrsp | Redi | Resi
                | Reax | Redx | Recx | Rebx
                | Rr8d | Rr9d
                | Rrax
                | Rrdi | Rrsi
                | Rrdx | Rrcx
                | Rr8 | Rr9
                | Rdil | Rsil | Rdl
                | Rcl | Rr8b | Rr9b
                | Ral

type Reg2Mem = M.Map LLVM.Register Value

type InstrWriter = Writer [Instr]

argPassingRegisters :: [Register]
argPassingRegisters = [Rrdi, Rrsi, Rrdx, Rrcx, Rr8, Rr9]

argPassingPositions :: Int -> Register -> [Value]
argPassingPositions offset base =
  [VAddress num base | num <- [offset, offset + 8..]]

fromLLVM :: LLVM.Module -> Module
fromLLVM (LLVM.Module globals _ functions) =
  Module $ concatMap transFunc functions ++ map transGlobal globals

transFunc :: LLVM.Function -> [TopDef]
transFunc function@(LLVM.Function _ name args blocks) =
  Globl ("_" ++ name) :
  Block ("_" ++ name) ([ IPushq (VRegister Rrbp)
                       , IMov SQuad (VRegister Rrsp) (VRegister Rrbp)
                       ] ++ storeArgs) :
  concatMap (\block -> transBlock block name reg2Mem collectedPhis) blocks ++
  map (uncurry transSplitEdge) splitEdges

  where
    typedStackRegisters = map (second LLVM.RArgument) stackArgs
      where
        stackArgs = drop (length argPassingRegisters) args
    localRegisters = S.toList $ S.fromList (
      mapMaybe extractRegister $
      TransLLVM.listValsInFunc function) S.\\
      S.fromList typedStackRegisters
    storeArgs = [IMov size (VRegister (castRegister passReg size))
                            (reg2Mem M.! LLVM.RArgument val)
                 | (passReg, (type_, val)) <- zip argPassingRegisters args
                 , let size = typeToSize type_ ] ++
                [ISubq (VConst (toInteger (align16 localsSpace)))
                       (VRegister Rrsp)]
      where
        localsSpace = if null localsLayout then 0 else - last localsLayout
    localsLayout = scanl1 (+) [-sizeToInt (typeToSize type_) | (type_, _) <- localRegisters]

    extractRegister (type_, LLVM.VRegister reg) = Just (type_, reg)
    extractRegister _ = Nothing
    reg2Mem =
      M.fromList (zip (map snd localRegisters)
                      (map (`VAddress` Rrbp) localsLayout) ++
                  zip (map snd typedStackRegisters)
                      (argPassingPositions 16 Rrbp))

    splitEdges =
      mapMaybe goBlock blocks
      where
        goBlock LLVM.Block { LLVM.bLabel = srcLabel
                           , LLVM.bExitInstr = LLVM.IBrCond _ _ dstLabel _ } =
          Just (srcLabel, dstLabel)
        goBlock _ = Nothing
    transSplitEdge srcLabel dstLabel =
      Block (phiLabel srcLabel dstLabel name) $
        execWriter (transPhis collectedPhis srcLabel dstLabel reg2Mem) ++
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

align16 :: Int -> Int
align16 num = num + ((16 - num `mod` 16) `mod` 16)

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
transInstr instr str reg2Mem collectedPhis label =
  execWriter (transInstrM instr str reg2Mem collectedPhis label)

transInstrM :: LLVM.Instr -> String -> Reg2Mem -> CollectedPhis -> LLVM.Label -> InstrWriter ()
transInstrM (LLVM.ICall fType name args mResultReg) _ reg2Mem _ _ = do
  argPushInstrs
  tell [ICall $ "_" ++ name]
  argPopInstrs
  saveResult
  where
    stackArgs = length args - 6
    argPushInstrs = do
      mapM_ (\ ((type_, value), reg) -> do
        let size = typeToSize type_
        val <- transVal value reg2Mem
        tell [IMov size val (VRegister (castRegister reg size))])
        (zip args argPassingRegisters)
      when (stackArgs > 0) $
        tell [ISubq (VConst (toInteger (align16 (8 * stackArgs)))) (VRegister Rrsp)]
      mapM_ (\ ((type_, value), dstVal) -> do
        let size = typeToSize type_
        val <- transVal value reg2Mem
        tell [IMov size val dstVal])
        (zip (drop (length argPassingRegisters) args)
             (argPassingPositions 0 Rrsp))
    argPopInstrs =
      when (stackArgs > 0) $
        tell [IAddq (VConst (toInteger (align16 (8 * stackArgs)))) (VRegister Rrsp)]
    saveResult = maybe (tell []) (\reg -> do
      val <- transVal (LLVM.VRegister reg) reg2Mem
      let retSize = typeToSize fType
      tell [IMov retSize
                 (VRegister (castRegister Rrax retSize)) val]) mResultReg
transInstrM (LLVM.IRet type_ val) _ reg2Mem _ _ = do
  tv <- transVal val reg2Mem
  let size = typeToSize type_
  tell [ IMov size tv (VRegister (castRegister Rrax size))
       , ILeave
       , IRet
       ]
transInstrM LLVM.IRetVoid _ _ _ _ = tell [ ILeave, IRet ]
transInstrM (LLVM.IArithm _ v1 v2 op reg) _ mem2Reg _ _
    | op == LLVM.OSDiv || op == LLVM.OSRem = do
  tv1 <- transVal v1 mem2Reg
  tv2 <- transVal v2 mem2Reg
  tres <- transVal (LLVM.VRegister reg) mem2Reg
  tell [ IMov SLong tv1 (VRegister Reax)
       , ICdq
       , IMov SLong tv2 (VRegister Rebx)
       , IDivl (VRegister Rebx)
       , IMov SLong (VRegister (if op == LLVM.OSDiv then Reax else Rrdx))
               tres
       ]

transInstrM (LLVM.IArithm _ v1 v2 op reg) _ mem2Reg _ _ = do
  tv1 <- transVal v1 mem2Reg
  tv2 <- transVal v2 mem2Reg
  res <- transVal (LLVM.VRegister reg) mem2Reg
  tell [ IMov SLong tv1 (VRegister Reax)
        , asmOp tv2 (VRegister Reax)
        , IMov SLong (VRegister Reax) res
        ]
  where
    asmOp = case op of
      LLVM.OSub -> ISubl
      LLVM.OMul -> IImull
      LLVM.OAdd -> IAddl
      _ -> error "asmOp: unreachable"

transInstrM (LLVM.IBr label) fName reg2Mem collectedPhis curLabel = do
  transPhis collectedPhis curLabel label reg2Mem
  tell [ IJmp (transLabel label fName) ]
transInstrM (LLVM.IIcmp cond type_ val1 val2 reg) _ mem2Reg _ _ = do
  tv1 <- transVal val1 mem2Reg
  tv2 <- transVal val2 mem2Reg
  res <- transVal (LLVM.VRegister reg) mem2Reg
  let size = typeToSize type_
  tell [ IMov size tv1 (VRegister (castRegister Rrax size))
        , ICmp size tv2 (VRegister (castRegister Rrax size))
        , asmOp res
        ]
  where
    asmOp = case cond of
      LLVM.RelOpEQ -> ISete
      LLVM.RelOpNE -> ISetne
      LLVM.RelOpSGE -> ISetge
      LLVM.RelOpSGT -> ISetg
      LLVM.RelOpSLE -> ISetle
      LLVM.RelOpSLT -> ISetl

transInstrM (LLVM.IBrCond _ val label1 label2)
            fName reg2Mem collectedPhis curLabel = do
  cond <- transVal val reg2Mem
  tell [ ITestb (VConst 1) cond
       , IJnz (phiLabel curLabel label1 fName) ]
  transPhis collectedPhis curLabel label2 reg2Mem
  tell [ IJmp (transLabel label2 fName) ]
transInstrM LLVM.IPhi {} _ _ _ _ = tell []
transInstrM LLVM.ILabel {} _ _ _ _ =
  error "transInstrM LLVM.ILabel: unreachable"
transInstrM LLVM.ILoad {} _ _ _ _ =
  error "transInstrM LLVM.ILoad: unreachable"
transInstrM LLVM.IStore {} _ _ _ _ =
  error "transInstrM LLVM.IStore: unreachable"
transInstrM LLVM.IAlloca {} _ _ _ _ =
  error "transInstrM LLVM.IAlloca: unreachable"
transInstrM LLVM.IUnreachable _ _ _ _ =
  error "transInstrM LLVM.IUnreachable: unreachable"

transPhis :: CollectedPhis -> LLVM.Label -> LLVM.Label -> Reg2Mem
             -> InstrWriter ()
transPhis collectedPhis srcLabel dstLabel reg2Mem =
  mapM_ (\(type_, reg, val) -> do
    tv <- transVal val reg2Mem
    res <- transVal (LLVM.VRegister reg) reg2Mem
    let size = typeToSize type_
    tell [ IMov size tv (VRegister (castRegister Rrax size))
         , IMov size (VRegister (castRegister Rrax size)) res
         ]) $ M.findWithDefault [] (srcLabel, dstLabel) collectedPhis

transVal :: LLVM.Value -> Reg2Mem -> InstrWriter Value
transVal (LLVM.VConst num) _ = return $ VConst num
transVal (LLVM.VRegister reg) reg2Mem = return $ reg2Mem M.! reg
transVal LLVM.VTrue _ = return $ VConst 1
transVal LLVM.VFalse _ = return $ VConst 0
transVal LLVM.VUndef _ = error "transVal LLVM.VUndef: unreachable"
transVal (LLVM.VGetElementPtr _ str) _ = do
  tell [ILeaq (VGlobal str) (VRegister Rrax)]
  return $ VRegister Rrax

transGlobal :: LLVM.Constant -> TopDef
transGlobal (LLVM.Constant _ name value) = Asciz name value

showAsm :: Module -> String
showAsm (Module topDefs) =
  unlines $ concatMap showTopDef topDefs

showTopDef :: TopDef -> [String]
showTopDef (Globl string) = [".globl " ++ string]
showTopDef (Block name instrs) = (name ++ ":") : map (ident . showInstr) instrs
  where
    ident = ("  " ++)
showTopDef (Asciz name value) = [name ++ ":", "  .asciz \"" ++ escape value ++ "\""]
  where
    escape :: String -> String
    escape [] = []
    escape ('\\' : s) = '\\': '1' : '3' : '4' : escape s
    escape ('\n' : s) = '\\' : '1' : '2' : escape s
    escape ('\t' : s) = '\\' : '1' : '1' : escape s
    escape ('"' : s) = '\\' : '4' : '2' : escape s
    escape (a : s) = a : escape s

showInstr :: Instr -> String
showInstr (IPushq val) = "pushq " ++ showVal val
showInstr (IMov size val1 val2) =
  "mov" ++ showSuffix size ++ " " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (ISubl val1 val2) = "subl " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (ISubq val1 val2) = "subq " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (IImull val1 val2) = "imull " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (IAddl val1 val2) = "addl " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (IAddq val1 val2) = "addq " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (IDivl val) = "divl " ++ showVal val
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
showInstr (ICmp size val1 val2) =
  "cmp" ++ showSuffix size ++ " " ++ showVal val1 ++ ", " ++ showVal val2
showInstr (ILeaq val1 val2) = "leaq " ++ showVal val1 ++ ", " ++ showVal val2

showSuffix :: Size -> String
showSuffix suf = case suf of
  SByte -> "b"
  SLong -> "l"
  SQuad -> "q"

showVal :: Value -> String
showVal (VRegister reg) = "%" ++ showRegister reg
showVal (VConst num) = "$" ++ show num
showVal (VAddress displ base) = show displ ++ "(%" ++ showRegister base ++ ")"
showVal (VGlobal str) = str ++ "(%rip)"

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
  Rrax -> "rax"
  Rrdi -> "rdi"
  Rrsi -> "rsi"
  Rrcx -> "rcx"
  Rr8 -> "r8"
  Rr9 -> "r9"
  Rdil -> "dil"
  Rsil -> "sil"
  Rdl -> "dl"
  Rcl -> "cl"
  Rr9b -> "r9b"
  Rr8b -> "r8b"
  Ral -> "al"
  Rebx -> "ebx"

typeToSize :: LLVM.Type -> Size
typeToSize LLVM.Ti32 = SLong
typeToSize LLVM.Ti8Ptr = SQuad
typeToSize LLVM.Ti1 = SByte
typeToSize _ = error "typeToSize: unreachable"

sizeToInt :: Size -> Int
sizeToInt size = case size of SLong -> 4
                              SQuad -> 8
                              SByte -> 1

castRegister :: Register -> Size -> Register
castRegister reg SLong = case reg of
  Rrdi -> Redi
  Rrsi -> Resi
  Rrdx -> Redx
  Rrcx -> Recx
  Rr8 -> Rr8d
  Rr9 -> Rr9d
  Rrax -> Reax
  _ -> error "castRegister: unreachable"

castRegister reg SByte = case reg of
  Rrdi -> Rdil
  Rrsi -> Rsil
  Rrdx -> Rdl
  Rrcx -> Rcl
  Rr8 -> Rr8b
  Rr9 -> Rr9b
  Rrax -> Ral
  _ -> error "castRegister: unreachable"
castRegister reg SQuad = reg
