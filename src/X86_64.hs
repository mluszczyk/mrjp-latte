{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- ADT representation of x86_64 assembly, transformation of LLVM to assembly
-- and pretty printing.

module X86_64 (fromLLVM, showAsm, colourGraph) where

import qualified LLVM
import qualified TransLLVM

import qualified Data.Map as M

import Data.List ((\\), sort, nub, elemIndex)
import Data.Tuple (swap)
import Control.Monad.Writer (tell, Writer, execWriter, when)
import Data.Maybe (mapMaybe, fromMaybe, isJust)

newtype Module = Module [TopDef]
data TopDef = Globl String
              | Block String [Instr]
              | Asciz String String
data Instr = IPushq Value
             | IMov Size Value Value
             | IXchg Size Value Value
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
               deriving Eq

type Reg2Mem = M.Map LLVM.Register Value


type InstrWriter = Writer [Instr]

argPassingRegisters :: [Register]
argPassingRegisters = [Rrdi, Rrsi, Rrdx, Rrcx, Rr8, Rr9]

-- x86_64
-- calleePreservedRegisters: r12, r13, r14, r15, rbx, rsp, rbp
-- rax, rbx, rcx, rdx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12-r15
-- rax, rcx, rdx have special use in division - never alloc them
-- rdi, rsi, r8, r9, r10, r11 are best for allocation
allocatedRegisters :: [Register]
allocatedRegisters = [Rrdi, Rrsi, Rr8, Rr9]

argPassingPositions :: Int -> Register -> [Value]
argPassingPositions offset base =
  [VAddress num base | num <- [offset, offset + 8..]]

fromLLVM :: LLVM.Module -> Module
fromLLVM (LLVM.Module globals _ functions) =
  Module $ concatMap transFunc functions ++ map transGlobal globals

smartmov :: Size -> Value -> Value -> [Instr]
smartmov size val1@(VAddress _ _) val2@(VAddress _ _)  =
  [IMov size val1 (VRegister (castRegister Rrax size)),
   IMov size (VRegister (castRegister Rrax size)) val2]
smartmov size val1 val2 =
  [IMov size val1 val2]

usedQuadRegisters :: Reg2Mem -> [Register]
usedQuadRegisters reg2Mem = nub (map (`castRegister` SQuad) (mapMaybe extractRegister (M.elems reg2Mem)))
  where
    extractRegister val = case val of VRegister reg -> Just reg
                                      _ -> Nothing

transFunc :: LLVM.Function -> [TopDef]
transFunc function@(LLVM.Function _ name args blocks) =
  Globl ("_" ++ name) :
  Block ("_" ++ name) ([ IPushq (VRegister Rrbp)
                       , IMov SQuad (VRegister Rrsp) (VRegister Rrbp)
                       ] ++ storeArgs) :
  concatMap (\block -> transBlock block name reg2Mem collectedPhis) blocks ++
  map (uncurry transSplitEdge) splitEdges

  {- + construct inference graph -> (Vertices, Edges)
     + colour the graph (map LLVM.Register Colour) Colour = Int
     + map Colour Value,
       mind that some args are already in the memory
     move args from stack to registers if needed
     move args from registers to stack if needed
     if using callee saved register, save their value
     + changes: at function calls, save all (used) caller saved registers
     +          handle phis correctly
   -}
 -- TOOD: make sure that no two args have the same place in memory (unused args)
  where
    regType = TransLLVM.registerType function
    reg2Mem :: Reg2Mem
    reg2Mem = M.mapWithKey (\reg val -> castVal val (regType M.! reg))
                           (M.map (colourToMem M.!) regColours)
     where
       castVal (VRegister reg) type_ =
         VRegister (castRegister reg (typeToSize type_))
       castVal val _ = val
    regColours = colourGraph (TransLLVM.inferenceGraph function)
    distinctColours = nub (sort (M.elems regColours))
    (colourToMemArg, coloursForMemArgs) = (colourToMemArg_, coloursForMemArgs_)
      where
        colourToMemArg_ = M.fromList $ map swap $
            filter highColour $ filter isMemArgument $ M.toList regColours
        coloursForMemArgs_ = sort $ nub $ map snd $ filter highColour $ filter isMemArgument $ M.toList regColours
        highColour (_, num) = num >= length allocatedRegisters
        isMemArgument (LLVM.RArgument argName, _) =
          maybe (error "unreachable") (>= length argPassingRegisters) (elemIndex (Just argName) (map snd args))
        isMemArgument _ = False

    regArgs :: [Register]
    regArgs = nub (M.elems regArgColourToReg)
    coloursForRegArgs :: [Int]
    coloursForRegArgs = M.keys regArgColourToReg
    regArgColourToReg :: M.Map Int Register
    regArgColourToReg = M.fromList (mapMaybe argNumToRegister colourToArgNum)
      where
        colourToArgNum :: [(Int, Int)]
        colourToArgNum = mapMaybe (extractArgNum . swap) (M.toList regColours)
        extractArgNum :: (Int, LLVM.Register) -> Maybe (Int, Int)
        extractArgNum (colour, LLVM.RArgument name_) = Just (colour, argPosition name_)
        extractArgNum _ = Nothing
        argNumToRegister (colour, num) | num < length argPassingRegisters =
          Just (colour, argPassingRegisters !! num)
        argNumToRegister _ = Nothing
      -- map snd (take (length argPassingRegisters) args)
    memArgToAddress (LLVM.RArgument name_) =
      argPassingPositions 16 Rrbp !! argPosition name_
    memArgToAddress _ = error "unreachable"

    argPosition :: String -> Int
    argPosition argName = let Just i = elemIndex (Just argName) (map snd args) in i

    colourToMem = M.map memArgToAddress colourToMemArg `M.union`
      M.map VRegister regArgColourToReg `M.union`
      M.fromList (
      zip ((distinctColours \\ coloursForMemArgs) \\ coloursForRegArgs)
           (map VRegister (allocatedRegisters \\ regArgs) ++
            [VAddress num Rrbp | num <- [offset, offset-8..]]))
      where
        offset =  -- save place for register backup
          - 8 - length allocatedRegisters * 8

    storeArgs = [IMov size (VRegister (castRegister passReg size))
                            (reg2Mem M.! LLVM.RArgument val)
                 | (passReg, (type_, mVal)) <- zip argPassingRegisters args
                 , isJust mVal
                 , let size = typeToSize type_
                       Just val = mVal ] ++
                [ISubq (VConst (toInteger
                                (align16 (8 * length distinctColours))))
                       (VRegister Rrsp)] ++
                concat [ smartmov size passAddr (reg2Mem M.! LLVM.RArgument val)
                | (passAddr, (type_, mVal)) <- zip (argPassingPositions 16 Rrbp) (drop (length argPassingRegisters) args)
                , isJust mVal
                , let size = typeToSize type_
                      Just val = mVal ]

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
    collectedPhis :: TransLLVM.CollectedPhis
    collectedPhis = TransLLVM.collectPhis function

align16 :: Int -> Int
align16 num = num + ((16 - num `mod` 16) `mod` 16)

transLabel :: LLVM.Label -> String -> String
transLabel (LLVM.Label num) fName = "label_" ++ fName ++ "_" ++ show num

phiLabel :: LLVM.Label -> LLVM.Label -> String -> String
phiLabel (LLVM.Label src) (LLVM.Label dst) fName =
  "phi_" ++ fName ++ "_" ++ show src ++ "_" ++ show dst

transBlock :: LLVM.Block -> String -> Reg2Mem -> TransLLVM.CollectedPhis
              -> [TopDef]
transBlock (LLVM.Block label innerInstrs exitInstr) functionName reg2Mem collectedPhis =
  [Block (transLabel label functionName)
   (concatMap (\instr -> transInstr instr functionName reg2Mem collectedPhis label)
              (innerInstrs ++ [exitInstr]))]

transInstr :: LLVM.Instr -> String -> Reg2Mem -> TransLLVM.CollectedPhis
              -> LLVM.Label -> [Instr]
transInstr instr str reg2Mem collectedPhis label =
  execWriter (transInstrM instr str reg2Mem collectedPhis label)

transInstrM :: LLVM.Instr -> String -> Reg2Mem -> TransLLVM.CollectedPhis
               -> LLVM.Label -> InstrWriter ()
transInstrM (LLVM.ICall fType name args mResultReg) _ reg2Mem _ _ = do
  argPushInstrs
  saveRegisters
  tell [ICall $ "_" ++ name]
  argPopInstrs
  restoreRegisters
  saveResult
  where
    stackArgs = length args - 6
    argPushInstrs = do
      mapM_ (\ ((type_, value), dstVal) -> do
        let size = typeToSize type_
        val <- transVal value reg2Mem
        tell $ smartmov size val (castVal dstVal size))
        (zip args safeArgPassingPositions)
      when (stackArgs > 0) $
        tell [ISubq (VConst (toInteger (align16 (8 * stackArgs)))) (VRegister Rrsp)]
      mapM_ (\ ((type_, value), dstVal) -> do
        let size = typeToSize type_
        val <- transVal value reg2Mem
        tell $ smartmov size val dstVal)
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
    saveRegisters =
      mapM_ (\(reg, backup) -> tell [IXchg SQuad (VRegister reg) backup]) usedRegisters
    restoreRegisters =
      mapM_ (\(reg, backup) -> tell [IXchg SQuad backup (VRegister reg)]) usedRegisters
    usedRegisters = zip (usedQuadRegisters reg2Mem)
                        [VAddress num Rrbp | num <- [-8, -16..]]
    safeArgPassingPositions = map backup argPassingRegisters
      where
        backup reg = fromMaybe (VRegister reg) (lookup reg usedRegisters)
    castVal :: Value -> Size -> Value
    castVal val size = case val of VRegister reg -> VRegister $ castRegister reg size
                                   _ -> val

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
       , IMov SLong tv2 (VRegister Recx)
       , IDivl (VRegister Recx)
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

transPhis :: TransLLVM.CollectedPhis -> LLVM.Label -> LLVM.Label -> Reg2Mem
             -> InstrWriter ()
transPhis collectedPhis srcLabel dstLabel reg2Mem = do
  mapM_ (\((type_, _, val), tempAddr) -> do
    tv <- transVal val reg2Mem
    let size = typeToSize type_
    tell $ smartmov size tv tempAddr) (zip phis temporaries)
  mapM_ (\(tempAddr, (type_, reg, _)) -> do
    res <- transVal (LLVM.VRegister reg) reg2Mem
    let size = typeToSize type_
    tell $ smartmov size tempAddr res) (zip temporaries phis)

  where
    phis = M.findWithDefault [] (srcLabel, dstLabel) collectedPhis
    temporaries = [VAddress num Rrsp | num <- [-8, -16..]]

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
showInstr (IXchg size val1 val2) =
  "xchg" ++ showSuffix size ++ " " ++ showVal val1 ++ ", " ++ showVal val2
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
castRegister reg SQuad = case reg of
  Redi -> Rrdi
  Resi -> Rrsi
  Redx -> Rrdx
  Recx -> Rrcx
  Rr8d -> Rr8
  Rr9d -> Rr9
  Reax -> Rrax

  Rdil -> Rrdi
  Rsil -> Rrsi
  Rdl -> Rrdx
  Rcl -> Rrcx
  Rr8b -> Rr8
  Rr9b -> Rr9
  Ral -> Rrax

  Rrdi -> Rrdi
  Rrsi -> Rrsi
  Rrdx -> Rrdx
  Rrcx -> Rrcx
  Rr8 -> Rr8
  Rr9 -> Rr9
  Rrax -> Rrax

  _ -> error "castRegister _ SQuad: unreachable"

colourGraph :: forall a. Ord a => ([a], [(a, a)]) -> M.Map a Int
{- http://web.cs.ucla.edu/~palsberg/paper/aplas05.pdf
   Register Allocation via Colouring of Chordal Graphs.
   Figure 4 and 5. -}
colourGraph (vertices, edges) =
  foldl colourVertex M.empty mcsOrder
  where
    colourVertex :: M.Map a Int -> a -> M.Map a Int
    colourVertex partialColouring vertex =
      M.insert vertex myColour partialColouring
      where
        myColour :: Int
        myColour = lowestNotIn (mapMaybe (`M.lookup` partialColouring)
                                (neighbours vertex))

    neighbours :: a -> [a]
    neighbours vertex = [ neigh | (a, b) <- edges
                                , let neigh = if a == vertex then b else a
                                , a == vertex || b == vertex]
    lowestNotIn :: [Int] -> Int
    lowestNotIn list = head ([0..] \\ list)

    mcsOrder :: [a]
    mcsOrder = result
      where
        (_, _, result) = foldl go (vertices, zip vertices [0..], []) vertices
        go :: ([a], [(a, Int)], [a]) -> a -> ([a], [(a, Int)], [a])
        go (remaining, lambda, pResult) _ =
          let (cur, _) = maxLambda in
          (remaining \\ [cur],
           [(ver, val) | (ver, oldVal) <- lambda
                       , let val = if ver `elem` neighbours cur
                                   then oldVal + 1 else oldVal],
           pResult ++ [cur])
          where
            remainingLambda = filter (\(key, _) -> key `elem` remaining) lambda
            maxLambda = foldl (\ maxT@(maxKey, _) curT@(curKey, _) ->
                if curKey > maxKey then curT else maxT)
                        (head remainingLambda) remainingLambda
