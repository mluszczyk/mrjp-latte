{-# OPTIONS_GHC -Wall -Werror #-}

module TransLLVM where

import qualified LLVM
import qualified Data.Set as S
import qualified Data.Set.Extra as SE

instrsToBlocks :: [LLVM.Instr] -> [LLVM.Block]
instrsToBlocks [] = []
instrsToBlocks (LLVM.ILabel label : rest) =
  LLVM.Block label innerInstrs lastInstr :
  instrsToBlocks postBlock
  where
    (innerInstrs, lastInstr, postBlock) = getNextBlock rest

    getNextBlock :: [LLVM.Instr] -> ([LLVM.Instr], LLVM.Instr, [LLVM.Instr])
    getNextBlock [] = error "basic block missies exit"
    getNextBlock (LLVM.ILabel {} : _) = error "basic block misses exit"
    getNextBlock (instr : rest') = if isExit instr then
      ([], instr, rest')
    else
      let (inner, exit, rest'') = getNextBlock rest'
      in (instr : inner, exit, rest'')
-- Ignore instruction if no block is open.
instrsToBlocks (_ : rest) = instrsToBlocks rest

isExit :: LLVM.Instr -> Bool
isExit instr = case instr of
  LLVM.IUnreachable -> True
  LLVM.IRet {} -> True
  LLVM.IRetVoid -> True
  LLVM.IBr {} -> True
  LLVM.IBrCond {} -> True
  _ -> False

removeUnreachableBlocks :: LLVM.Function -> LLVM.Function
removeUnreachableBlocks function@LLVM.Function { LLVM.fBlocks = blocks} =
  function { LLVM.fBlocks = filter
      (\ LLVM.Block { LLVM.bLabel = label } -> label `elem` reachable) blocks }

  where
    instrNeighbours :: LLVM.Instr -> [LLVM.Label]
    instrNeighbours (LLVM.IBr label) = [label]
    instrNeighbours (LLVM.IBrCond _ _ label1 label2) = [label1, label2]
    instrNeighbours _ = []
    neighbours :: LLVM.Label -> [LLVM.Label]
    neighbours = instrNeighbours . LLVM.bExitInstr . getBlock
    getBlock :: LLVM.Label -> LLVM.Block
    getBlock label = head $
      filter (\ LLVM.Block { LLVM.bLabel = label' } -> (label == label'))
        blocks
    startingLabel :: LLVM.Label
    startingLabel = LLVM.bLabel (head blocks)
    follow :: S.Set LLVM.Label -> S.Set LLVM.Label
    follow s = s `S.union` SE.concatMap (S.fromList . neighbours) s
    fix prev = let next = follow prev in
      if next == prev then next else fix next
    reachable :: S.Set LLVM.Label
    reachable = fix (S.singleton startingLabel)

hasUnreachableInstruction :: LLVM.Function -> Bool
hasUnreachableInstruction LLVM.Function { LLVM.fBlocks = blocks } =
  any (\ block -> LLVM.bExitInstr block == LLVM.IUnreachable) blocks

-- transforms conditional jumps to unconditional jumps
constantProp :: LLVM.Function -> LLVM.Function
constantProp = mapBlock (mapExit brCondToBr)
  where
    mapBlock go function@LLVM.Function { LLVM.fBlocks = blocks } =
       function { LLVM.fBlocks  = map go blocks }

    mapExit go block@LLVM.Block { LLVM.bExitInstr = ei } =
      block { LLVM.bExitInstr = go ei }

    brCondToBr instr@(LLVM.IBrCond _ val ltrue lfalse) = case val of
      LLVM.VTrue -> LLVM.IBr ltrue
      LLVM.VFalse -> LLVM.IBr lfalse
      _ -> instr
    brCondToBr instr = instr
