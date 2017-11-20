module CompileLatte where

import qualified Data.Map as M
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified AbsLatte
import CompilerErr (CompilerErrorM, raiseCEUndefinedVariable)
import Control.Monad (foldM)

latteMain = [ "target triple = \"x86_64-apple-macosx10.13.0\""
            , "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1"
            , "define i32 @main() #0 {"
            , "  call void @lat_main()"
            , "  ret i32 0"
            , "}"
            , ""
            , "define void @printInt(i32 %num) #1 {"
            , "  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %num)"
            , "  ret void"
            , "}"
            , "declare i32 @printf(i8*, ...) #2"
            ]

-- newtype LLVMIdent = LLVMIdent String
-- data LLVMFunc = LLVMFunc { funcName :: LLVMIdent
--                          , funcType :: LLVMType }

newtype LLVMValue = LLVMConst Integer

newtype ValueMap = ValueMap (M.Map String LLVMValue)

setVariable :: String -> LLVMValue -> ValueMap -> ValueMap
setVariable name value (ValueMap valueMap) =
    ValueMap (M.insert name value valueMap)

lookupVariable :: String -> ValueMap -> CompilerErrorM LLVMValue
lookupVariable name (ValueMap valueMap) =
    maybe (raiseCEUndefinedVariable name 0 0) return (M.lookup name valueMap)

initValueMap :: ValueMap
initValueMap = ValueMap M.empty

compileLatte :: AbsLatte.Program -> CompilerErrorM String
compileLatte (AbsLatte.Program topDefs) =
   do funcLines <- mapM compileFunc topDefs
      let allLines = concat funcLines
      return $ unlines $ latteMain ++ allLines

compileFunc :: AbsLatte.TopDef -> CompilerErrorM [String]
compileFunc (AbsLatte.FnDef type_ ident args (AbsLatte.Block block)) =
   do (blockLines, _) <- foldM go ([], initValueMap) block
      return $ ["define void @" ++ compileFuncIdent ident ++ "() {"] ++ blockLines ++ ["ret void", "}"]

  where go (sLines, valueMap) statement =
          do (newLines, newValueMap) <- compileStmt statement valueMap
             return (sLines ++ newLines, newValueMap)

compileStmt :: AbsLatte.Stmt -> ValueMap -> CompilerErrorM ([String], ValueMap)
compileStmt (AbsLatte.SExp expr) valueMap =
   do (_, stmts) <- compileExpr expr valueMap
      return (stmts, valueMap)

compileStmt (AbsLatte.Ass ident expr) valueMap =
   do (value, stmts) <- compileExpr expr valueMap
      let valueMap1 = setVariable (compileVariableIdent ident) value valueMap
      return (stmts, valueMap1)

compileStmt (AbsLatte.Decl _ [AbsLatte.Init ident expr]) valueMap =
  compileStmt (AbsLatte.Ass ident expr) valueMap

compileExpr :: AbsLatte.Expr -> ValueMap -> CompilerErrorM (LLVMValue, [String])
compileExpr (AbsLatte.EApp ident args) valueMap =
   do (sLines, argVals) <- foldM go ([], []) args
      return (LLVMConst 0, sLines ++ callInstr (compileFuncIdent ident) argVals)

   where
     go (sLines, argValues) argExpr =
           do (val, newLines) <- compileExpr argExpr valueMap
              return (sLines ++ newLines, argValues ++ [val])
     callInstr name args =
           ["call void @" ++ name ++ "(" ++ intercalate "," (map showArg args) ++  ")"]
     showArg (LLVMConst num) = "i32 " ++ show num

compileExpr (AbsLatte.ELitInt int) valueMap =
    return (LLVMConst int, [])

compileExpr (AbsLatte.EVar ident) valueMap =
  do value <- lookupVariable (compileVariableIdent ident) valueMap
     return (value, [])

compileFuncIdent (AbsLatte.Ident str) | str == "printInt" = "printInt"
                                      | otherwise = "lat_" ++ str

compileVariableIdent (AbsLatte.Ident str) = str
