module CompileLatte where

import Data.List (intercalate)
import qualified AbsLatte
import CompilerErr (CompilerErrorM)


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

compileLatte :: AbsLatte.Program -> CompilerErrorM String
compileLatte (AbsLatte.Program topDefs) =
   do funcLines <- mapM compileFunc topDefs
      let allLines = concat funcLines
      return $ unlines $ latteMain ++ allLines

compileFunc :: AbsLatte.TopDef -> CompilerErrorM [String]
compileFunc (AbsLatte.FnDef type_ ident args (AbsLatte.Block block)) =
   do blockLines <- mapM compileStmt block
      return $ ["define void @" ++ compileIdent ident ++ "() {"] ++ concat blockLines ++ ["ret void", "}"]

compileStmt :: AbsLatte.Stmt -> CompilerErrorM [String]
compileStmt (AbsLatte.SExp expr) =
   do (_, stmts) <- compileExpr expr
      return stmts

compileExpr :: AbsLatte.Expr -> CompilerErrorM (LLVMValue, [String])
compileExpr (AbsLatte.EApp ident args) =
   do exprRes <- mapM compileExpr args
      let statements = concatMap snd exprRes
      let args = map fst exprRes
      return (LLVMConst 0, statements ++ callInstr (compileIdent ident) args)

   where
     callInstr name args =
           ["call void @" ++ name ++ "(" ++ intercalate "," (map showArg args) ++  ")"]
     showArg (LLVMConst num) = "i32 " ++ show num

compileExpr (AbsLatte.ELitInt int) =
    return (LLVMConst int, [])

compileIdent (AbsLatte.Ident str) | str == "printInt" = "printInt"
                                  | otherwise = "lat_" ++ str
