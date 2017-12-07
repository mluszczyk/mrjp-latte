module CompileLatte where

import qualified Data.Map as M
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified AbsLatte
import CompilerErr (CompilerErrorM, raiseCEUndefinedVariable, raiseCEUndefinedFunction)
import Control.Monad (foldM)

latteMain = [ "target triple = \"x86_64-apple-macosx10.13.0\""
            , "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1"
            , "define i32 @main() #0 {"
            , "  call i32 @lat_main()"
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

newtype Register = Register Int

data LLVMValue = VConst Integer
               | VRegister Register
               -- TODO: registers should be one and nested variant

data LLVMType = Ti32 | Tvoid deriving Eq
data LLVMInstr = ICall LLVMType String [(LLVMType, LLVMValue)] (Maybe Register)

newtype ValueMap = ValueMap (M.Map String LLVMValue)

data LLVMFunctionType = LLVMFunctionType [LLVMType] LLVMType
newtype Signatures = Signatures (M.Map String LLVMFunctionType)

newtype NextRegister = NextRegister Register

initNextRegister :: NextRegister
initNextRegister = NextRegister (Register 0)

setVariable :: String -> LLVMValue -> ValueMap -> ValueMap
setVariable name value (ValueMap valueMap) =
    ValueMap (M.insert name value valueMap)

lookupVariable :: String -> ValueMap -> CompilerErrorM LLVMValue
lookupVariable name (ValueMap valueMap) =
    maybe (raiseCEUndefinedVariable name 0 0) return (M.lookup name valueMap)

initValueMap :: ValueMap
initValueMap = ValueMap M.empty

collectSignatures :: [AbsLatte.TopDef] -> Signatures
collectSignatures topDefs = Signatures $ M.fromList pairs
  where
    pairs = ("printInt", LLVMFunctionType [Ti32] Tvoid) :
            map getSignaturePair topDefs
    getSignaturePair (AbsLatte.FnDef retType ident args _) =
      (compileFuncIdent ident,
       LLVMFunctionType
       (map (\ (AbsLatte.Arg type_ _) -> compileType type_) args)
       (compileType retType))

compileType :: AbsLatte.Type -> LLVMType
compileType AbsLatte.Int = Ti32
compileType AbsLatte.Void = Tvoid

compileLatte :: AbsLatte.Program -> CompilerErrorM String
compileLatte (AbsLatte.Program topDefs) =
   let signatures = collectSignatures topDefs in
   do funcLines <- mapM (compileFunc signatures) topDefs
      let allLines = concat funcLines
      return $ unlines $ latteMain ++ allLines

compileFunc :: Signatures -> AbsLatte.TopDef -> CompilerErrorM [String]
compileFunc signatures (AbsLatte.FnDef type_ ident args (AbsLatte.Block block)) =
   do (blockLines, _, _) <- foldM go ([], initValueMap, initNextRegister) block
      return $ ["define " ++ showType type_ ++
                " @" ++ compileFuncIdent ident ++ "() {"] ++
                map indent blockLines ++
                [indent "ret void" | type_ == AbsLatte.Void] ++
                ["}"]

  where go (sLines, valueMap, nextRegister) statement =
          do (newLines, newValueMap, newNextRegister) <- compileStmt signatures statement valueMap nextRegister
             return (sLines ++ newLines, newValueMap, newNextRegister)

compileStmt :: Signatures -> AbsLatte.Stmt -> ValueMap -> NextRegister -> CompilerErrorM ([String], ValueMap, NextRegister)
compileStmt signatures (AbsLatte.SExp expr) valueMap nextReg =
   do (_, stmts, newNextReg) <- compileExpr signatures expr valueMap nextReg
      return (stmts, valueMap, newNextReg)

compileStmt signatures (AbsLatte.Ass ident expr) valueMap nextReg =
   do (value, stmts, newNextReg) <- compileExpr signatures expr valueMap nextReg
      let valueMap1 = setVariable (compileVariableIdent ident) value valueMap
      return (stmts, valueMap1, newNextReg)

compileStmt signatures (AbsLatte.Decl _ [AbsLatte.Init ident expr]) valueMap nextReg =
  compileStmt signatures (AbsLatte.Ass ident expr) valueMap nextReg

compileStmt signatures (AbsLatte.Ret expr) valueMap nextReg =
  do (value, stmts, newNextReg) <- compileExpr signatures expr valueMap nextReg
     return (stmts ++ ["ret i32 " ++ showValue value], valueMap, newNextReg)

compileExpr :: Signatures -> AbsLatte.Expr -> ValueMap -> NextRegister -> CompilerErrorM (LLVMValue, [String], NextRegister)
compileExpr signatures (AbsLatte.EApp ident args) valueMap nextReg =
   do (sLines, argVals, nextReg1) <- foldM go ([], [], nextReg) args
      retType <- getReturnedType (compileFuncIdent ident) signatures
      if retType == Tvoid then
        return (VConst 0, sLines ++ [showLLVMInst $ ICall retType (compileFuncIdent ident) [(Ti32, value) | value <- argVals] Nothing] , nextReg1)
      else do
        let (register, nextReg2) = getNextRegister nextReg1
        return (VRegister register, sLines ++ [showLLVMInst $ ICall retType (compileFuncIdent ident) [(Ti32, value) | value <- argVals] (Just register)], nextReg2)

   where
     go (sLines, argValues, nextReg1) argExpr =
           do (val, newLines, nextReg2) <- compileExpr signatures argExpr valueMap nextReg1
              return (sLines ++ newLines, argValues ++ [val], nextReg2)

compileExpr _ (AbsLatte.ELitInt int) valueMap nextReg =
    return (VConst int, [], nextReg)

compileExpr _ (AbsLatte.EVar ident) valueMap nextReg =
  do value <- lookupVariable (compileVariableIdent ident) valueMap
     return (value, [], nextReg)

compileFuncIdent (AbsLatte.Ident str) | str == "printInt" = "printInt"
                                      | otherwise = "lat_" ++ str

compileVariableIdent (AbsLatte.Ident str) = str

showValue :: LLVMValue -> String
showValue (VConst num) = show num
showValue (VRegister reg) = showRegister reg

showRegister :: Register -> String
showRegister (Register num) =  "%unnamed_" ++ show num

showType :: AbsLatte.Type -> String
showType AbsLatte.Int = "i32"
showType AbsLatte.Void = "void"

indent :: String -> String
indent = ("  " ++)

showLLVMType :: LLVMType -> String
showLLVMType Tvoid = "void"
showLLVMType Ti32 = "i32"

showLLVMInst :: LLVMInstr -> String
showLLVMInst (ICall retType ident args Nothing) = showCall retType ident args
showLLVMInst (ICall retType ident args (Just register)) = showRegister register ++ " = " ++ showCall retType ident args
showCall retType ident args =
  "call " ++ showLLVMType retType ++ " @" ++ ident ++ " (" ++
  intercalate ", " (map showArgPair args) ++
  ")"

  where showArgPair (type_, value) =
          showLLVMType type_ ++ " " ++ showValue value


getReturnedType :: String -> Signatures -> CompilerErrorM LLVMType
getReturnedType string (Signatures signatures) =
  maybe (raiseCEUndefinedFunction string 0 0)
  (\ (LLVMFunctionType _ retType) -> return retType )
  (M.lookup string signatures)

getNextRegister :: NextRegister -> (Register, NextRegister)
getNextRegister (NextRegister (Register num)) = (Register num, NextRegister (Register (num + 1)))
