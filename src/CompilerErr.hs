{-# OPTIONS_GHC -Wall -Werror #-}

-- listing of possible compiler errors

module CompilerErr where

import qualified AbsLatte
import qualified LLVM

import qualified LatteCommon

type Type = LLVM.Type
type VariableIdent = AbsLatte.CIdent
type FunctionIdent = AbsLatte.CIdent

data Position = Position { row :: Int
                         , column :: Int }
builtinPosition :: Position
builtinPosition = Position { row = -1 , column = -1 }

data CompilerError = CEUndefinedVariable { ceVariableIdent :: VariableIdent
                                         , cePosition :: Position }
                   | CEUndefinedFunction { ceFunctionIdent :: FunctionIdent
                                         , cePosition :: Position }
                   | CEDuplicatedFunctionDeclaration { ceFunctionIdent :: FunctionIdent
                                                     , cePosition :: Position }
                   | CEMissingMainFunction
                   | CEIncorrectMainFunctionType Position
                   | CETypeError { cePosition :: Position
                                 , ceExpectedType :: Type
                                 , ceActualType :: Type
                                 , ceDescription :: String }
                   | CERedefinitionOfVariable { cePosition :: Position
                                              , ceVariableIdent :: VariableIdent }
                   | CEVoidFunctionArgument { cePosition :: Position
                                            , ceArgumentNumber :: Int }
                   | CEVoidDeclaration Position
                   | CEWrongNumberOfFunctionArguments { cePosition :: Position
                                                      , ceExpectedArgs :: Int
                                                      , ceGotIntArgs :: Int
                                                      , ceFunction :: FunctionIdent }
                   | CEInvalidBinaryOp { cePosition :: Position
                                       , ceType1 :: Type
                                       , ceOperation :: LatteCommon.Operation
                                       , ceType2 :: Type }
                   | CEMissingReturn { ceFunctionIdent :: FunctionIdent
                                     , cePosition :: Position }
                   | CEIntLiteralOutOfBounds { cePosition :: Position
                                             , ceIntLiteral :: Integer
                                             , ceLowerBound :: Integer
                                             , ceUpperBound :: Integer }
                   | CEExprReturnInVoid { cePosition :: Position }
                   | CEDivisionByZero

showPosition :: Position -> String
showPosition position = "on line " ++ show (row position) ++ " column " ++ show (column position)

errorToString :: CompilerError -> String
errorToString CEUndefinedVariable { ceVariableIdent = ident
                                  , cePosition = position }
 = "undefined variable " ++ showVariableIdent ident ++ " " ++ showPosition position

errorToString CEUndefinedFunction { ceFunctionIdent = ident
                                   , cePosition = position }
  = "undefined function " ++ showFunctionIdent ident ++ " " ++ showPosition position

errorToString CEDuplicatedFunctionDeclaration { ceFunctionIdent = ident
                                              , cePosition = position }
  = "duplicated declaration of function " ++ showFunctionIdent ident ++ " " ++ showPosition position

errorToString CEMissingMainFunction = "main function not declared"
errorToString (CEIncorrectMainFunctionType pos) =
  "incorrect type of main function " ++
  showPosition pos ++
  ", should return int and take no arguments"
errorToString (CETypeError position expType actType description) =
  "type error in " ++ description ++ " " ++ showPosition position ++
  ", expected " ++ showType expType ++ ", got " ++ showType actType
errorToString (CERedefinitionOfVariable position ident) =
  "variable " ++ showVariableIdent ident ++ " redefined " ++
  showPosition position
errorToString (CEVoidDeclaration position) =
  "void variable declaration " ++ showPosition position
errorToString (CEVoidFunctionArgument position argNum) =
  "void expression passed to a function as argument " ++ show argNum ++ " " ++
  showPosition position
errorToString (CEWrongNumberOfFunctionArguments position expArgs gotArgs ident) =
  "wrong number of arguments to function " ++ showFunctionIdent ident ++
  " " ++ showPosition position ++
  ", expected " ++ show expArgs ++ ", got " ++ show gotArgs
errorToString (CEInvalidBinaryOp position type1 op type2) =
  "invalid binary operation " ++
  showType type1 ++ showOperator op ++ showType type2 ++
  " " ++ showPosition position
errorToString (CEMissingReturn ident pos) =
  "missing return in function " ++ showFunctionIdent ident ++
  " returning non-void " ++ showPosition pos
errorToString (CEIntLiteralOutOfBounds pos lit lowerBound upperBound) =
  "int literal " ++ show lit ++ " out of bounds [" ++
  show lowerBound ++ ", " ++ show upperBound ++ "] " ++
  showPosition pos
errorToString (CEExprReturnInVoid pos) =
  "return with a value illegal in void function " ++ showPosition pos
errorToString CEDivisionByZero =
  "division by zero found during optimization"

showVariableIdent :: VariableIdent -> String
showVariableIdent (AbsLatte.CIdent string) = string

showFunctionIdent :: FunctionIdent -> String
showFunctionIdent (AbsLatte.CIdent string) = string

showType :: Type -> String
showType LLVM.Ti1 = "boolean"
showType LLVM.Ti32 = "int"
showType LLVM.Ti8Ptr = "string"
showType LLVM.Tvoid = "void"

showOperator :: LatteCommon.Operation -> String
showOperator LatteCommon.Mul = "*"
showOperator LatteCommon.Add = "+"
showOperator LatteCommon.Div = "/"
showOperator LatteCommon.Mod = "%"
showOperator LatteCommon.Sub = "-"
showOperator LatteCommon.Equal = "=="
showOperator LatteCommon.NotEqual = "!="
showOperator LatteCommon.LessThan = "<"
showOperator LatteCommon.GreaterThan = ">"
showOperator LatteCommon.LessEqual = "<="
showOperator LatteCommon.GreaterEqual = ">="
