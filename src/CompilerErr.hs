{-# OPTIONS_GHC -Wall -Werror #-}

module CompilerErr where

import qualified AbsLatte
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
                   | CEIncorrectMainFunctionType
                   | CETypeError String
                   | CERedefinitionOfVariable VariableIdent
                   | CEVoidFunctionArgument { cePosition :: Position
                                            , ceArgumentNumber :: Int }
                   | CEVoidDeclaration Position
                   | CEWrongNumberOfFunctionArguments { cePosition :: Position
                                                      , ceExpectedArgs :: Int
                                                      , ceGotIntArgs :: Int
                                                      , ceFunction :: FunctionIdent }


type CompilerErrorM = Either CompilerError


raise :: CompilerError -> CompilerErrorM a
raise = Left

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
errorToString CEIncorrectMainFunctionType = "incorrect type of main function; should return int and take no arguments"
errorToString (CETypeError description) = "type error in " ++ description
errorToString (CERedefinitionOfVariable ident) =
  "variable " ++ showVariableIdent ident ++ " redefined"
errorToString (CEVoidDeclaration position) =
  "void variable declaration " ++ showPosition position
errorToString (CEVoidFunctionArgument position argNum) =
  "void expression passed to a function as argument " ++ show argNum ++ " " ++
  showPosition position
errorToString (CEWrongNumberOfFunctionArguments position expArgs gotArgs ident) =
  "wrong number of arguments to function " ++ showFunctionIdent ident ++
  " " ++ showPosition position ++
  ", expected " ++ show expArgs ++ ", got " ++ show gotArgs

showVariableIdent :: VariableIdent -> String
showVariableIdent (AbsLatte.CIdent string) = string

showFunctionIdent :: FunctionIdent -> String
showFunctionIdent (AbsLatte.CIdent string) = string
