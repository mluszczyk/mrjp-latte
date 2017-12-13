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


type CompilerErrorM = Either CompilerError

raiseCEUndefinedVariable :: VariableIdent -> Position -> CompilerErrorM a
raiseCEUndefinedVariable ident position = Left CEUndefinedVariable { ceVariableIdent = ident
                                                                   , cePosition = position }

raiseCEUndefinedFunction :: FunctionIdent -> Position -> CompilerErrorM a
raiseCEUndefinedFunction ident position = Left CEUndefinedFunction { ceFunctionIdent = ident
                                                                   , cePosition = position }

raiseCEDuplicatedFunctionDeclaration :: FunctionIdent -> Position -> CompilerErrorM a
raiseCEDuplicatedFunctionDeclaration ident position = Left CEDuplicatedFunctionDeclaration { ceFunctionIdent = ident
                                                                                           , cePosition = position }

raiseCEMissingMainFunction :: CompilerErrorM a
raiseCEMissingMainFunction = Left CEMissingMainFunction

raiseCEIncorrectMainFunctionType :: CompilerErrorM a
raiseCEIncorrectMainFunctionType = Left CEIncorrectMainFunctionType

raiseCETypeError :: String -> CompilerErrorM a
raiseCETypeError string = Left $ CETypeError string

raiseCERedefinitionOfVariable :: VariableIdent -> CompilerErrorM a
raiseCERedefinitionOfVariable ident = Left $ CERedefinitionOfVariable ident

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

showVariableIdent :: VariableIdent -> String
showVariableIdent (AbsLatte.CIdent string) = string

showFunctionIdent :: FunctionIdent -> String
showFunctionIdent (AbsLatte.CIdent string) = string
