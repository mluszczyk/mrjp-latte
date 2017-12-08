module CompilerErr where

data Position = Position { row :: Int
                         , column :: Int }
builtinPosition = Position { row = -1 , column = -1 }

data CompilerError = CEUndefinedVariable { ceIdent :: String
                                         , cePosition :: Position }
                   | CEUndefinedFunction { ceIdent :: String
                                         , cePosition :: Position }
                   | CEDuplicatedFunctionDeclaration { ceIdent :: String
                                                     , cePosition :: Position }
                   | CEMissingMainFunction
                   | CEIncorrectMainFunctionType
                   | CETypeError String


type CompilerErrorM a = Either CompilerError a

raiseCEUndefinedVariable :: String -> Position -> CompilerErrorM a
raiseCEUndefinedVariable ident position = Left CEUndefinedVariable { ceIdent = ident
                                                                   , cePosition = position }

raiseCEUndefinedFunction :: String -> Position -> CompilerErrorM a
raiseCEUndefinedFunction ident position = Left CEUndefinedFunction { ceIdent = ident
                                                                   , cePosition = position }

raiseCEDuplicatedFunctionDeclaration :: String -> Position -> CompilerErrorM a
raiseCEDuplicatedFunctionDeclaration ident position = Left CEDuplicatedFunctionDeclaration { ceIdent = ident
                                                                                           , cePosition = position }

raiseCEMissingMainFunction :: CompilerErrorM a
raiseCEMissingMainFunction = Left CEMissingMainFunction

raiseCEIncorrectMainFunctionType :: CompilerErrorM a
raiseCEIncorrectMainFunctionType = Left CEIncorrectMainFunctionType

raiseCETypeError :: String -> CompilerErrorM a
raiseCETypeError string = Left $ CETypeError string

showPosition :: Position -> String
showPosition position = "on line " ++ show (row position) ++ " column " ++ show (column position)

errorToString :: CompilerError -> String
errorToString CEUndefinedVariable { ceIdent = ident
                                  , cePosition = position }
 = "undefined variable " ++ ident ++ " " ++ showPosition position

errorToString CEUndefinedFunction { ceIdent = ident
                                   , cePosition = position }
  = "undefined function " ++ ident ++ " " ++ showPosition position

errorToString CEDuplicatedFunctionDeclaration { ceIdent = ident
                                              , cePosition = position }
  = "duplicated declaration of function " ++ ident ++ " " ++ showPosition position

errorToString CEMissingMainFunction = "main function not declared"
errorToString CEIncorrectMainFunctionType = "incorrect type of main function; should return int and take no arguments"
errorToString (CETypeError description) = "type error in " ++ description
