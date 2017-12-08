module CompilerErr where

data Position = Position { row :: Int
                         , column :: Int }

data CompilerError = CEUndefinedVariable { ceIdent :: String
                                         , cePosition :: Position }
                   | CEUndefinedFunction { ceIdent :: String
                                         , cePosition :: Position }


type CompilerErrorM a = Either CompilerError a

raiseCEUndefinedVariable :: String -> Position -> CompilerErrorM a
raiseCEUndefinedVariable ident position = Left CEUndefinedVariable { ceIdent = ident
                                                                   , cePosition = position }

raiseCEUndefinedFunction :: String -> Position -> CompilerErrorM a
raiseCEUndefinedFunction ident position = Left CEUndefinedFunction { ceIdent = ident
                                                                   , cePosition = position }


showPosition :: Position -> String
showPosition position = "on line " ++ show (row position) ++ " column " ++ show (column position)

errorToString :: CompilerError -> String
errorToString CEUndefinedVariable { ceIdent = ident
                                  , cePosition = position }
 = "undefined variable " ++ ident ++ " " ++ showPosition position

errorToString CEUndefinedFunction { ceIdent = ident
                                   , cePosition = position }
  = "undefined function " ++ ident ++ " " ++ showPosition position
