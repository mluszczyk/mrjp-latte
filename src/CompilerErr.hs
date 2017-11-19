module CompilerErr where

data CompilerError = CEUndefinedVariable { ceIdent :: String
                                         , ceLine :: Int
                                         , ceColumn :: Int }

type CompilerErrorM a = Either CompilerError a

raiseCEUndefinedVariable :: String -> Int -> Int -> CompilerErrorM a
raiseCEUndefinedVariable ident line column = Left CEUndefinedVariable { ceIdent = ident
                                                                      , ceLine = line
                                                                      , ceColumn = column }

errorToString :: CompilerError -> String
errorToString CEUndefinedVariable { ceIdent = ident
                                  , ceLine = line
                                  , ceColumn = column }
 = "undefined variable " ++ ident ++ " on line " ++ show line ++ " column " ++ show column
