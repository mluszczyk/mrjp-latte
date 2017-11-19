module CompileLatte where

import qualified AbsLatte
import CompilerErr (CompilerErrorM)

compileLatte :: AbsLatte.Program -> CompilerErrorM String
compileLatte _ = return "OK"
