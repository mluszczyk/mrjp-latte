{-# OPTIONS_GHC -Wall -Werror #-}

{- Command line interface to the compiler. Based on the BNFC parser. -}
module Main where

import System.IO ( hPutStrLn, stderr, hPrint )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import LexLatte
import ParLatte
import AbsLatte
import ErrM

import CompileLatte (compileLatte, Position, compileLatteToX86_64)
import CompilerErr (errorToString)

type ParseFun a = [Token] -> Err a

data Backend = LLVM | X86_64

run :: ParseFun (Program Position) -> Backend -> String -> IO ()
run p backend s =
   let ts = myLexer s in case p ts of
        Bad descr ->  do hPutStrLn stderr "\nParse              Failed...\n"
                         hPutStrLn stderr "Tokens:"
                         hPrint stderr ts
                         hPutStrLn stderr descr
                         exitFailure
        Ok  tree ->  case getBackend backend tree of
          Left ce      -> do
                            hPutStrLn stderr (errorToString ce)
                            exitFailure
          Right output -> do
                            putStr output
                            exitSuccess
  where
    getBackend LLVM = compileLatte
    getBackend X86_64 = compileLatteToX86_64

usage :: IO ()
usage = do
  hPutStrLn stderr $ unlines
    [ "usage: Call with one of the following arguments:"
    , "  --help          Display this help message."
    , "                  Compile stdin."
    , "  file            Compile content of file."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [ "--x86_64" ] -> getContents >>= run pProgram X86_64
    [ "--x86_64", file ] -> readFile file >>= run pProgram X86_64
    [] -> getContents >>= run pProgram LLVM
    [file] -> readFile file >>= run pProgram LLVM
    _ -> usage
