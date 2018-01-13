{-# OPTIONS_GHC -Wall -Werror #-}

module LatteCommon where

data Operation = Add | Sub | Mul | Div | Mod
                 | LessThan | LessEqual
                 | GreaterThan | GreaterEqual | Equal | NotEqual
                 deriving Eq

data Type = Boolean | Int | Void | String | Array Type
            deriving Eq

data FunctionType = FunctionType [Type] Type
                    deriving Eq
