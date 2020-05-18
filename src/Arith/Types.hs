{-|
Module:    Arith.Types

The types for the abstract syntax tree etc.
-}
module Arith.Types ( Exp(..)
                   , Env )
  where

data Exp = Val Int
  | Id String
  | Plus Exp Exp
  | Minus Exp Exp
  | Mult Exp Exp
  | Div Exp Exp deriving (Show, Eq)

type Env = [(String, Int)]
