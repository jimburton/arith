module Arith.Types

  where

data Exp = Val Int
  | Id String
  | Plus Exp Exp
  | Minus Exp Exp
  | Mult Exp Exp
  | Div Exp Exp deriving (Show, Eq)

type Env = [(String, Int)]
