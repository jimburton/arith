module Arith.Eval

  where

import Arith.Types

eval :: Exp -> Env -> Maybe Int
eval (Val v)       env = Just v
eval (Id s)        env = lookup s env
eval (Plus e1 e2)  env = (+) <$> (eval e1 env) <*> (eval e2 env)
eval (Minus e1 e2) env = (-) <$> (eval e1 env) <*> (eval e2 env)
eval (Mult e1 e2)  env = (*) <$> (eval e1 env) <*> (eval e2 env)
eval (Div e1 e2)   env = div <$> (eval e1 env) <*> (eval e2 env)
