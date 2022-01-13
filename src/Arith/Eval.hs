{-|
Module:    Arith.Eval

Evaluating ASTs
-}
module Arith.Eval ( eval )

  where

import Arith.Types

{-| Evaluate an expression.

Note the definition of `eval' for the operators, e.g.

eval (Plus e1 e2)  env = (+) <$> eval e1 env <*> eval e2 env

This is using the fact that Maybe is an Applicative. The "simplest"
but least neat way to write it would be by using case statements to
unpack the result of each inner call to `eval':

eval (Plus e1 e2)  env = case eval e1 env of
                           Nothing  -> Nothing
                           (Just i) -> case eval e2 env of
                                        Nothing  -> Nothing
                                        (Just j) -> Just (i+j)

Since we know Maybe is a monad we can neaten this up considerably do notation:

eval (Plus e1 e2)  env = do i <- eval e1 env
                            j <- eval e2 env
                            pure (i+j)

Or (>>=) ("bind"):

eval (Plus e1 e2)  env = eval e1 env >>= \i -> eval e2 env >>= \j -> pure (i+j)

In the last expression we have used `pure' to "lift" the addition function into the Maybe
context and performed two monadic actions to get the arguments to (+). This is exactly
what Applicative allows us to do.

-}
eval :: Exp -> Env -> Maybe Int
eval (Val v)       _    = Just v
eval (Id s)        env = lookup s env
eval (Plus e1 e2)  env = (+) <$> eval e1 env <*> eval e2 env
eval (Minus e1 e2) env = (-) <$> eval e1 env <*> eval e2 env
eval (Mult e1 e2)  env = (*) <$> eval e1 env <*> eval e2 env
eval (Div e1 e2)   env = div <$> eval e1 env <*> eval e2 env
