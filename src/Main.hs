{-|
Module:   Main

Entry point for the arith package
-}
module Main where

import Text.Parsec        ( parse )
import System.Environment ( getArgs )

import Arith.Parse        ( parseExp )
import Arith.Eval         ( eval )

main :: IO ()
main = do (e:env:_) <- getArgs
          case parse parseExp "arithmetic" e of
            Left e -> print e
            Right x -> case eval x (read env) of
              Nothing -> putStrLn "error"
              (Just i) -> print i
                          
