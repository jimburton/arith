{-|
Module:   Main

Entry point for the arith package

Run it from cabal like this:

arith$ cabal run arith "2+2"
4

or if you want to use identifiers, supply an enironment to look them up in:

arith$ cabal run arith -- "2+x" "[(\"x\",2)]"
4

-}
module Main where

import Text.Parsec        ( parse )
import System.Environment ( getArgs )

import Arith.Parse        ( parseExp )
import Arith.Eval         ( eval )

usage :: String
usage = "enter an arithmetic expression in quotes followed by an optional environment in quotes. \n \
        \ E.g. \n \
        \ > arith \"2+x\" \"[(\\\"x\\\",2)]\" \n \
        \ 4 "
        
main :: IO ()
main = do args <- getArgs
          if length args == 0
            then putStrLn usage
            else case parse parseExp "arithmetic" (head args) of
                   Left e -> print e
                   Right x -> do let env = if length args > 1 then (read (head (drop 1 args))) else []
                                 case eval x env of
                                   Nothing -> putStrLn "error"
                                   (Just i) -> print i

                          
