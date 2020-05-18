{-|
Module Arith.Parse

Parsing strings to ASTs in the arithmentic language.
-}
module Arith.Parse ( parseExp )
  where

import Text.Parsec.String      ( Parser )
import Text.Parsec.Char        ( oneOf
                               , char
                               , digit
                               , satisfy
                               , letter )
import Text.Parsec.Combinator ( many1, choice, chainl1 )
import Control.Applicative    ( (<|>), many )
import Control.Monad (void)

import Arith.Types

-- |Parse whitespace
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- |Run a parse then through away whitespace up to the next token
lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

-- |Parse balanced parentheses
parseParen = do
  void $ lexeme $ char '('
  e <- parseExp
  void $ lexeme $ char ')'
  return e

-- |Parse a number
parseVal :: Parser Exp
parseVal = do
  i <- many1 digit
  return (Val $ read i)

-- |Parse an identifier
parseId :: Parser Exp
parseId = do
  str <- many1 letter
  return (Id str)

-- |Parse a term
parseTerm :: Parser Exp
parseTerm = do
  f1 <- parseFactor
  loop f1
  where factorSuffix f1 = do
          op <- lexeme $ oneOf "*/"
          f2 <- parseFactor
          case op of
            '*' -> loop (Mult f1 f2)
            '/' -> loop (Div f1 f2)
        loop t = factorSuffix t <|> return t

-- |Parse a factor
parseFactor :: Parser Exp
parseFactor = parseVal <|> parseParen <|> parseId

-- |Parse an expression
parseExp :: Parser Exp
parseExp = do
  t1 <- parseTerm
  loop t1
  where termSuffix t1 = do
          op <- lexeme $ oneOf "+-"
          t2 <- parseTerm
          case op of
            '+' -> loop (Plus t1 t2)
            '-' -> loop (Minus t1 t2)
        loop t = termSuffix t <|> return t
