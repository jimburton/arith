module Arith.Parse
  where

import Text.Parsec.String      ( Parser )
import Text.Parsec.Char        ( oneOf
                               , char
                               , digit
                               , satisfy
                               , letter )
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)

import Arith.Types

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

parseParen = do
  void $ lexeme $ char '('
  e <- parseExp
  void $ lexeme $ char ')'
  return e

parseVal :: Parser Exp
parseVal = do
  i <- many1 digit
  return (Val $ read i)

parseId :: Parser Exp
parseId = do
  str <- many1 letter
  return (Id str)

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

parseFactor :: Parser Exp
parseFactor = parseVal <|> parseParen <|> parseId

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
