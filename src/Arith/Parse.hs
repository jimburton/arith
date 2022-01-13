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

-- |Run a parse then throw away whitespace up to the next token
lexeme :: Parser a -> Parser a
lexeme p = p >>= \x -> whitespace >> pure x

-- |Parse balanced parentheses
parseParen :: Parser Exp
parseParen = lexeme $ char '(' >> parseExp
             >>= \e -> lexeme $ char ')' >> pure e

-- |Parse a number
parseVal :: Parser Exp
parseVal = Val . read <$> many1 digit

-- |Parse an identifier
parseId :: Parser Exp
parseId = Id <$> many1 letter

-- |Parse a term
parseTerm :: Parser Exp
parseTerm = parseFactor >>= loop
  where factorSuffix f1 = do
          op <- lexeme $ oneOf "*/"
          f2 <- parseFactor
          case op of
            '*' -> loop (Mult f1 f2)
            '/' -> loop (Div f1 f2)
        loop t = factorSuffix t <|> pure t

-- |Parse a factor
parseFactor :: Parser Exp
parseFactor = parseVal <|> parseId <|> parseParen

-- |Parse an expression
parseExp :: Parser Exp
parseExp = parseTerm >>= loop
  where termSuffix t1 = do
          op <- lexeme $ oneOf "+-"
          t2 <- parseTerm
          case op of
            '+' -> loop (Plus t1 t2)
            '-' -> loop (Minus t1 t2)
        loop t = termSuffix t <|> pure t
