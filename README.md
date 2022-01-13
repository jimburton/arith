# arith

A simple demo of building a parser with combinators. The parser consumes
and evaluates arithmetic expressions. If you enter a badly formed expression
it will tell you what went wrong. For example:

```
$ cabal run arith "2+(4/(5-4))"
6
$ cabal run arith "2+(4/(5-4)"
"arithmetic" (line 1, column 11):
unexpected end of input
expecting ")"
```

You can also supply an *environment* or lookup table of identifiers and the
values assigned to them:

```
$ cabal run arith "2+(4/(5-4))+foo-x" "[(\"foo\",1),(\"x\",9)]"
-2
```

Note that you need to escape the quotation marks around identifier names with a backslash.

The parser uses the
[Parsec](https://hackage.haskell.org/package/parsec) library. When
building parsers with combinators, we start with tiny parsers capable
of, for instance, parsing a single character or the repetition of a
character, then combine these in various ways to produce more
sophisticated parsers. Parsec provides atomic parsers like `digit` and
`letter`, then ways of repeating and combining them, like `many`
(takes a parser and applies it zero to many times), `many1` (apply a
parser one or more times) and `(<|>)` (take two parsers and apply the
first one then, if the first one fails, apply the second one).

We want to parse strings, such as "2 + x", into our data type for
expressions. An expression is either a value (e.g. 2), an identifier
(e.g. "x") or the addition, subtraction, multiplication or division of
two expressions:

```haskell
-- in Arith.Types

data Exp = Val Int
  | Id String
  | Plus Exp Exp
  | Minus Exp Exp
  | Mult Exp Exp
  | Div Exp Exp deriving (Show, Eq)
```

We can create a parser that will consume an identifier and produce the right kind 
of `Exp` value using `many1` and `letter`:

```haskell
-- in Arith.Parse

-- |Parse an identifier
parseId :: Parser Exp
parseId = Id <$> many1 letter
```

Note that `parseId` is using applicative style to "lift" the `Id`
constructor, which takes a `String`, into the `Parser` context then
tries to run the parser `many1 letter` to produce a `String`. You could
equally well write it with do-notation like so:

```haskell
parseId = do str <- many1 letter
             pure (Id str)
```

Now we can run this parser using the `parse` function from
`Parsec`. It will return `Either ParseError Exp`. That is, if all goes
well, it will return an `Exp` value in the `Left` constructor, like
`Left (Id "x")`, and if anything goes wrong it will return `Right e`,
where `e` tells us what went wrong. The `parse` function takes three
arguments: the parser, a string describing the parser and the string
to be parsed. We can play with it in `ghci` like so:

```haskell
$ cabal repl
ghci> :l Arith.Parse
ghci> :m + Text.Parsec
ghci> parse parseId "parsing an identifier" "x"
Right (Id "x")
ghci> parse parseId "parsing an identifier" "5"
Left "parsing an identifier" (line 1, column 1):
unexpected "5"
expecting letter
```

After creating similar parsers for the various things that can make up
an arithmetic expression, we can think about parsing complex
expressions made up of several parts. So that our expressions are
unambiguous without the need for lots of brackets, we introduce a
distinction between *terms* (expressions that are operands to a
multiplication or division) and *factors* (expressions that are
operands to an addition or subtraction).  This allows us to say that
we want terms to "bind" more tightly than factors, following the
normal convention that `2+3*2` is equal to 8 rather than 10.

A factor is a value or an identifier or an expression inside brackets:

```haskell
-- |Parse a factor
parseFactor :: Parser Exp
parseFactor = parseVal <|> parseId <|> parseParen
```

To parse a term expression, we get the factor on the left, decide
which operator we're looking at, get the factor on the right then 
return a `Mul` or `Div` expression as required:

```haskell
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
```

Note that the alternation in `loop` means that if the `factorSuffix`
parser doesn't match we just return the expression as is. So calling
`parseTerm` on an expression that doesn't contain any terms, like `2+2` 
or `5`, just returns the original expression. 

To parse an expression that might contains terms and factors, we
first try to parse terms then try to parse factors. 

```haskell
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
```

Finally, we can hook this up in the `main` method to read expressions
from the command line. The important part is the case expression that 
runs the `parse` function and unpacks the result. We are expecting `args`,
the list of strings supplied as command line arguments, to have at least one
thing in it, which is the expression to parse. If there is more than one 
thing then the second one is assumed to be an environment, which has to be 
valid Haskell syntax for a list of `(String, Int)` pairs.

```haskell
case parse parseExp "arithmetic" (head args) of
  Left e -> print e
  Right x -> do let env = if length args > 1
                          then read (args !! 1)
                          else []
                case eval x env of
                    Nothing  -> putStrLn "error"
                    (Just i) -> print i
```

