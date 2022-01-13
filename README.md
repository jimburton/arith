# arith

A simple demo of building a parser with combinators. The parser consumes
and evaluates arithmetic expressions, for example:

```
$ arith "2+(4/(5-4))"
6
$ arith "2+(4/(5-4)"
"arithmetic" (line 1, column 11):
unexpected end of input
expecting ")"
```

You can also supply an *environment* or lookup table of identifiers and the
values assigned to them:

```
$ arith "2+(4/(5-4))+foo-x" "[(\"foo\",1),(\"x\",9)]"
-2
```

Note that you need to escape the quotation marks around identifier names with a backslash.

The parser uses the
[Parsec](https://hackage.haskell.org/package/parsec) library. When
building parsers with combinators, we start with tiny parsers capable
of, for instance, parsing a single character or the repetition of a
character, then combine these in various ways to produce more
sophisticated parsers. Parsec provides atomic parsers like `digit` and
`letter`, then ways of repeating and combining them, like `many` and `(<|>)`. 
