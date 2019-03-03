---
title: Molecule to atoms
description: Parse chemical formula with parser combinators as well as Alex and Happy
tags: haskell, parsing, Alex, Happy
---

[Codewars](https://www.codewars.com) has a very interesting [problem](https://www.codewars.com/kata/molecule-to-atoms):

> For a given chemical formula represented by a string,
count the number of atoms of each element contained in the molecule and return an object
`Either String [(String, Int)]`.

For example, 

~~~~~{.haskell}
>>> parseMolecule "H2O" -- water
Right [("H",2),("O",1)]

>>> parseMolecule "Mg(OH)2" -- magnesium hydroxide
Right [("Mg",1),("O",2),("H",2)]

>>> parseMolecule "K4[ON(SO3)2]2" -- Fremy's salt
Right [("K",4),("O",14),("N",2),("S",4)]

>>> parseMolecule "pie"
Left "Not a valid molecule"
~~~~~

This is actually a parsing problem.
Traditionally, parsing is considered as[^dragon] the *front-end* phase of a compiler
which consists of two phases: lexical analysis and syntax analysis:
lexical analysis takes a string a input and outputs a list of tokens
(strings with an assigned and thus identified meaning);
syntax analysis then consumes list of tokens and builds a data structure.

[^dragon]: [https://en.wikipedia.org/wiki/Compilers:_Principles,_Techniques,_and_Tools](https://en.wikipedia.org/wiki/Compilers:_Principles,_Techniques,_and_Tools)

Haskell is particular suitable for building parsers.
Indeed, its [abstract data type](https://wiki.haskell.org/Abstract_data_type)
is just **the** right representation of abstract syntax tree (AST), a common data structure produced by syntax analysis.

A typical Haskell parser is created by either parser combinators or Alex and Happy.
A parser combinator is a higher-order function that accepts several parsers as input and
returns a new parser as its output.
It generally perform the lexical analysis and syntax analysis in one shot.
Many libraries in Haskell help to create parser combinators, such as

- [ReadP](https://hackage.haskell.org/package/base-4.9.0.0/docs/Text-ParserCombinators-ReadP.html) (built-in in base)
- [Parsec](https://hackage.haskell.org/package/parsec)
- [Attoparsec](https://hackage.haskell.org/package/parsec)
- [Megaparsec](https://hackage.haskell.org/package/megaparsec)
- [Trifecta](https://hackage.haskell.org/package/trifecta)

[Alex](https://hackage.haskell.org/package/alex) and [Happy](https://hackage.haskell.org/package/happy),
on the other hand, are responsible for lexical and syntax analysis respectively.
The are the Haskell equivalent of the famous
[Lex](https://en.wikipedia.org/wiki/Lex_(software)) and [Yacc](https://en.wikipedia.org/wiki/Yacc)
tool chain.

The molecule parsing problem is simple enough that we are going to explore the two approaches in this post.

For the rest of the post,
we are going to use the following helper functions

```haskell
import qualified Data.Map as M

-- Composition is a count of atoms
type Atom = String
type Composition = M.Map Atom Int

-- Multiply a composition by index
mul :: Index -> Composition -> Composition
mul One = id
mul (Mul n) = M.map (n *)

-- Merge two compositions
merge :: Composition -> Composition -> Composition
merge = M.unionWith (+)
```

# Approach with parser combinators

> TL; DR: See [here](https://github.com/BoeingX/codewars/blob/master/src/MoleculeToAtoms.hs)

In this post we use ReadP,
which is available by default in a standard GHC installation,
to build our parser.
One can easily port the implementation to more advanced tools like Parsec with very few modifications.

`ReadP` is defined in `Text.ParserCombinators.ReadP`.
It should be considered abstract and we should not worry about its implementation in most cases.
The `ReadS` type, defined in the same module, is actually what we could call a parser

```haskell
type ReadS a = String -> [(a, String)]
```

In plain words, a `ReadS` parser for type `a` takes a string
and returns a list[^hutton-meijer] of possible parses as tuples
where the first component is what is recognized as type `a`,
whereas the second component is whatever is left of the string that
the parser doesn't consume to produce the value of type `a`.

[^hutton-meijer]: This is called a Hutton-Meijer parser.
Modern Haskell parser combinators tend to use a less powerful (but more efficient)
version
```haskell
type Parser a = String -> Maybe (a, String)
```

There is a function `readP_to_S` turns a ReadP-style parser to a (ReadS-style) parser
```haskell
readP_to_S :: ReadP a -> ReadS a
```
This is the main way in which you can "run" a ReadP parser.

A bunch of parsers are defined in `Text.ParserCombinators.ReadP`[^common-parsers], such as

```haskell
-- Succeeds iff we are at the end of input
eof :: ReadP ()

-- Parses and returns the specified character.
char :: Char -> ReadP Char

-- Parses and returns the specified string.
string :: String -> ReadP String

-- Combines all parsers in the specified list.
choice :: [ReadP a] -> ReadP a 

-- option x p will either parse p or return x without consuming any input.
option :: a -> ReadP a -> ReadP a 

-- between open close p parses open, followed by p and finally close.
-- Only the value of p is returned.
between :: ReadP open -> ReadP close -> ReadP a -> ReadP a

-- Parses one or more occurrences of the given parser.
many1 :: ReadP a -> ReadP [a] 
```

[^common-parsers]: These parsers are actually standard and you can find them in most parser combinator libraries.

We can easily build complex parsers with these smaller ones.
In particular, these are all we need to parse our molecule to atoms problem.

## Attack the problem

The difficulties of our problems are
- indices are optional
- the presence of brackets makes the formula recursive by nature

With parser combinators, however,
`optional` and `between` make it very easy to solve the two problems.

First, we need some helper functions

```haskell
-- | Parse an integer
integer :: ReadP Int
integer = read <$> many1 (choice (map char "0123456789"))

atoms :: [Atom]
atoms = ["H","He","Li","Be","B","C","N","O","F","Ne","Na","Mg","Al","Si","P","S","Cl","Ar","K","Ca","Sc","Ti","V","Cr","Mn","Fe","Co","Ni","Cu","Zn","Ga","Ge","As","Se","Br","Kr","Rb","Sr","Y","Zr","Nb","Mo","Tc","Ru","Rh","Pd","Ag","Cd","In","Sn","Sb","Te","I","Xe","Cs","Ba","La","Ce","Pr","Nd","Pm","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu","Hf","Ta","W","Re","Os","Ir","Pt","Au","Hg","Tl","Pb","Bi","Po","At","Rn","Fr","Ra","Ac","Th","Pa","U","Np","Pu","Am","Cm","Bk","Cf","Es","Fm","Md","No","Lr","Rf","Db","Sg","Bh","Hs","Mt","Ds","Rg","Cn","Nh","Fl","Mc","Lv","Ts","Og"]
```

Then to parse a single atom, we can write

```haskell
parseAtom :: ReadP Atom
parseAtom = choice (map string atoms)
```

It is simple to add support for an optional index

```haskell
parseSingleton :: ReadP Composition
parseSingleton = do
    atom <- parseAtom -- read an atom
    index <- option 1 integer -- read an index (default to 1)
    return $ mul index (M.fromList [(atom, 1)])
```

Now we try to parse formula between brackets

```haskell
type OpenBracket  = Char
type CloseBracket = Char

-- | Parse molecule between an open and close bracket
parseBracket :: OpenBracket -> CloseBracket -> ReadP Composition
parseBracket open close = do
    subMolecule <- between (char open) (char close) parse
    index <- option 1 integer
    return $ mul index subMolecule

-- | Only parentheses, bracket and brace are valid
parseSubMolecule :: ReadP Composition
parseSubMolecule = choice $ map (uncurry parseBracket) [('(', ')'), ('[', ']'), ('{', '}')]
```

To parse a molecule formula to atoms composition,
we repeat one or more times (using `many1`)
`parseSingleton` or (using `+++`) `parseSubMolecule`,
which results a list of type `[Composition]`.
Then we use `merge :: Composition -> Composition`
to reduce them to a single `Composition`

```haskell
parse :: ReadP Composition
parse = foldr merge M.empty <$> many1 (parseSingleton +++ parseSubMolecule)
```

Finally, we wrap `parse` in a function with the required
type signature

```haskell
parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = case readP_to_S (parse <* eof) formula of
    [] -> Left "Not a valid molecule"
    [(x, "")] -> Right (M.toList x)
    _ -> undefined -- this will not happen
```

# Approach with Alex and Happy

> TL;DR: see [here](https://github.com/BoeingX/codewars/tree/master/src/MoleculeToAtomsWithAlexHappy)

## Alex

Alex takes a description of tokens to be recognised
in the form of regular expressions
and **generates** a Haskell lexical analyser.

For our problem, the lexer could be as follows

```haskell
{
module Lexer where
}

%wrapper "basic"

tokens :-
    $white+         ;
    [A-Z][a-z]?     {\s -> TAtom s}
    [0-9]+          {\s -> TIndex (read s)}
    \(              {\s -> TLParen}
    \)              {\s -> TRParen}
    \[              {\s -> TLBracket}
    \]              {\s -> TRBracket}
    \{              {\s -> TLBrace}
    \}              {\s -> TRBrace}

{
data Token = TAtom String
           | TIndex Int
           | TLParen
           | TRParen
           | TLBracket
           | TRBracket
           | TLBrace
           | TRBrace
    deriving (Eq, Show)
}
```

In an Alex source file, any lines between `{` and `}` are
copied directly to the generated Haskell code.
`%wrapper "basic"` controls what kind of support code
Alex should produce along with the basic scanner.

The `tokens :-` line starts the definition of the scanner.
The scanner is specified as a series of token definitions
where each token specification takes the form of

```
regexp   { code }
```

The meaning of a this rule is
*if the input matches `regexp`, then return `code`*.
`code` could be any Haskell function of type

```
f :: String -> Token
```

When a token (for example, white space or comments)
is to be ignored,
the code part (along with the braces)
can be replaced by `;`.

Process[^alex] this file (suppose name `Lexer.x`) with

```haskell
alex Lexer.x
```

we get a Haskell module named `Lexer.hs` which exports
a function

```haskell
alexScanTokens :: String -> [Token]
```

that we can use in other modules.

[^alex]: If you use the Stack tool, all you have to do
is to add a `build-tools` session in `package.yaml`
```yaml
build-tools:
- alex
- happy
```
and add `array` to `dependencies`.
You are free to import `Lexer` throughout in the project
even though only an Alex file exists.
Stack will automatically detects the dependency
and generate `Lexer.hs` before actually compiling the project.

## Happy

Before showing how Happy works,
let us first introduce [Context-free Grammar (CFG)](https://en.wikipedia.org/wiki/Context-free_grammar).

Context-free grammar consists of a set of production rules
that describe all possible strings in a given
formal language.
Production rules are simple replacements.
Production rules whose right hand side is empty string
is called $\epsilon-$production.

For example, the context-free grammar

```
A -> Ba
B -> A | epsilon
```

generates the language of zero or more *a*'s

```
{"", "a", "aa", "aaa", ...}
```

In the sense of context-free grammar,
parsing a given string is to determine
whether and **how** it can be generated from the grammar.

Happy takes a file containing an annotated [BNF specification](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) of a grammar and produces a Haskell module containing a parser for the grammar.

It happens that many languages are context-free.
In particular, the language of valid chemical formulas
is context-free.
Indeed, it can be described by the following CFG

```
Molecule -> M
          | MoleculeMolecule

M        -> Atom Index
          | (Molecule)Index
          | [Molecule]Index
          | {Molecule}Index

Index    -> epsilon | int

Atom     -> "H" | "He" | ...
```

Now we translate the above grammar into a Happy grammar file.
The file starts off like this
```haskell
{
module Grammar where

import Lexer (Token(..))
}
```
where we defined the name of the grammar module and
import from the lexical analysis module the token
data type and all its constructors.

Next we make a couple of declarations
```haskell
%name parse
%tokentype { Token }
%error { parseError }
```
`%name parse` gives the name of the Haskell parser function.
`%tokentype {Token}` declares the Haskell type of tokens
that the parser will accept.
The `%error` directive tells Happy the name of a function
it should call in the event of a parse error.

By default, with the above configuration the parser will be
of type
```haskell
parse :: [Token] -> T
```
where `T` is the return type of the parser
determined by the production rules introduced later.
However, if an error is encounter during passing,
the program with crash with no error message.
In most cases, this is not the desired behavior.
Fortunately, Happy makes it easy to wrap the parser
in a monad.
By adding
```haskell
%monad { Either String }
```
to the declarations, the parser function (and the parse error handling function)
will have type
```haskell
parser :: [Token] -> Either String a
```
This way, we can not only wrap error messages in the `Either`
monad, but also make the parser function have the required
return type in the original problem.

Now we declare all the possible tokens:
```haskell
%token
    index           { TIndex $$ } 
    atom            { TAtom $$ }
    '('             { TLParen }
    ')'             { TRParen }
    '['             { TLBracket }
    ']'             { TRBracket }
    '{'             { TLBrace }
    '}'             { TRBrace }
%%
```
The symbols on the left are the tokens as they will be
referred to in the rest of the grammar file,
and to the right of each token enclosed in braces is a
Haskell pattern that matches the token.
The parser will expect to receive a stream of tokens,
each of which will match one of the given patterns
(the definition of the `Token` datatype is imported
from the lexer module).
The `$$` symbol is a placeholder that represents
the value of this token.

The translation of the CFG to Happy grammar is almost trivial
```haskell
Molecule : M                 { Molecule $1 }
         | Molecule Molecule { Compound $1 $2 }

M : atom Index              { Singleton $1 $2 }
  | '(' Molecule ')' Index  { Multiply $2 $4 }
  | '[' Molecule ']' Index  { Multiply $2 $4 }
  | '{' Molecule '}' Index  { Multiply $2 $4 }

Index : {- empty -} { One }
      | index       { Mul $1 } 
```
Each production consists of a **non-terminal** symbol
on the left,
followed by a colon,
followed by one or more expansions on the right,
separated by `|`.
Each expansion has some Haskell code associated with it,
enclosed in braces as in Alex file.
Note that in the Haskell code, we use `$1, $2` etc
to refer to the symbols on the right of colon[^dollar].

[^dollar]: For example, 
```haskell
M: '(' Molecule ')' Index  { Multiply $2 $4 }
--  $1    $2     $3   $4  
```

Finally, defined the parse error handling function
and declare the data type that represents
the parsed expression

```haskell
{
parseError :: [Token] -> Either String a
parseError _ = Left "Not a valid molecule"

type Atom = String

data Molecule = Molecule M 
              | Compound Molecule Molecule
    deriving (Eq, Show)

data M = Singleton Atom Index
       | Multiply Molecule Index
    deriving (Eq, Show)

data Index = One | Mul Int
    deriving (Eq, Show)

}
```

Process[^happy] this file with Happy (suppose the filename is `Grammar.y`)

```bash
happy Grammar.y
```
an Haskell module named `Grammar.hs` will be generated
which parses our CFG.

[^happy]: Again, as with Alex, if you use the Stack tool,
you are free to import `Grammar` throughout in the project
even though only a Happy file exists.
Stack will automatically detects the dependency
and generate `Grammar.hs` before actually
compiling the project.

## Wrap

Given the lexer and parser, let's wrap everything into
`parseMolecule` with required type
```haskell
evalM :: M -> Composition
evalM (Singleton s i) = mul i (M.fromList [(s, 1)])
evalM (Multiply m i) = mul i (eval m)

eval :: Molecule -> Composition
eval (Molecule m) = evalM m
eval (Compound m1 m2) = merge (eval m1) (eval m2)

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = M.toList . eval <$> parse (alexScanTokens formula)
```
# Further reading

## Parser combinators

- [ReadP documentation](https://hackage.haskell.org/package/base-4.9.0.0/docs/Text-ParserCombinators-ReadP.html)
- Haskell Programming from First Principles, the [Parsers chapter](http://haskellbook.com/progress.html#parsers)

## Alex and Happy

- [Alex documentation](https://www.haskell.org/alex/doc/html/index.html) and [Happy documentation](https://www.haskell.org/happy/doc/html/index.html)
- Stanford [CS143 course](http://web.stanford.edu/class/cs143/) (a MOOC version is freely available as [CS1](https://lagunita.stanford.edu/courses/Engineering/Compilers/Fall2014/about) on [Stanford Lagunita](https://lagunita.stanford.edu/))
