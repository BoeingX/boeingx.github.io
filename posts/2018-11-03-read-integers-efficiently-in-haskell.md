---
title: Read integers efficiently in Haskell
description: What is the best practice to read a list of numbers from standard input (or a file) in Haskell? This post will show you by conducting a series of benchmarks.
tags: haskell, performance
---

## Introduction

> TL;DR: See this [GitHub gist](https://gist.github.com/BoeingX/a4a67c03599c381d5f1371c1f073be39)

Recently I participated in a [online programming contest](https://www.codechef.com/snackdown).
Since I was learning and practicing Haskell for the last few months,
I decided to solve the problems in Haskell.

The contest uses a online judge (OJ) system to evaluate submitted solutions.
A valid solution is supposed to read test data from standard input
and print the answer to standard output.
Sometimes, inputs are integer arrays of length `10^5`,
and I got systematically TLE (Time Limit Exceeded) error with the OJ.
Ironically, without changing the algorithm,
recoding everything in Python always pass the tests :(.

Certainly, the OJ uses [5x time limit](https://blog.codechef.com/2009/04/01/announcing-time-limits-based-on-programming-language/)
for solutions in Python.
However, a closer investigation shows that the Haskell version
is more than 5 times slower than the Python version:
how could this be possible?

To keep things simple,
throughout this post we consider the simple problem


> Read a list of numbers from standard input and print the sum to standard output.


Also, we use following Python script

```python
# filename: baseline.py
import sys

if __name__ == '__main__':
    xs = [int(x) for x in sys.stdin.readline().strip().split(' ')]
    print(sum(xs))
```

and Haskell code[^numbers]

```haskell
-- filename: Baseline.hs

main = do
    xs <- map read . words <$> getLine
    putStrLn $ "Sum: " ++ show (sum xs)
```

as baseline.
We also generated a test file `data.txt` containing a single line of
10^5 space-separated randomly generated integers between 0 and 10^5.

[numbers]: Careful reader will find that we leave `xs` polymorphic,
which is mostly for the sake of simplicity.
We could have provided a concrete type like `Int`.
This is indeed accelerate a bit the program, but it is not at all the bottleneck.

With the `time` command line utility, the Python script took 0.05 seconds
whereas the Haskell one, compiled with `-O2` flag, took 0.33 seconds (6x slower).

## Locate the bottleneck

Donald Knuth once said,
*premature optimization is the root of all evil*,
because we can easily 
*spent far too much time worrying about efficiency in the wrong places and at the wrong times*[^knuth].
This is bloody true.
Bearing this in mind, we will first profile the program and find the bottleneck
before any attempt of optimization.

[knuth]: Computer Programming as an Art (1974), https://en.wikiquote.org/wiki/Donald_Knuth

To profile the Haskell code, we insert some [cost centers by hand](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#inserting-cost-centres-by-hand)
with the GHC pragma `SCC`:

~~~~~{.haskell}
-- filename: BaselineProfile.hs

main = do
    xs <- {-# SCC read_list #-} map read . words <$> getLine
    putStrLn $ "Sum: " ++ show ({-# SCC sum_list #-} sum xs)
~~~~~

Cost centers are program annotations around expressions that GHC's profiling system assigns cost.
It can be either generated automatically with `-fprof-auto` compiling flag
or specified manually with pragma as we did here[^hand].

[hand]: If we do not insert the pragmas, GHC will profile the `main` function as a whole
which loses the point of profiling!

Save the file as `BaselineProfile.hs`, then compile and run the program as follows

```bash
$ stack ghc -- -prof -rtsopts -O2 BaselineProfile.hs
$ cat /path/to/data | ./BaselineProfile +RTS -p
```

When a GHC-compiled program is run with the -p RTS option,
it generates a file called `<program-name>.prof`.
In our case, the file will contain something like this (with irrelevant lines omitted)

```
	Sun Nov  4 10:40 2018 Time and Allocation Profiling Report  (Final)

	   BaselineProfile +RTS -p -RTS

	total time  =        0.55 secs   (548 ticks @ 1000 us, 1 processor)
	total alloc = 595,340,832 bytes  (excludes profiling overheads)

COST CENTRE MODULE    SRC                         %time %alloc

read_list   Main      BaselineProfile.hs:2:33-60   99.5   99.7
```

As we can see, the seemingly innocent `read_list` cost center took more than 99% of time!
The bottleneck being located, let's figure out why is that and how can we do better.

## Optimization

To read from standard input, we used the `getLine :: IO String` from the `System.IO` module[^prelude].
It produces a `String` which is nothing but a type alias of `[Char]`, a `List` of character.
This could be inefficient for at least two reasons:

[prelude]: `Prelude` exports this function so we do not need to import `System.IO` manually.

1. `List` is essentially a singly linked list, which is lazy evaluated.
Lazy evaluation often hurts the performance by [adding a constant overhead to everything](https://wiki.haskell.org/Performance/Strictness).
2. Data from the input are in binary form.
The `read_list` routine first parses binary to `[Char]` and then `[Char]` to `Int`.
We could have gone directly from binary to `Int`.

To address these two issues, we introduce two modules:
[Text](https://hackage.haskell.org/package/text) and [ByteString](https://hackage.haskell.org/package/bytestring).

Note that with these two modules, we need to change both the *reading* (e.g. `getLine`)
and *parsing* (e.g. `map read . words`) routine.
Also, because they both export function names in conflict with ones in `Prelude`,
we should use [qualified import](https://wiki.haskell.org/Import).

### Text

`Text` is an efficient packed, immutable Unicode text type (both strict and lazy),
with a powerful [loop fusion](https://wiki.haskell.org/GHC_optimisations#Fusion)
optimization framework.
It represents Unicode character strings, in a *time* and *space*-efficient manner. 
Moreover, it addresses the lazy evaluation issue by loading the entire data into memory by default.

`Data.Text.IO` exports `getLine :: IO T.Text`,
which reads a line from standard input as our `Text` data type.
A common mistake is to use then `words` from `Prelude` to split `Text` with spaces.
This will not type-check and GHC complains

```
Couldn't match type ‘T.Text’ with ‘[Char]’
```

This makes perfectly sense, because `words` expects `[Char]` as input but `Text` is a new data type,
not just a type synonym.
Fortunately, `Data.Text` provides a counterpart also named (surprise!) `words :: T.Text -> [T.Text]`.

Then the problem comes: how can we *parse* a `Text` into an `Int`?
Though `Text` and `Int` are both instances of `Read`, we cannot use `read` for neither
`Text` nor `Int` since `read :: Read a => String -> a`.

Generally, we need a parser library which supports `Text` such as [parsec](https://hackage.haskell.org/package/parsec), [attoparsec](https://hackage.haskell.org/package/attoparsec).
Since parsing numbers are so common, `Data.Text.Read` exports

```haskell
decimal :: Integral a => Either String (a, Text)
```

which reads a decimal integer. The input must begin with at least one decimal digit, and is consumed until a non-digit or end of string is reached.
If the read succeeds, return its value and the remaining text, otherwise an error message.
Let's see an example

```haskell
Prelude> T.decimal $ T.pack "123"
Right(123,"")
Prelude> T.decimal $ T.pack "123 456"
Right(123," 456")
Prelude> T.decimal $ T.pack "abc"
Left "input does not start with a digit"
```

Note the second example, the parser reads as far as it can and return the unconsumed `Text`
along with the parsed `Int`. 
This is [the typical behavior](https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-ParserCombinators-ReadP.html#v:readP_to_S)
when running a parser.
How can we parse all integers, instead of just the first one, and return them as a `List`?
This is generally called [tokenization](https://en.wikipedia.org/wiki/Tokenization)
which produces a list of tokens for parsing.
In our case, the tokenization is very simple: just *split* on spaces,
and this is already done with `T.words`!

The following code wraps everything[^import]

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

readText :: IO [Int]
readText = map parse . T.words <$> T.getLine
    where parse :: T.Text -> Int
          parse s = let Right (n, _) = T.decimal s in n
```

[import]: Yes, we can give two qualified imports the same name,
as long as there is no conflict in function names.

### ByteString

`ByteString` is an efficient compact, immutable byte string type (both strict and lazy) suitable
for binary or 8-bit character data.
It is suitable for high performance use, both in terms of large data quantities,
or high speed requirements.
Moreover, `ByteString` functions follow the **same** style as Haskell's ordinary lists,
so it is easy to convert code from using `String` to `ByteString`.
It addresses the second issue by parsing **directly** an Int from binary data.

Since we only deal with integers in our case,
we can use `Data.ByteString.Char8` module, 
which covers the subset of Unicode covered by code points 0-255.

The code using `ByteString` is nearly the same as that using `Text`

```haskell
import qualified Data.ByteString.Char8 as C

readByteString :: IO [Int]
readByteString = map parse . C.words <$> C.getLine
    where parse :: C.ByteString -> Int
          parse s = let Just (n, _) = C.readInt s in n
```

## Benchmark

How is the speedup of using `Text` and `ByteString`?
To answer this question, we need to *benchmark* our code.
To this end, we will use the [Criterion](https://hackage.haskell.org/package/criterion) library.
We are not going to introduce Criterion in detail
and refer interested readers to [this excellent tutorial](http://www.serpentine.com/criterion/tutorial.html).

Criterion supports benchmarking IO functions.
However, it is only possible to read from a file, not from standard input[^stdin].
Therefore, we modified our code so that it reads directly a file named `data.txt`.

[stdin]: Using `/dev/stdin` as filename will **not** work.

```haskell
-- filename: Main.hs

import Control.Monad
import Criterion.Main
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

readBaseline :: String -> [Int]
readBaseline = map parse . words
    where parse :: String -> Int
          parse = read

readText :: T.Text -> [Int]
readText = map parse . T.words
    where parse :: T.Text -> Int
          parse s = let Right (n, _) = T.decimal s in n


readByteString :: C.ByteString -> [Int]
readByteString = map parse . C.words
    where parse :: C.ByteString -> Int
          parse s = let Just (n, _) = C.readInt s in n

main = do
    let filename = "data.txt"
    defaultMain [
                  bench "readBaseline" $ nfIO $ map readBaseline . lines <$> readFile filename
                , bench "readText" $ nfIO $ map readText . T.lines <$> T.readFile filename
                , bench "readByteString" $ nfIO $ map readByteString . C.lines <$> C.readFile filename
                ]
```

Compile and run the above program with

```bash
$ stack ghc -- -O2 Main.hs
$ ./Main
```

and we got the following benchmark result

```
benchmarking readBaseline
time                 346.7 ms   (344.5 ms .. 349.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 346.5 ms   (345.9 ms .. 347.0 ms)
std dev              640.7 μs   (447.9 μs .. 776.0 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking readText
time                 11.19 ms   (11.02 ms .. 11.30 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 11.42 ms   (11.31 ms .. 11.75 ms)
std dev              466.2 μs   (104.7 μs .. 907.9 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking readByteString
time                 7.216 ms   (7.173 ms .. 7.255 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.230 ms   (7.217 ms .. 7.246 ms)
std dev              45.26 μs   (38.18 μs .. 56.04 μs)
```

> You can check the interactive benchmark report [here](/files/benchmark-reading-ints.html)

Both the `Text` and `ByteString` version reduce significantly the run time.
The `ByteString` version is slightly faster because it parses directly binary data
into integers without intermediate types.
Note also that these two versions also run **much** faster than the Python code.
