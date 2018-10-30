---
title: Understanding the "it" variable in GHCi
description: The mythical "it" variable in GHCi explained
tags: haskell
---

## Introduction

As we are told from the very beginning, Haskell is a [purely functional](https://en.wikipedia.org/wiki/Purely_functional_programming),
strongly [statically typed](https://en.wikipedia.org/wiki/Static_typing) programming language.

By purely functional, we mean that everything is *mostly* immutable.
In non-pure languages like C++ or Python, it is perfectly OK to set a variable `a`
to the value `1` and then to the value `2` later.
The following Haskell code does not compile

~~~~~{.haskell}
a = 1
a = 2
~~~~~

because GHC complains the "multiple declaration of `a`".

Strongly statically typed, on the otherhand,
means that it is not possible to compare a Bool with a Char.
What's more, there is no implicit [type coercion](https://en.wikipedia.org/wiki/Type_conversion)
in that we cannot use `1` as `True` and `0` as `False`.

So far, so good.

Until one day, when playing with GHCi we realized that the "magic" variable `it` refers
always to the value of the last **successful** expression.
The following examples illustrates this behavior.

~~~~~{.haskell}
Prelude> 3
3
Prelude> it
3
Prelude> 'a'
'a'
Prelude> it
'a'
-- We type it again
Prelude> it
'a'
Prelude> True
True
Prelude> it
True
~~~~~

So, what happens?!
It seems that not only we can assign values several times to `it`
but also that the values could have different types!

## Dive in

The question seems very natural, yet there are only a few discussion around it on the Internet.
What's more, it is rather difficult to find such discussions because
`it` is a terrible [stop word](https://en.wikipedia.org/wiki/Stop_words).

It took me a long time before figuring out why
(I once even thought that `it` gets some special treatment in the GHCi implementation).
Actually, the mechanism is **very** simple: the [shadowing](http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html).

Consider the following example (taken from Real World Haskell)

~~~~~{.haskell}
Prelude> bar = let x = 1 in ((let x = "foo" in x), x)
Prelude> bar
("foo", 1)
~~~~~

The inner `x` in the definition of `bar` *shadows* the outer `x`.
In particular, though share the same same, they are **not** the same variable
and nothing prevents us from assigning different type values to them
(in this example, the outer `x` is `Num` and the inner `x` is `String`).

GHCi actually implements an instance of Monad.
In a vanilla Monad, we can do very similar shadowing (with do-syntax)

~~~~~{.haskell}
f :: IO ()
f = do
    let x = 1
    print x
    let x = "foo"
    print x
~~~~~

The idea is always the same, the latter `x` shadows the former `x`
and they are two completely different variables.
It is easier to see this in the following [desugured version](https://en.wikibooks.org/wiki/Haskell/Syntactic_sugar#Do_notation)

~~~~~{.haskell}
Prelude> let x = 1 in print x >> let x = "foo" in print x
1
"foo"
~~~~~

If you actually look at the source code of GHC, what happens is quite similar to what is explained above.
Indeed, you can find the following comments

```
{-
--------------------------------------------------------------------------
                Typechecking Stmts in GHCi
Here is the grand plan, implemented in tcUserStmt
        What you type                   The IO [HValue] that hscStmt returns
        -------------                   ------------------------------------
        let pat = expr          ==>     let pat = expr in return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]
        pat <- expr             ==>     expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]
        expr (of IO type)       ==>     expr >>= \ it -> return [coerce HVal it]
          [NB: result not printed]      bindings: [it]
        expr (of non-IO type,   ==>     let it = expr in print it >> return [coerce HVal it]
          result showable)              bindings: [it]
        expr (of non-IO type,
          result not showable)  ==>     error
-}
```

Basically, when one enters an IO-type expression `expr`, the expression is evaluated
and its value is extracted and bind to `it`

~~~~~{.haskell}
Prelude> print 3
-- The expression gets evaluated and () is bond to it, since print 3 :: IO ()
3
Prelude> it
()
~~~~~

On the other hand, when one enters a non-IO type expression `expr`, 
`expr` gets evaluated and the result is bond to `it`. `it` is also printed to screen.

~~~~~{.haskell}
Prelude> 1 + 2
-- The expression gets evaluated and the result 3 is bond to it. `it` also gets printed.
3
Prelude> it
3
~~~~~

## Further Reading

- [The it variable](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-it-variable) in the [Glasgow Haskell Compiler User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html).

- The [source code](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcRnDriver.hs) where this logic is implemented.
