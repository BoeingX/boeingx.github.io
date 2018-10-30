---
title: foldr and the Implementation of (!!) Operator in Haskell
description: Explain the not-so-obvious usage of foldr in the implementation of list index operator (!!) through type inspection and a simple example.
tags: haskell
---

## Introduction

If you come from an imperative language, you are certainly familiar with the list index operator `[]`,
which is used to access arbitrary elements in an array in constant time.
In Haskell, however, the omnipresent list `[a]` is more like a linked list
(there does exist an [array data structure](https://hackage.haskell.org/package/array) in GHC base)
defined in [GHC.Types](https://hackage.haskell.org/package/ghc-prim-0.5.3/docs/GHC-Types.html) as follows

~~~~~{.haskell}
data [] a = [] | a : [a]
~~~~~

To access an arbitrary element in a list, we use the (!!) operator.
For example

~~~~~{.haskell}
Prelude> let xs = [1,2,3]
Prelude> xs !! 1
2
Prelude> xs !! (-1)
*** Exception: Prelude.!!: negative index
Prelude> xs !! 4
*** Exception: Prelude.!!: index too large
~~~~~

Note that because of the nature of list, (!!) has linear time complexity.
In other words, to access the last element, you will have to traverse the **entire** list.

An intuitive implementation of (!!) could be

~~~~~{.haskell}
(!!) :: [a] -> Int -> a
xs !! n
    | n < 0 = error "negative index"
[] !! _ = error "index too large"
(x:xs) !! n
    | n == 0    = x
    | otherwise = xs !! (n - 1)
~~~~~

The above implementation being a [tail recursion](https://en.wikipedia.org/wiki/Tail_call),
it is optimized by GHC and becomes essentially a loop.
Therefore, you will not get a "max recursion depth" error for a very large list as in imperative languages.

However, if you check the [GHC source code](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#%21%21), this function is actually implements as follows

~~~~~{.haskell}
(!!)                    :: [a] -> Int -> a
#if defined(USE_REPORT_PRELUDE)
xs     !! n | n < 0 =  errorWithoutStackTrace "Prelude.!!: negative index"
[]     !! _         =  errorWithoutStackTrace "Prelude.!!: index too large"
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n-1)
#else

-- We don't really want the errors to inline with (!!).
-- We may want to fuss around a bit with NOINLINE, and
-- if so we should be careful not to trip up known-bottom
-- optimizations.
tooLarge :: Int -> a
tooLarge _ = errorWithoutStackTrace (prel_list_str ++ "!!: index too large")

negIndex :: a
negIndex = errorWithoutStackTrace $ prel_list_str ++ "!!: negative index"

{-# INLINABLE (!!) #-}
xs !! n
  | n < 0     = negIndex
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) tooLarge xs n
#endif
~~~~~

As we can see, there are two implementations 
where the first implementation is exactly what we previously gave.
Which of them gets compiled depends on where
`USER_REPORT_PRELUDE` is defined.
In general, the blocks in `USER_REPORT_PRELUDE` are legacy code and no longer used
(see [this discussion](https://mail.haskell.org/pipermail/ghc-devs/2014-October/006948.html)).
and those in the `else` block are generally more (time or space) efficient.

Ignore for now the `INLINABLE` pragma,
we can see that it is essentially the `foldr` that differents the second implementation from the first one.
`foldr` is generally more efficient than explicite recursion
because it is systematically optimized by GHC (see [this post](https://www.stackbuilders.com/tutorials/haskell/ghc-optimization-and-fusion/)).

## What type?

Recall that `foldr` has the following type

~~~~~{.haskell}
foldr :: (a -> b -> b) -> b -> [a] -> b
~~~~~

Given a binary operator `f :: a -> b -> b`, a list `xs = [x1, x2, ..., xn]` and an initial value `z`,
`foldr` can be thought as

~~~~~{.haskell}
foldr f z xs = x1 `f` ( x2 `f` ( .. (xn `f` z) ... )
~~~~~

If you inspect the second implementation, however, the use of `foldr` is somewhat unusual:
1) why there are three arguments `x`, `r` and `k` in the **binary** operator
and 2) why we have a `n` after the list `xs`?

To understand this, let's first identify the `a` and `b` in the type of `foldr`.
Note `xs :: [a]`, since `tooLarge :: Int -> a`, actually the only possibility would be

~~~~~{.haskell}
foldr :: (a -> (Int -> a) -> (Int -> a)) -> (Int -> a) -> [a] -> (Int -> a)
~~~~~

If you fancy playing with GHCi, you can assert this as follows

~~~~~{.haskell}
Prelude> let xs :: [a] ; xs = undefined
Prelude> let tooLarge :: Int -> a ; tooLarge = undefined
Prelude> :t \f -> foldr f tooLarge xs
\f -> foldr f tooLarge xs
  :: (a1 -> (Int -> a2) -> Int -> a2) -> Int -> a2
~~~~~

Since `(->)` is right associative, this matches exactly our assertion.

With this type in mind, it is easy to answer the two questions: 

- The binary operator `f` has three arguments `x`, `r` and `k` because of [currying](https://wiki.haskell.org/Currying). If you haven't got it, the following rewriting may help you

~~~~~{.haskell}
\x r -> \k -> case k of
                0 -> x
                _ -> r (k - 1)
~~~~~

- There is a `n :: Int` at the end of `foldr` because the fully applied `foldr` gives a **function** `Int -> a`. In other words, `n` is the argument of that function.

## Example

Let's take an example to see this implementation in action.
Suppose that `xs = [0, 1, 2, 3]` and we want to evaluate `xs !! 2`, 
note `f = \x r k -> case k of 0 -> x ; _ -> r (k - 1)`, then

~~~~~{.haskell}
xs !! 2 = foldr f tooLarge xs 2
        = 0 `f` ( 1 `f` ( 2 `f` ( 3 `f` tooLarge ) ) ) 2
--        |     |____________________________________| |
--        x                       r                    k
-- because k = 2 /= 0, so the second branch of case gets evaluated
-- now we have r (k - 1)
        = (1 `f` ( 2 `f` ( 3 `f` tooLarge ) ) ) 1
--        |___________________________________| |
--                          r                  k-1
        = 1 `f` ( 2 `f` ( 3 `f` tooLarge ) ) 1
--        |     |__________________________| |
--        x                  r               k   <-- alpha equivalence
        = ( 2 `f` ( 3 `f` tooLarge ) ) 0
--        |__________________________| |
--                     r              k-1
        = 2 `f` ( 3 `f` tooLarge ) 0
--        |     |________________| |
--        x             r          k
-- this time k = 0 so the first branch of case is fired
-- we get simply x = 2
        = 2
~~~~~

As we can see from this example, though `foldr` generally evaluates from right to left,
we get a function waiting for an argument as the result of `foldr`.
When we feed it with an argument, we happen to evaluate from left to right.
Otherwise, `xs !! n` would have given the `n`-th element from right.

## Exercises

1. Take `k = -1` and `k = 4` with the same `xs`, try to see why `xs !! k` gives `negative index` and `index too large` exceptions respectively.

2. Given that

~~~~~{.haskell}
const :: a -> b -> a
const a _ = a
~~~~~

What is the result of

~~~~~{.haskell}
foldr const undefined [1, undefined, undefined]
~~~~~

3. Will the following expression terminates? If yes, what is the result? If not, why?

~~~~~{.haskell}
foldr (\x acc -> if x >= 10 then x else x + acc) 0 [1..]
~~~~~

What happens if we use `foldl` instead of `foldr`?
