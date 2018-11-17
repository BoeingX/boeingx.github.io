---
title: Write slides with literate Haskell and LaTeX Beamer
description: How to take the best of both worlds without pain.
tags: latex, beamer, haskell, literate programming
---

## Introduction

> TL;DR: Check this [GitHub Repository](https://github.com/BoeingX/beamer-template-for-literate-haskell) and enjoy writing :)

[Literate programming](https://en.wikipedia.org/wiki/Literate_programming), introduced by [Donald Knuth](https://www-cs-faculty.stanford.edu/~knuth/)[^WEB],
is a programming paradigm where source code is written
interleaved with text in natural language in a way that
it is both a valid document (for example, article)
and compilable (or interpretable) program. 

If you are familiar with [Doxygen](http://www.doxygen.org/)
or [Sphinx](http://www.sphinx-doc.org/en/master/)
which generate documentation from the comments
of source code,
you can consider literate programming as the opposite:
it is the source code in documentation.
The idea behind is best explained by Knuth's own words

> "The main idea is to regard a program as a communication to human beings rather than as a set of instructions to a computer."

[^WEB]: Knuth wrote the TeX engine, which powers document preparation systems like LaTeX, XeLaTeX and many more,
in literate Pascal called [WEB](https://en.wikipedia.org/wiki/WEB).

## Literate programming in Haskell

If you are data scientist like me,
you must be familiar with [Jupyter Notebook](https://jupyter.org/)
where we can write Markdown or even LaTeX snippet along with Python[^R]
(or many others through [kernels](https://en.wikipedia.org/wiki/Project_Jupyter#Jupyter_kernels)) code.
So basically you are using literate programming everyday!

[^R]: If you prefer using [R](https://www.r-project.org/about.html), [R Notebook](https://rmarkdown.rstudio.com/r_notebooks)
looks great too.

However, I personally find two inconveniences with Jupyter

- Python does not support literate programming **natively**,
which means we have to install **many** dependencies in order to use it.
- The underlying serialization of Jupyter notebooks is JSON
and it is extremely difficult to work with in text editors
or on remote machines with strict firewall.

Haskell, on the other hand,
is one of [the few languages](https://en.wikipedia.org/wiki/Literate_programming#Tools)
that support literate programming natively.
In other words, files written in literate Haskell can be compiled directly with GHC or loaded into GHCi.

Instead of the normal `.hs` extension, a literate program ends with `.lhs` where `l` means literate (surprise!).
To tell GHC a portion of text is code, we have two options

- Prepend all code with a `>` (Bird style)
- Surround lines of code with `\begin{code}` and `\end{code}` pairs (LaTeX style)


Here are two examples[^mix]

[^mix]: It is not recommended to mix these two styles,
otherwise you will get some mysterious parse errors.

- Bird style
```
In Bird-style you have to leave a blank before the code.
 
> fact :: Integer -> Integer
> fact 0 = 1
> fact n = n * fact (n-1)
 
And you have to leave a blank line after the code as well.
```

- LaTeX style

```
In LaTeX style, the code snippet must be at the top indentation level
or you will have an `unlit' error
\begin{code}
tsort []     = []
tsort (x:xs) = tsort [y | y<-xs, y>x] ++ [x] ++ tsort [y | y<-xs, y<=x]
\end{code}
```

It [seems](https://wiki.haskell.org/Literate_programming) that the LaTeX style is more robust,
so you should privilege it whenever possible.

As to the non-code portions, Haskell is rather agnostic
so you can write LaTeX, HTML or whatever you want.
But be careful, however, if you write Markdown

- GHC is not happy with leading `#` which indicates heading
in Markdown.
As a workaround, you can use `---` or `===` **below** the line of interest.
- `>` signifies quotation in Markdown.

## Write slides with literate Haskell

Since in literate Haskell we have very few restraints on
writing non-code text,
we can actually write slides!

### Javascript-based approach

A number of Javascript based libraries such as
[reveal.js](https://revealjs.com/)
make it possible to produce attractive slides
with HTML or Markdown sources.
However, it is not easy (if possible at all)
to write a literate Haskell which
gets highlighted correctly in the rendered HTML output.
It is even more difficult to hide valid code in the
slides or hide invalid code from Haskell.

One possible solution is to use [Pandoc](https://pandoc.org/) as a middleware when render the HTML.
For example, it [can output `reveal.js` HTML
from Markdown source](https://github.com/jgm/pandoc/wiki/Using-pandoc-to-produce-reveal.js-slides).
However, since we cannot use `#`, `##` to mark levels,
the output HTMLs have very shallow structure.

### LaTeX beamer

The best solution I found is to simply use LaTeX
[beamer](https://ctan.org/pkg/beamer).
For those who are not familiar with it,
it is a LaTeX package which are widely used
to produce academic slides.
Since literate Haskell supports LaTeX mode,
we have **full** control over the output slides.

Only a few pitfalls here

- the file must have `.lhs` extension
- when producing slides it must be compile with `-shell-escape` option
- `frame` must use `fragile` mode
- `\begin{code}` and `\end{code}` must be at the top indentation level

Here is a minimalist template

```latex
% filename: main.lhs
\documentclass{beamer}

\usepackage{minted}
\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}

\newenvironment{slide}[1]
  {\begin{frame}[fragile,environment=slide]{#1}}
  {\end{frame}}
\long\def\ignore#1{}

\title{Title}
\subtitle{Subtitle}
\author{Author}
\date{\today}

\begin{document}

\begin{slide}{Example}
This appear both in slides and Haskell
\begin{code}
add :: Integer -> Integer -> Integer
add x y =  x + y
\end{code}
This only appear in slides
\begin{spec}
-- broken code
foo :: String
foo = bar
\end{spec}
And this only appear in Haskell
\ignore{
\begin{code}
module Main where

import Control.Applicative
\end{code}
}
\end{slide}

\end{document}
```
