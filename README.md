simple
=======
An implementation of System F in Haskell

# Overview

`systemf` is an implementation of System F presented by Benjamin C. Pierce in his [Types and Programming Languages][tapl]. I transliterated the original source code written in OCaml into Haskell except for the parser which I rewrote in [Parsec][parsec].

# Syntax

* Variables: `x`
* Applications: `x x`
* Lambda abstractions: `\x.x`
* Type abstractions: `/\X.X`
* Conditional: `if x then y else z`
* Booleans: `true`, `false`
* Universal quantification: `All X.X->X`
* Existential quantification : `{*All Y.Y, \x:(All Y.Y). x} as {Some X,X->X}`

# Implementation Details

* The representation of a variable is a number - its De Brujin index
* Evaluation performs substitution

# REPL

`systemfi` is a REPL where you can input a lambda calculus term.

```
% \x:Bool.x
(\x:Bool.x)
$ {*All Y.Y, \x:(All Y.Y). x} as {Some X,X->X}
(\x:(All X.(X->X)).x)
```

[parsec]: https://hackage.haskell.org/package/parsec
[tapl]: https://www.google.co.kr/search?q=tapl&oq=tapl&aqs=chrome..69i57j69i60l3j0l2.776j0j7&sourceid=chrome&ie=UTF-8#q=tapl+benjamin
