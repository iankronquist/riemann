Riemann
=======

A Haskell implementation of the Zeta language, or at least its parser.

Because C is, "Unmaintanable devil spawn". Also I want to learn to use the
Parsec library.

It is useful to develop two parallel implementations of a language, such
as Google Go and GCC Go. This ensures that the language itself is formally
defined and not too closely tied to its implementation (**cough Ruby cough**).

Building
--------

You will need Cabal and GHC.

```
$ cabal sandbox init
$ cabal install
$ cabal build
```
