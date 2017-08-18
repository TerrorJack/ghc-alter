# ghc-alter

[![Build Status](https://travis-ci.org/TerrorJack/ghc-alter.svg)](https://travis-ci.org/TerrorJack/ghc-alter)

`ghc-alter` is a set of libraries to ease the development of custom Haskell compilers on top of ghc. Currently contains:

* [`ghc-alter-print-ir`](ghc-alter-print-ir/): Orphan `Show` instances for various ghc IR types. Useful for debugging & studying ghc internals.
* [`ghc-alter-store`](ghc-alter-store/): A simple object file store. Useful for compiling Haskell sources to custom object formats.
* [`ghc-alter-with-ir`](ghc-alter-with-ir/): Extract IRs from ghc pipeline for custom compilation, with support for patching parsed modules.

## Documentation

[Auto-generated haddock documentation](https://terrorjack.github.io/ghc-alter/)

Documentation is currently lacking, as the main focus right now is developing new functionalities. Hopefully the haddock documentation can be helpful, we try to make API simple & self-explanatory.
