# ghc-alter

[![Build Status](https://travis-ci.org/TerrorJack/ghc-alter.svg)](https://travis-ci.org/TerrorJack/ghc-alter)

`ghc-alter` is a set of packages to ease the development of custom Haskell compilers on top of ghc. Currently contains:

* [`ghc-alter-eval`](ghc-alter-eval): Import values at runtime.
* [`ghc-alter-buildinfo`](ghc-alter-buildinfo): Obtain build info of current package.
* [`ghc-alter-frontend`](ghc-alter-frontend): Utils for building ghc frontend plugins.
* [`ghc-alter-orphans`](ghc-alter-orphans): Orphan `Show` instances for various ghc IR types. Useful for debugging & studying ghc internals.
* [`ghc-alter-store`](ghc-alter-store): A simple object file store. Useful for compiling Haskell sources to custom object formats.
* [`ghc-alter-unexported`](ghc-alter-unexported): Unexported functions from ghc.
* [`ghc-alter-with-ir`](ghc-alter-with-ir): Extract IRs from ghc pipeline for custom compilation, with support for patching parsed modules.

Requires ghc-head. For packages other than `ghc-alter-orphans` & `ghc-alter-unexported`, ghc-8.2.1 may work as well.

## Documentation

[Auto-generated haddock documentation](https://terrorjack.github.io/ghc-alter/)

Documentation is currently lacking, as the main focus right now is developing new functionalities. Hopefully the haddock documentation can be helpful, we try to make API simple & self-explanatory.
