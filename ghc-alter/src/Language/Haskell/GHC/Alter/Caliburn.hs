module Language.Haskell.GHC.Alter.Caliburn where

import Language.Haskell.GHC.Alter.Compiler

initCompiler :: IO Compiler
initCompiler = pure defaultCompiler
