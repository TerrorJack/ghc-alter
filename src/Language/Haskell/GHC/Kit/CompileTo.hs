{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.CompileTo
  ( CompilerConfig(..)
  , CompilerStore(..)
  , newCompilerStore
  , Compiler(..)
  , compileTo
  ) where

import HscTypes
import Language.Haskell.GHC.Kit.WithIRs
import Module

newtype CompilerConfig a = CompilerConfig
  { topdir :: FilePath
  }

data CompilerStore a = CompilerStore
  { moduleGet :: Module -> IO a
  , modulePut :: Module -> a -> IO ()
  }

newCompilerStore :: CompilerConfig a -> IO (CompilerStore a)
newCompilerStore = undefined

newtype Compiler a = Compiler
  { runCompiler :: ModSummary -> IR -> IO a
  }

compileTo :: CompilerStore a -> Compiler a -> IO (ModSummary -> IR -> IO ())
compileTo = undefined
