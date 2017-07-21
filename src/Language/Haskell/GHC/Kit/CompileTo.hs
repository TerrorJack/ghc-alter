{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.CompileTo
  ( CompilerConfig(..)
  , CompilerStore(..)
  , newCompilerStore
  , Compiler(..)
  , compileTo
  ) where

import Data.Binary
import FastString
import HscTypes
import Language.Haskell.GHC.Kit.WithIRs
import Module
import System.Directory
import System.FilePath

newtype CompilerConfig a = CompilerConfig
  { topdir :: FilePath
  }

data CompilerStore a = CompilerStore
  { moduleGet :: Module -> IO a
  , modulePut :: Module -> a -> IO ()
  }

moduleKey :: Module -> (FilePath, FilePath)
moduleKey Module {..} =
  (unpackFS (unitIdFS moduleUnitId), unpackFS (moduleNameFS moduleName))

newCompilerStore :: Binary a => CompilerConfig a -> IO (CompilerStore a)
newCompilerStore CompilerConfig {..} =
  pure
    CompilerStore
    { moduleGet = decodeFile . tofn
    , modulePut =
        \mod_key x ->
          let fn = tofn mod_key
          in do createDirectoryIfMissing True $ takeDirectory fn
                encodeFile fn x
    }
  where
    tofn mod_key = topdir </> k0 </> k1
      where
        (k0, k1) = moduleKey mod_key

newtype Compiler a = Compiler
  { runCompiler :: ModSummary -> IR -> IO a
  }

compileTo ::
     Binary a => CompilerStore a -> Compiler a -> IO (ModSummary -> IR -> IO ())
compileTo CompilerStore {..} Compiler {..} =
  pure $ \mod_summary ir -> do
    r <- runCompiler mod_summary ir
    modulePut (ms_mod mod_summary) r
