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
import DynFlags
import HscTypes
import Language.Haskell.GHC.Kit.WithIRs
import Module
import Outputable
import System.Directory
import System.FilePath

newtype CompilerConfig a = CompilerConfig
  { topdir :: FilePath
  }

data CompilerStore a = CompilerStore
  { moduleGet :: Module -> IO a
  , modulePut :: Module -> a -> IO ()
  }

moduleKey :: Module -> FilePath
moduleKey = showSDocOneLine unsafeGlobalDynFlags . pprModule

newCompilerStore :: Binary a => CompilerConfig a -> IO (CompilerStore a)
newCompilerStore CompilerConfig {..} = do
  createDirectoryIfMissing True topdir
  pure
    CompilerStore {moduleGet = decodeFile . tofn, modulePut = encodeFile . tofn}
  where
    tofn = (topdir </>) . moduleKey

newtype Compiler a = Compiler
  { runCompiler :: ModSummary -> IR -> IO a
  }

compileTo ::
     Binary a => CompilerStore a -> Compiler a -> IO (ModSummary -> IR -> IO ())
compileTo CompilerStore {..} Compiler {..} =
  pure $ \mod_summary ir -> do
    r <- runCompiler mod_summary ir
    modulePut (ms_mod mod_summary) r
