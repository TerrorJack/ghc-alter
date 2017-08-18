{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Alter.CompilerStore
  ( CompilerConfig(..)
  , CompilerStore(..)
  , newCompilerStore
  ) where

import Module
import System.Directory
import System.FilePath

data CompilerConfig a = CompilerConfig
  { topdir, ext :: FilePath
  , rawPut :: FilePath -> a -> IO ()
  }

newtype CompilerStore a = CompilerStore
  { modulePut :: Module -> a -> IO ()
  }

moduleKey :: Module -> (FilePath, FilePath)
moduleKey Module {..} = (unitIdString moduleUnitId, moduleNameString moduleName)

newCompilerStore :: CompilerConfig a -> IO (CompilerStore a)
newCompilerStore CompilerConfig {..} =
  pure
    CompilerStore
    { modulePut =
        \mod_key x ->
          let fn = tofn mod_key
          in do createDirectoryIfMissing True $ takeDirectory fn
                rawPut fn x
    }
  where
    tofn mod_key = topdir </> k0 </> f k1 <.> ext
      where
        (k0, k1) = moduleKey mod_key
        f p =
          case span (/= '.') p of
            (p0, "") -> p0
            (p0, '.':p1) -> p0 </> f p1
            _ -> error "Impossible happened in tofn"
