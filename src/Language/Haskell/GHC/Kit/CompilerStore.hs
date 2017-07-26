{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.CompilerStore
  ( CompilerConfig(..)
  , CompilerStore(..)
  , newCompilerStore
  , modifyTVar'
  ) where

import GHC.Conc
import Module
import System.Directory
import System.FilePath

data CompilerConfig a = CompilerConfig
  { topdir, ext :: FilePath
  , rawGet :: FilePath -> IO a
  , rawPut :: FilePath -> a -> IO ()
  }

data CompilerStore a = CompilerStore
  { moduleGet :: Module -> IO a
  , modulePut :: Module -> a -> IO ()
  }

moduleKey :: Module -> (FilePath, FilePath)
moduleKey Module {..} = (unitIdString moduleUnitId, moduleNameString moduleName)

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
  x <- readTVar var
  writeTVar var $! f x

newCompilerStore :: CompilerConfig a -> IO (CompilerStore a)
newCompilerStore CompilerConfig {..} = do
  cache_map_ref <- newTVarIO emptyModuleEnv
  pure
    CompilerStore
    { moduleGet =
        \mod_key -> do
          cache_map <- atomically $ readTVar cache_map_ref
          case lookupModuleEnv cache_map mod_key of
            Just x -> pure x
            _ -> do
              x <- rawGet $ tofn mod_key
              atomically $
                modifyTVar' cache_map_ref $ \cache_map' ->
                  extendModuleEnv cache_map' mod_key x
              pure x
    , modulePut =
        \mod_key x ->
          let fn = tofn mod_key
          in do atomically $
                  modifyTVar' cache_map_ref $ \cache_map' ->
                    extendModuleEnv cache_map' mod_key x
                createDirectoryIfMissing True $ takeDirectory fn
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
