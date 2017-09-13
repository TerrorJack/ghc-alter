{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Alter.Script
  ( compile
  ) where

import Control.Monad.IO.Class
import Data.Functor
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Traversable
import DynFlags
import GHC
import Language.Haskell.GHC.Alter.BuildInfo
import Language.Haskell.GHC.Alter.Compiler

compile ::
     [PackageDBFlag]
  -> [PackageFlag]
  -> [FilePath]
  -> [String]
  -> IO (Map.Map ModuleName IR)
compile pkgdbs pkgs import_paths mods = do
  ir_map_ref <- newIORef Map.empty
  hooks <-
    toHooks $
    defaultCompiler
    { withIR =
        \ModSummary {..} ir ->
          atomicModifyIORef' ir_map_ref $ \ir_map ->
            (Map.insert (moduleName ms_mod) ir ir_map, ())
    }
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just ghcLibDir) $ do
      dflags <- getSessionDynFlags
      void $
        setSessionDynFlags $
        Opt_ForceRecomp `setGeneralFlag'`
        dflags
        { importPaths = import_paths
        , hooks = hooks
        , packageDBFlags = pkgdbs
        , packageFlags = pkgs
        }
      for mods (`guessTarget` Nothing) >>= setTargets
      sf <- load LoadAllTargets
      case sf of
        Succeeded -> liftIO $ readIORef ir_map_ref
        Failed -> fail "GHC.load returned Failed."
