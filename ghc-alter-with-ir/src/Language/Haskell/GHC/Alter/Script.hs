{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Alter.Script
  ( compile
  ) where

import Control.Monad.IO.Class
import Data.Functor
import qualified Data.Map.Strict as Map
import DynFlags
import GHC
import GHC.Conc
import Language.Haskell.GHC.Alter.BuildInfo
import Language.Haskell.GHC.Alter.Compiler

compile :: [FilePath] -> [ModuleName] -> IO (Map.Map ModuleName IR)
compile import_paths mod_names = do
  ir_map_ref <- newTVarIO Map.empty
  hooks <-
    toHooks $
    defaultCompiler
    { runCompiler =
        \ModSummary {..} ir ->
          atomically $ do
            ir_map <- readTVar ir_map_ref
            writeTVar ir_map_ref $! Map.insert (moduleName ms_mod) ir ir_map
    }
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just ghcLibDir) $ do
      dflags <- getSessionDynFlags
      void $
        setSessionDynFlags $
        Opt_ForceRecomp `setGeneralFlag'`
        dflags {importPaths = import_paths, hooks = hooks}
      setTargets [Target (TargetModule m) True Nothing | m <- mod_names]
      sf <- load LoadAllTargets
      case sf of
        Succeeded -> liftIO $ atomically $ readTVar ir_map_ref
        Failed -> fail "GHC.load returned Failed."
