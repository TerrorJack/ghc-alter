{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.Compiler
  ( IR(..)
  , CompilerConfig(..)
  , CompilerStore(..)
  , moduleKey
  , newCompilerStore
  , Compiler(..)
  , toHooks
  ) where

import Cmm
import Control.Monad
import CoreSyn
import Data.Binary
import DriverPipeline
import FastString
import GHC.Conc
import Hooks
import HscTypes
import qualified Language.Haskell.GHC.Kit.Hooks.Frontend as F
import qualified Language.Haskell.GHC.Kit.Hooks.RunPhase as RP
import Module
import StgSyn
import qualified Stream
import System.Directory
import System.FilePath
import TcRnTypes

data IR = IR
  { tc :: TcGblEnv
  , core :: CgGuts
  , corePrep :: CoreProgram
  , stgFromCore, stg :: [StgTopBinding]
  , cmmFromStg, cmm :: Stream.Stream IO CmmGroup ()
  , cmmRaw :: Stream.Stream IO RawCmmGroup ()
  }

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

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
  x <- readTVar var
  writeTVar var $! f x

newCompilerStore :: Binary a => CompilerConfig a -> IO (CompilerStore a)
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
              x <- decodeFile $ tofn mod_key
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
                encodeFile fn x
    }
  where
    tofn mod_key = topdir </> k0 </> k1
      where
        (k0, k1) = moduleKey mod_key

newtype Compiler = Compiler
  { runCompiler :: ModSummary -> IR -> IO ()
  }

toHooks :: Compiler -> IO Hooks
toHooks Compiler {..} = do
  flag_set_ref <- newTVarIO emptyModuleSet
  tc_map_ref <- newTVarIO emptyModuleEnv
  core_map_ref <- newTVarIO emptyModuleEnv
  corePrep_map_ref <- newTVarIO emptyModuleEnv
  stgFromCore_map_ref <- newTVarIO emptyModuleEnv
  stg_map_ref <- newTVarIO emptyModuleEnv
  cmmFromStg_map_ref <- newTVarIO emptyModuleEnv
  cmm_map_ref <- newTVarIO emptyModuleEnv
  cmmRaw_map_ref <- newTVarIO emptyModuleEnv
  pure
    emptyHooks
    { runPhaseHook =
        Just $
        RP.runPhaseWith $
        RP.defaultRunPhase
        { RP.onleave =
            \phase_plus _ _ ->
              case phase_plus of
                HscOut _ _ (HscRecomp _ mod_summary) ->
                  let key = ms_mod mod_summary
                      read_map ref = do
                        m <- readTVar ref
                        case lookupModuleEnv m key of
                          Just v -> pure v
                          _ -> fail "Impossible happened in toHooks"
                  in join $
                     atomically $ do
                       flag_set <- readTVar flag_set_ref
                       if key `elemModuleSet` flag_set
                         then pure $ pure ()
                         else do
                           modifyTVar' flag_set_ref $ \flag_set' ->
                             extendModuleSet flag_set' key
                           ir <-
                             IR <$> read_map tc_map_ref <*>
                             read_map core_map_ref <*>
                             read_map corePrep_map_ref <*>
                             read_map stgFromCore_map_ref <*>
                             read_map stg_map_ref <*>
                             read_map cmmFromStg_map_ref <*>
                             read_map cmm_map_ref <*>
                             read_map cmmRaw_map_ref
                           pure $ runCompiler mod_summary ir
                _ -> pure ()
        , RP.core = write_map core_map_ref
        , RP.corePrep = write_map corePrep_map_ref
        , RP.stgFromCore = write_map stgFromCore_map_ref
        , RP.stg = write_map stg_map_ref
        , RP.cmmFromStg = write_map cmmFromStg_map_ref
        , RP.cmm = write_map cmm_map_ref
        , RP.cmmRaw = write_map cmmRaw_map_ref
        }
    , hscFrontendHook =
        Just $
        F.genericHscFrontendWith $
        F.defaultFrontend {F.onleave = write_map tc_map_ref}
    }
  where
    write_map ref mod_summary x =
      atomically $
      modifyTVar' ref $ \m -> extendModuleEnv m (ms_mod mod_summary) x
