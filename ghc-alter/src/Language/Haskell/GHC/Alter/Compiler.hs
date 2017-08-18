{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Alter.Compiler
  ( IR(..)
  , Compiler(..)
  , defaultCompiler
  , toHooks
  ) where

import Cmm
import Control.Monad
import CoreSyn
import DriverPipeline
import GHC.Conc
import Hooks
import HscTypes
import qualified Language.Haskell.GHC.Alter.Hooks.Frontend as F
import qualified Language.Haskell.GHC.Alter.Hooks.RunPhase as RP
import Module
import StgSyn
import qualified Stream
import TcRnTypes

data IR = IR
  { parsed :: HsParsedModule
  , tc :: TcGblEnv
  , core :: CgGuts
  , corePrep :: CoreProgram
  , stgFromCore, stg :: [StgTopBinding]
  , cmmFromStg, cmm :: [CmmGroup]
  , cmmRaw :: [RawCmmGroup]
  }

data Compiler = Compiler
  { patch :: ModSummary -> HsParsedModule -> IO HsParsedModule
  , runCompiler :: ModSummary -> IR -> IO ()
  }

defaultCompiler :: Compiler
defaultCompiler = Compiler {patch = const pure, runCompiler = \_ _ -> pure ()}

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
  x <- readTVar var
  writeTVar var $! f x

toHooks :: Compiler -> IO Hooks
toHooks Compiler {..} = do
  flag_set_ref <- newTVarIO emptyModuleSet
  parsed_map_ref <- newTVarIO emptyModuleEnv
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
                      del_map ref = modifyTVar' ref $ \m -> delModuleEnv m key
                  in join $
                     atomically $ do
                       flag_set <- readTVar flag_set_ref
                       if key `elemModuleSet` flag_set
                         then pure $ pure ()
                         else do
                           modifyTVar' flag_set_ref $ \flag_set' ->
                             extendModuleSet flag_set' key
                           ir <-
                             IR <$> read_map parsed_map_ref <*>
                             read_map tc_map_ref <*>
                             read_map core_map_ref <*>
                             read_map corePrep_map_ref <*>
                             read_map stgFromCore_map_ref <*>
                             read_map stg_map_ref <*>
                             read_map cmmFromStg_map_ref <*>
                             read_map cmm_map_ref <*>
                             read_map cmmRaw_map_ref
                           pure $ do
                             runCompiler mod_summary ir
                             atomically $ do
                               del_map parsed_map_ref
                               del_map tc_map_ref
                               del_map core_map_ref
                               del_map corePrep_map_ref
                               del_map stgFromCore_map_ref
                               del_map stg_map_ref
                               del_map cmmFromStg_map_ref
                               del_map cmm_map_ref
                               del_map cmmRaw_map_ref
                _ -> pure ()
        , RP.core = write_map core_map_ref
        , RP.corePrep = write_map corePrep_map_ref
        , RP.stgFromCore = write_map stgFromCore_map_ref
        , RP.stg = write_map stg_map_ref
        , RP.cmmFromStg = write_map_stream cmmFromStg_map_ref
        , RP.cmm = write_map_stream cmm_map_ref
        , RP.cmmRaw = write_map_stream cmmRaw_map_ref
        }
    , hscFrontendHook =
        Just $
        F.genericHscFrontendWith $
        F.defaultFrontend
        { F.onleave = write_map tc_map_ref
        , F.parsed =
            \mod_summary hpm -> do
              r <- patch mod_summary hpm
              atomically $
                modifyTVar' parsed_map_ref $ \m ->
                  extendModuleEnv m (ms_mod mod_summary) r
              pure r
        }
    }
  where
    write_map ref mod_summary x =
      atomically $
      modifyTVar' ref $ \m -> extendModuleEnv m (ms_mod mod_summary) x
    write_map_stream ref mod_summary x =
      Stream.collect x >>= write_map ref mod_summary
