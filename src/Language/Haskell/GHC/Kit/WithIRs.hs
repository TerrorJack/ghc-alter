{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.WithIRs
  ( IR(..)
  , toRunPhase
  ) where

import Cmm
import Control.Monad
import CoreSyn
import DriverPipeline
import GHC.Conc
import HscTypes
import qualified Language.Haskell.GHC.Kit.RunPhase as RP
import Module
import StgSyn
import qualified Stream

data IR = IR
  { core :: CgGuts
  , corePrep :: CoreProgram
  , stgFromCore, stg :: [StgTopBinding]
  , cmmFromStg, cmm :: Stream.Stream IO CmmGroup ()
  , cmmRaw :: Stream.Stream IO RawCmmGroup ()
  }

toRunPhase :: (ModSummary -> IR -> IO ()) -> IO RP.RunPhase
toRunPhase cont = do
  flag_set_ref <- newTVarIO emptyModuleSet
  core_map_ref <- newTVarIO emptyModuleEnv
  corePrep_map_ref <- newTVarIO emptyModuleEnv
  stgFromCore_map_ref <- newTVarIO emptyModuleEnv
  stg_map_ref <- newTVarIO emptyModuleEnv
  cmmFromStg_map_ref <- newTVarIO emptyModuleEnv
  cmm_map_ref <- newTVarIO emptyModuleEnv
  cmmRaw_map_ref <- newTVarIO emptyModuleEnv
  pure
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
                      _ -> fail "Impossible happened in toRunPhase"
              in join $
                 atomically $ do
                   flag_set <- readTVar flag_set_ref
                   if key `elemModuleSet` flag_set
                     then pure $ pure ()
                     else do
                       writeTVar flag_set_ref $ extendModuleSet flag_set key
                       ir <-
                         IR <$> read_map core_map_ref <*>
                         read_map corePrep_map_ref <*>
                         read_map stgFromCore_map_ref <*>
                         read_map stg_map_ref <*>
                         read_map cmmFromStg_map_ref <*>
                         read_map cmm_map_ref <*>
                         read_map cmmRaw_map_ref
                       pure $ cont mod_summary ir
            _ -> pure ()
    , RP.core = write_map core_map_ref
    , RP.corePrep = write_map corePrep_map_ref
    , RP.stgFromCore = write_map stgFromCore_map_ref
    , RP.stg = write_map stg_map_ref
    , RP.cmmFromStg = write_map cmmFromStg_map_ref
    , RP.cmm = write_map cmm_map_ref
    , RP.cmmRaw = write_map cmmRaw_map_ref
    }
  where
    write_map ref mod_summary x =
      atomically $ do
        m <- readTVar ref
        writeTVar ref $ extendModuleEnv m (ms_mod mod_summary) x
