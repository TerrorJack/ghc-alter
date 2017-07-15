{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.RunPhase where

import Cmm
import CmmBuildInfoTables
import CmmInfo
import CmmPipeline
import CodeOutput
import Control.Monad.IO.Class
import CorePrep
import CoreSyn
import CoreToStg
import Data.Functor
import DriverPipeline
import DynFlags
import ErrUtils
import HscTypes
import Outputable
import Platform
import ProfInit
import SimplStg
import StgCmm
import StgSyn
import Stream hiding (liftIO)
import TyCon
import UniqSupply

data RunPhaseTask = RunPhaseTask
  { logRunPhase :: PhasePlus -> FilePath -> DynFlags -> IO ()
  , coreHook :: ModSummary -> CgGuts -> IO ()
  , corePrepHook :: ModSummary -> CoreProgram -> IO ()
  , stgFromCoreHook, stgHook :: ModSummary -> [StgTopBinding] -> IO ()
  , cmmFromStgHook, cmmHook :: ModSummary -> Stream IO CmmGroup () -> IO ()
  , cmmRawHook :: ModSummary -> Stream IO RawCmmGroup () -> IO ()
  }

defaultRunPhaseTask :: RunPhaseTask
defaultRunPhaseTask =
  RunPhaseTask
  { logRunPhase = \_ _ _ -> pure ()
  , coreHook = \_ _ -> pure ()
  , corePrepHook = \_ _ -> pure ()
  , stgFromCoreHook = \_ _ -> pure ()
  , stgHook = \_ _ -> pure ()
  , cmmFromStgHook = \_ _ -> pure ()
  , cmmHook = \_ _ -> pure ()
  , cmmRawHook = \_ _ -> pure ()
  }

runPhaseWithTask ::
     RunPhaseTask
  -> PhasePlus
  -> FilePath
  -> DynFlags
  -> CompPipeline (PhasePlus, FilePath)
runPhaseWithTask RunPhaseTask {..} phase_plus input_fn dflags' = do
  liftIO $ logRunPhase phase_plus input_fn dflags'
  case phase_plus of
    HscOut src_flavour _ (HscRecomp cgguts@CgGuts {..} mod_summary) -> do
      liftIO $ coreHook mod_summary cgguts
      let next_phase =
            hscPostBackendPhase dflags' src_flavour (hscTarget dflags')
      output_fn <- phaseOutputFilename next_phase
      PipeState {hsc_env} <- getPipeState
      liftIO $ do
        let CgGuts { cg_module = this_mod
                   , cg_binds = core_binds
                   , cg_tycons = tycons
                   , cg_foreign = foreign_stubs0
                   , cg_foreign_files = foreign_files
                   , cg_dep_pkgs = dependencies
                   , cg_hpc_info = hpc_info
                   } = cgguts
            dflags = hsc_dflags hsc_env
            location = ms_location mod_summary
            data_tycons = filter isDataTyCon tycons
        prepd_binds <-
          corePrepPgm hsc_env this_mod location core_binds data_tycons
        corePrepHook mod_summary prepd_binds
        let stg_binds' = coreToStg dflags this_mod prepd_binds
        stgFromCoreHook mod_summary stg_binds'
        (stg_binds, cost_centre_info) <- stg2stg dflags this_mod stg_binds'
        stgHook mod_summary stg_binds
        let prof_init = profilingInitCode this_mod cost_centre_info
            foreign_stubs = foreign_stubs0 `appendStubC` prof_init
        withTiming
          (pure dflags)
          (text "CodeGen" <+> brackets (ppr this_mod))
          (const ()) $ do
          cmms <-
            do let cmm_stream :: Stream IO CmmGroup ()
                   cmm_stream =
                     StgCmm.codeGen
                       dflags
                       this_mod
                       data_tycons
                       cost_centre_info
                       stg_binds
                       hpc_info
                   dump1 a = do
                     dumpIfSet_dyn
                       dflags
                       Opt_D_dump_cmm_from_stg
                       "Cmm produced by codegen"
                       (ppr a)
                     return a
                   ppr_stream1 = Stream.mapM dump1 cmm_stream
               cmmFromStgHook mod_summary cmm_stream
               us <- mkSplitUniqSupply 'S'
               let pipeline_stream
                     | gopt Opt_SplitObjs dflags ||
                         gopt Opt_SplitSections dflags ||
                         osSubsectionsViaSymbols
                           (platformOS (targetPlatform dflags)) =
                       let run_pipeline us_ cmmgroup' = do
                             let (topSRT', us') = initUs us_ emptySRT
                             (topSRT, cmmgroup) <-
                               cmmPipeline hsc_env topSRT' cmmgroup'
                             let srt
                                   | isEmptySRT topSRT = []
                                   | otherwise = srtToData topSRT
                             return (us', srt ++ cmmgroup)
                       in do _ <- Stream.mapAccumL run_pipeline us ppr_stream1
                             return ()
                     | otherwise =
                       let initTopSRT = initUs_ us emptySRT
                           run_pipeline = cmmPipeline hsc_env
                       in do topSRT <-
                               Stream.mapAccumL
                                 run_pipeline
                                 initTopSRT
                                 ppr_stream1
                             Stream.yield (srtToData topSRT)
               cmmHook mod_summary pipeline_stream
               let dump2 a = do
                     dumpIfSet_dyn dflags Opt_D_dump_cmm "Output Cmm" (ppr a)
                     return a
                   ppr_stream2 = Stream.mapM dump2 pipeline_stream
               return ppr_stream2
          rawcmms0 <- cmmToRawCmm dflags cmms
          cmmRawHook mod_summary rawcmms0
          let dump a = do
                dumpIfSet_dyn dflags Opt_D_dump_cmm_raw "Raw Cmm" (ppr a)
                return a
              rawcmms1 = Stream.mapM dump rawcmms0
          void $
            codeOutput
              dflags
              this_mod
              output_fn
              location
              foreign_stubs
              foreign_files
              dependencies
              rawcmms1
      runPhase phase_plus input_fn dflags'
    _ -> runPhase phase_plus input_fn dflags'
