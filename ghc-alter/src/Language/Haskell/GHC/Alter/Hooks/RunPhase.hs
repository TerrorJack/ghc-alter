{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Alter.Hooks.RunPhase
  ( RunPhase(..)
  , defaultRunPhase
  , runPhaseWith
  ) where

import Cmm
import CmmBuildInfoTables
import CmmInfo
import CmmPipeline
import CodeOutput
import Control.Monad.IO.Class
import CorePrep
import CoreSyn
import CoreToStg
import CostCentre
import DriverPipeline
import DynFlags
import ErrUtils
import HscMain
import HscTypes hiding (Hsc)
import Module
import Outputable
import PipelineMonad
import Platform
import ProfInit
import SimplStg
import StgCmm
import StgSyn
import qualified Stream
import TyCon
import UniqSupply

data RunPhase = RunPhase
  { onenter, onleave :: PhasePlus -> FilePath -> DynFlags -> IO ()
  , core :: ModSummary -> CgGuts -> IO ()
  , corePrep :: ModSummary -> CoreProgram -> IO ()
  , stgFromCore, stg :: ModSummary -> [StgTopBinding] -> IO ()
  , cmmFromStg, cmm :: ModSummary -> Stream.Stream IO CmmGroup () -> IO ()
  , cmmRaw :: ModSummary -> Stream.Stream IO RawCmmGroup () -> IO ()
  }

defaultRunPhase :: RunPhase
defaultRunPhase =
  RunPhase
  { onenter = three
  , onleave = three
  , core = two
  , corePrep = two
  , stgFromCore = two
  , stg = two
  , cmmFromStg = two
  , cmm = two
  , cmmRaw = two
  }
  where
    three _ = two
    two _ _ = pure ()

myCoreToStgWith ::
     RunPhase
  -> ModSummary
  -> DynFlags
  -> Module
  -> CoreProgram
  -> IO ([StgTopBinding], CollectedCCs)
myCoreToStgWith RunPhase {..} mod_summary dflags this_mod prepd_binds = do
  let stg_binds = coreToStg dflags this_mod prepd_binds
  stgFromCore mod_summary stg_binds
  (stg_binds2, cost_centre_info) <- stg2stg dflags this_mod stg_binds
  stg mod_summary stg_binds2
  return (stg_binds2, cost_centre_info)

doCodeGenWith ::
     RunPhase
  -> ModSummary
  -> HscEnv
  -> Module
  -> [TyCon]
  -> CollectedCCs
  -> [StgTopBinding]
  -> HpcInfo
  -> IO (Stream.Stream IO CmmGroup ())
doCodeGenWith RunPhase {..} mod_summary hsc_env this_mod data_tycons cost_centre_info stg_binds hpc_info = do
  let dflags = hsc_dflags hsc_env
  let cmm_stream :: Stream.Stream IO CmmGroup ()
      cmm_stream =
        StgCmm.codeGen
          dflags
          this_mod
          data_tycons
          cost_centre_info
          stg_binds
          hpc_info
  cmmFromStg mod_summary cmm_stream
  let dump1 a = do
        dumpIfSet_dyn
          dflags
          Opt_D_dump_cmm_from_stg
          "Cmm produced by codegen"
          (ppr a)
        return a
      ppr_stream1 = Stream.mapM dump1 cmm_stream
  us <- mkSplitUniqSupply 'S'
  let pipeline_stream
        | gopt Opt_SplitObjs dflags ||
            gopt Opt_SplitSections dflags ||
            osSubsectionsViaSymbols (platformOS (targetPlatform dflags)) =
          let run_pipeline us_ cmmgroup = do
                let (topSRT', us') = initUs us_ emptySRT
                (topSRT, cmmgroup') <- cmmPipeline hsc_env topSRT' cmmgroup
                let srt
                      | isEmptySRT topSRT = []
                      | otherwise = srtToData topSRT
                return (us', srt ++ cmmgroup')
          in do _ <- Stream.mapAccumL run_pipeline us ppr_stream1
                return ()
        | otherwise =
          let initTopSRT = initUs_ us emptySRT
              run_pipeline = cmmPipeline hsc_env
          in do topSRT <- Stream.mapAccumL run_pipeline initTopSRT ppr_stream1
                Stream.yield (srtToData topSRT)
  cmm mod_summary pipeline_stream
  let dump2 a = do
        dumpIfSet_dyn dflags Opt_D_dump_cmm "Output Cmm" (ppr a)
        return a
      ppr_stream2 = Stream.mapM dump2 pipeline_stream
  return ppr_stream2

hscGenHardCodeWith ::
     RunPhase
  -> HscEnv
  -> CgGuts
  -> ModSummary
  -> FilePath
  -> IO (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)])
hscGenHardCodeWith rp@RunPhase {..} hsc_env cgguts mod_summary output_filename = do
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
  prepd_binds <- corePrepPgm hsc_env this_mod location core_binds data_tycons
  liftIO $ corePrep mod_summary prepd_binds
  (stg_binds, cost_centre_info) <-
    myCoreToStgWith rp mod_summary dflags this_mod prepd_binds
  let prof_init = profilingInitCode this_mod cost_centre_info
      foreign_stubs = foreign_stubs0 `appendStubC` prof_init
  withTiming
    (pure dflags)
    (text "CodeGen" <+> brackets (ppr this_mod))
    (const ()) $ do
    cmms <-
      doCodeGenWith
        rp
        mod_summary
        hsc_env
        this_mod
        data_tycons
        cost_centre_info
        stg_binds
        hpc_info
    rawcmms0 <- cmmToRawCmm dflags cmms
    cmmRaw mod_summary rawcmms0
    let dump a = do
          dumpIfSet_dyn dflags Opt_D_dump_cmm_raw "Raw Cmm" (ppr a)
          return a
        rawcmms1 = Stream.mapM dump rawcmms0
    (output_filename', (_stub_h_exists, stub_c_exists), foreign_fps) <-
      codeOutput
        dflags
        this_mod
        output_filename
        location
        foreign_stubs
        foreign_files
        dependencies
        rawcmms1
    return (output_filename', stub_c_exists, foreign_fps)

runPhaseWith ::
     RunPhase
  -> PhasePlus
  -> FilePath
  -> DynFlags
  -> CompPipeline (PhasePlus, FilePath)
runPhaseWith rp@RunPhase {..} phase_plus input_fn dflags = do
  liftIO $ onenter phase_plus input_fn dflags
  r <-
    case phase_plus of
      HscOut src_flavour mod_name (HscRecomp cgguts mod_summary) -> do
        liftIO $ core mod_summary cgguts
        location <- getLocation src_flavour mod_name
        setModLocation location
        let hsc_lang = hscTarget dflags
            next_phase = hscPostBackendPhase dflags src_flavour hsc_lang
        output_fn <- phaseOutputFilename next_phase
        PipeState {hsc_env = hsc_env'} <- getPipeState
        (outputFilename, _, _) <-
          liftIO $ hscGenHardCodeWith rp hsc_env' cgguts mod_summary output_fn
        setForeignOs []
        return (RealPhase next_phase, outputFilename)
      _ -> runPhase phase_plus input_fn dflags
  liftIO $ onleave phase_plus input_fn dflags
  pure r
