{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.RunPhase
  ( RunPhase(..)
  , defaultRunPhase
  , runPhaseWith
  ) where

import Cmm
import CmmBuildInfoTables
import CmmInfo
import CmmPipeline
import CodeOutput
import Control.Monad
import Control.Monad.IO.Class
import CorePrep
import CoreSyn
import CoreToStg
import CostCentre
import Data.Maybe
import DriverPhases
import DriverPipeline
import DynFlags
import ErrUtils
import GHC.Base (when)
import Hooks
import HscMain
import HscTypes hiding (Hsc)
import Module
import Outputable
import Panic
import PipelineMonad
import Platform
import ProfInit
import SimplStg
import StgCmm
import StgSyn
import qualified Stream
import SysTools
import System.FilePath
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

runHookedPhase ::
     PhasePlus -> FilePath -> DynFlags -> CompPipeline (PhasePlus, FilePath)
runHookedPhase pp input dflags =
  lookupHook runPhaseHook runPhase dflags pp input dflags

pipeLoop :: PhasePlus -> FilePath -> CompPipeline (DynFlags, FilePath)
pipeLoop phase input_fn = do
  env <- getPipeEnv
  dflags <- getDynFlags
  let happensBefore' = happensBefore dflags
      stopPhase = stop_phase env
  case phase of
    RealPhase realPhase
      | realPhase `eqPhase` stopPhase ->
        case output_spec env of
          Temporary -> return (dflags, input_fn)
          output -> do
            pst <- getPipeState
            final_fn <-
              liftIO $
              getOutputFilename
                stopPhase
                output
                (src_basename env)
                dflags
                stopPhase
                (maybe_loc pst)
            when (final_fn /= input_fn) $ do
              let msg = "Copying `" ++ input_fn ++ "' to `" ++ final_fn ++ "'"
                  line_prag =
                    Just ("{-# LINE 1 \"" ++ src_filename env ++ "\" #-}\n")
              liftIO $ copyWithHeader dflags msg line_prag input_fn final_fn
            return (dflags, final_fn)
      | not (realPhase `happensBefore'` stopPhase) ->
        panic
          ("pipeLoop: at phase " ++
           show realPhase ++ " but I wanted to stop at phase " ++ show stopPhase)
    _ -> do
      liftIO $ debugTraceMsg dflags 4 (text "Running phase" <+> ppr phase)
      (next_phase, output_fn) <- runHookedPhase phase input_fn dflags
      r <- pipeLoop next_phase output_fn
      case phase of
        HscOut {} ->
          whenGeneratingDynamicToo dflags $ do
            setDynFlags $ dynamicTooMkDynamicDynFlags dflags
            _ <- pipeLoop phase input_fn
            return ()
        _ -> return ()
      return r

runPipeline' ::
     PhasePlus
  -> HscEnv
  -> PipeEnv
  -> FilePath
  -> Maybe ModLocation
  -> [FilePath]
  -> IO (DynFlags, FilePath)
runPipeline' start_phase hsc_env env input_fn maybe_loc foreign_os = do
  let state = PipeState {hsc_env, maybe_loc, foreign_os = foreign_os}
  evalP (pipeLoop start_phase input_fn) env state

runPipeline ::
     Phase
  -> HscEnv
  -> (FilePath, Maybe PhasePlus)
  -> Maybe FilePath
  -> PipelineOutput
  -> Maybe ModLocation
  -> [FilePath]
  -> IO (DynFlags, FilePath)
runPipeline stop_phase hsc_env0 (input_fn, mb_phase) mb_basename output maybe_loc foreign_os = do
  let dflags0 = hsc_dflags hsc_env0
      dflags = dflags0 {dumpPrefix = Just (basename ++ ".")}
      hsc_env = hsc_env0 {hsc_dflags = dflags}
      (input_basename, suffix) = splitExtension input_fn
      suffix' = drop 1 suffix
      basename
        | Just b <- mb_basename = b
        | otherwise = input_basename
      start_phase = fromMaybe (RealPhase (startPhase suffix')) mb_phase
      isHaskell (RealPhase (Unlit _)) = True
      isHaskell (RealPhase (Cpp _)) = True
      isHaskell (RealPhase (HsPp _)) = True
      isHaskell (RealPhase (Hsc _)) = True
      isHaskell HscOut {} = True
      isHaskell _ = False
      isHaskellishFile = isHaskell start_phase
      env =
        PipeEnv
        { stop_phase
        , src_filename = input_fn
        , src_basename = basename
        , src_suffix = suffix'
        , output_spec = output
        }
  when (isBackpackishSuffix suffix') $
    throwGhcExceptionIO (UsageError ("use --backpack to process " ++ input_fn))
  let happensBefore' = happensBefore dflags
  case start_phase of
    RealPhase start_phase' ->
      unless
        (start_phase' `happensBefore'` stop_phase ||
         start_phase' `eqPhase` stop_phase) $
      throwGhcExceptionIO
        (UsageError ("cannot compile this file to desired target: " ++ input_fn))
    HscOut {} -> return ()
  debugTraceMsg dflags 4 (text "Running the pipeline")
  r <- runPipeline' start_phase hsc_env env input_fn maybe_loc foreign_os
  let dflags' = hsc_dflags hsc_env
  unless (platformOS (targetPlatform dflags') == OSMinGW32) $
    when isHaskellishFile $
    whenCannotGenerateDynamicToo dflags' $ do
      debugTraceMsg
        dflags'
        4
        (text "Running the pipeline again for -dynamic-too")
      let dflags'' = dynamicTooMkDynamicDynFlags dflags'
      hsc_env' <- newHscEnv dflags''
      _ <- runPipeline' start_phase hsc_env' env input_fn maybe_loc foreign_os
      return ()
  return r

compileForeign :: HscEnv -> ForeignSrcLang -> FilePath -> IO FilePath
compileForeign hsc_env lang stub_c = do
  let phase =
        case lang of
          LangC -> Cc
          LangCxx -> Ccxx
          LangObjc -> Cobjc
          LangObjcxx -> Cobjcxx
  (_, stub_o) <-
    runPipeline
      StopLn
      hsc_env
      (stub_c, Just (RealPhase phase))
      Nothing
      Temporary
      Nothing
      []
  return stub_o

compileStub :: HscEnv -> FilePath -> IO FilePath
compileStub hsc_env = compileForeign hsc_env LangC

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
        (outputFilename, mStub, foreign_files) <-
          liftIO $ hscGenHardCodeWith rp hsc_env' cgguts mod_summary output_fn
        stub_o <- liftIO (mapM (compileStub hsc_env') mStub)
        foreign_os <-
          liftIO $ mapM (uncurry (compileForeign hsc_env')) foreign_files
        setForeignOs (maybe [] return stub_o ++ foreign_os)
        return (RealPhase next_phase, outputFilename)
      _ -> runPhase phase_plus input_fn dflags
  liftIO $ onleave phase_plus input_fn dflags
  pure r
