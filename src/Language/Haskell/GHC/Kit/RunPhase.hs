{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.RunPhase where

import Control.Monad.IO.Class
import DriverPipeline
import DynFlags
import HscTypes

data RunPhaseTask = RunPhaseTask
  { logRunPhase :: PhasePlus -> FilePath -> DynFlags -> IO ()
  , coreHook :: ModSummary -> CgGuts -> IO ()
  }

defaultRunPhaseTask :: RunPhaseTask
defaultRunPhaseTask =
  RunPhaseTask {logRunPhase = \_ _ _ -> pure (), coreHook = \_ _ -> pure ()}

runPhaseWithTask ::
     RunPhaseTask
  -> PhasePlus
  -> FilePath
  -> DynFlags
  -> CompPipeline (PhasePlus, FilePath)
runPhaseWithTask RunPhaseTask {..} phase_plus input_fn dflags = do
  liftIO $ logRunPhase phase_plus input_fn dflags
  runPhase phase_plus input_fn dflags
