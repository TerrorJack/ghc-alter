{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.FrontendPlugin where

import Data.Functor
import GHC
import GhcPlugins
import Hooks
import Language.Haskell.GHC.Kit.RunPhase
import Language.Haskell.GHC.Kit.WalkAST

coreAction :: ModSummary -> CgGuts -> IO ()
coreAction ModSummary {..} CgGuts {..} = do
  putStrLn $ "Module: " ++ showSDocUnsafe (ppr ms_mod)
  let vs = vars cg_binds
  putStrLn $ "Vars: " ++ showSDocUnsafe (ppr vs)

runPhaseTask :: RunPhaseTask
runPhaseTask = defaultRunPhaseTask {coreHook = coreAction}

frontendAction :: [String] -> [(String, Maybe Phase)] -> Ghc ()
frontendAction _ targets = do
  dflags <- getSessionDynFlags
  void $
    setSessionDynFlags
      dflags
      { ghcMode = CompManager
      , hooks = emptyHooks {runPhaseHook = Just $ runPhaseWithTask runPhaseTask}
      }
  sequenceA [guessTarget t f | (t, f) <- targets] >>= setTargets
  sf <- load LoadAllTargets
  case sf of
    Succeeded -> pure ()
    Failed -> fail "GHC.load returned Failed."

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {frontend = frontendAction}
