{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.FrontendPlugin where

import Control.Monad.IO.Class
import Data.Functor
import GHC
import GhcPlugins
import Hooks
import Language.Haskell.GHC.Kit.RunPhase
import Language.Haskell.GHC.Kit.WalkAST

coreAction :: ModSummary -> CgGuts -> IO ()
coreAction ModSummary {..} CgGuts {..} = do
  putStrLn $ "Current Module: " ++ showSDocUnsafe (ppr ms_mod)
  let bs = do
        b <- cg_binds
        case b of
          NonRec v _ -> [v]
          Rec bs' -> [v | (v, _) <- bs']
  putStrLn $ "Top-level Binding Modules: " ++ showSDocUnsafe (ppr (extmods bs))
  let ms = extmods cg_binds
  putStrLn $ "Dependent Modules: " ++ showSDocUnsafe (ppr ms)

runPhaseTask :: RunPhaseTask
runPhaseTask =
  defaultRunPhaseTask
  { logRunPhase =
      \phase_plus input_fn _ ->
        putStrLn $
        "runPhase " ++
        showSDocUnsafe (ppr phase_plus) ++ ", input filename: " ++ show input_fn
  , coreHook = coreAction
  , corePrepHook = \_ _ -> putStrLn "corePrep"
  , stgFromCoreHook = \_ _ -> putStrLn "stgFromCore"
  , stgHook = \_ _ -> putStrLn "stg"
  , cmmFromStgHook = \_ _ -> putStrLn "cmmFromStg"
  , cmmHook = \_ _ -> putStrLn "cmm"
  , cmmRawHook = \_ _ -> putStrLn "cmmRaw"
  }

frontendAction :: [String] -> [(String, Maybe Phase)] -> Ghc ()
frontendAction args targets = do
  liftIO $ putStrLn $ "args: " ++ show args
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
