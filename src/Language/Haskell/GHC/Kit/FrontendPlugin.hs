{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.FrontendPlugin
  ( frontendPlugin
  ) where

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

runPhaseOpts :: RunPhase
runPhaseOpts =
  defaultRunPhase
  { onenter =
      \phase_plus input_fn _ ->
        putStrLn $
        "runPhase " ++
        showSDocUnsafe (ppr phase_plus) ++ ", input filename: " ++ show input_fn
  , core = coreAction
  , corePrep = \_ _ -> putStrLn "corePrep"
  , stgFromCore = \_ _ -> putStrLn "stgFromCore"
  , stg = \_ _ -> putStrLn "stg"
  , cmmFromStg = \_ _ -> putStrLn "cmmFromStg"
  , cmm = \_ _ -> putStrLn "cmm"
  , cmmRaw = \_ _ -> putStrLn "cmmRaw"
  }

frontendAction :: [String] -> [(String, Maybe Phase)] -> Ghc ()
frontendAction args targets = do
  liftIO $ putStrLn $ "args: " ++ show args
  dflags <- getSessionDynFlags
  void $
    setSessionDynFlags
      dflags
      { ghcMode = CompManager
      , hooks = emptyHooks {runPhaseHook = Just $ runPhaseWith runPhaseOpts}
      }
  sequenceA [guessTarget t f | (t, f) <- targets] >>= setTargets
  sf <- load LoadAllTargets
  case sf of
    Succeeded -> pure ()
    Failed -> fail "GHC.load returned Failed."

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {frontend = frontendAction}
