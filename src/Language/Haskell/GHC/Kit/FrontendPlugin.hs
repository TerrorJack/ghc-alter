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
import Language.Haskell.GHC.Kit.Compiler
import Language.Haskell.GHC.Kit.RabbitHole
import Language.Haskell.GHC.Kit.RunPhase (runPhaseWith)

frontendAction :: [String] -> [(String, Maybe Phase)] -> Ghc ()
frontendAction args targets = do
  liftIO $ putStrLn $ "args: " ++ show args
  db <- liftIO $ newCompilerSession $ CompilerConfig "../../.boot/compile-to"
  rp <- liftIO $ toRunPhase db rabbitHole
  dflags <- getSessionDynFlags
  void $
    setSessionDynFlags
      dflags
      { ghcMode = CompManager
      , hooks = emptyHooks {runPhaseHook = Just $ runPhaseWith rp}
      }
  sequenceA [guessTarget t f | (t, f) <- targets] >>= setTargets
  sf <- load LoadAllTargets
  case sf of
    Succeeded -> pure ()
    Failed -> fail "GHC.load returned Failed."

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {frontend = frontendAction}
