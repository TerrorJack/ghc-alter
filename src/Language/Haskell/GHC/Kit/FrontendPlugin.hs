{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.FrontendPlugin
  ( frontendPlugin
  ) where

import Control.Monad
import Control.Monad.IO.Class
import GHC
import GhcPlugins
import Language.Haskell.GHC.Kit.Compiler
import Language.Haskell.GHC.Kit.Mystery

frontendAction :: [String] -> [(String, Maybe Phase)] -> Ghc ()
frontendAction args targets = do
  liftIO $ putStrLn $ "args: " ++ show args
  h <- liftIO $ toHooks compiler
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags dflags {ghcMode = CompManager, hooks = h}
  sequenceA [guessTarget t f | (t, f) <- targets] >>= setTargets
  sf <- load LoadAllTargets
  case sf of
    Succeeded -> pure ()
    Failed -> fail "GHC.load returned Failed."

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {frontend = frontendAction}
