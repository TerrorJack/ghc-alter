module Language.Haskell.GHC.Alter.FrontendPlugin
  ( frontendPlugin
  ) where

import Control.Monad
import Control.Monad.IO.Class
import GHC
import GhcPlugins
import Hooks
import Language.Haskell.GHC.Alter.Caliburn
import Language.Haskell.GHC.Alter.Compiler

frontendAction :: [String] -> [(String, Maybe Phase)] -> Ghc ()
frontendAction args targets = do
  liftIO $ putStrLn $ "args: " ++ show args
  dflags <- getSessionDynFlags
  h <- liftIO $ initCompiler >>= toHooks
  void $ setSessionDynFlags dflags {ghcMode = CompManager, hooks = h}
  sequenceA [guessTarget t f | (t, f) <- targets] >>= setTargets
  sf <- load LoadAllTargets
  case sf of
    Succeeded -> pure ()
    Failed -> fail "GHC.load returned Failed."

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {frontend = frontendAction}
