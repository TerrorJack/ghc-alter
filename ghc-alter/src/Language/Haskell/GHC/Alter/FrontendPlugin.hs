module Language.Haskell.GHC.Alter.FrontendPlugin
  ( frontendPlugin
  ) where

import Control.Monad
import Control.Monad.IO.Class
import GHC
import GhcPlugins
import Hooks

frontendAction :: [String] -> [(String, Maybe Phase)] -> Ghc ()
frontendAction args targets = do
  liftIO $ putStrLn $ "args: " ++ show args
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags dflags {ghcMode = CompManager, hooks = emptyHooks}
  sequenceA [guessTarget t f | (t, f) <- targets] >>= setTargets
  sf <- load LoadAllTargets
  case sf of
    Succeeded -> pure ()
    Failed -> fail "GHC.load returned Failed."

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {frontend = frontendAction}
