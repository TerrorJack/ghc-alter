{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.FrontendPlugin
  ( frontendPlugin
  ) where

import Bag
import Control.Monad
import Control.Monad.IO.Class
import GHC
import GhcPlugins
import Language.Haskell.GHC.Kit.Compiler
import Language.Haskell.GHC.Kit.CompilerStore
import qualified Stream
import TcRnTypes

frontendAction :: [String] -> [(String, Maybe Phase)] -> Ghc ()
frontendAction args targets = do
  liftIO $ putStrLn $ "args: " ++ show args
  CompilerStore {..} <-
    liftIO $
    newCompilerStore $ CompilerConfig "../../.boot/compile-to" "ghc-kit_o"
  h <-
    liftIO $
    toHooks $
    Compiler $ \ModSummary {..} IR {tc = TcGblEnv {..}, core = CgGuts {..}, ..} -> do
      modulePut ms_mod "233"
      s <- moduleGet ms_mod
      unless (s == "233") $ fail "No 233, wryyyyyyyy"
      putStrLn $ "Length of tcg_binds: " ++ show (lengthBag tcg_binds)
      putStrLn $ "Length of cg_binds: " ++ show (length cg_binds)
      putStrLn $ "Length of corePrep: " ++ show (length corePrep)
      putStrLn $ "Length of stgFromCore: " ++ show (length stgFromCore)
      putStrLn $ "Length of stg: " ++ show (length stg)
      cmmFromStg_list <- Stream.collect cmmFromStg
      putStrLn $ "Length of cmmFromStg: " ++ show (length cmmFromStg_list)
      cmm_list <- Stream.collect cmm
      putStrLn $ "Length of cmm: " ++ show (length cmm_list)
      cmmRaw_list <- Stream.collect cmmRaw
      putStrLn $ "Length of cmmRaw: " ++ show (length cmmRaw_list)
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags dflags {ghcMode = CompManager, hooks = h}
  sequenceA [guessTarget t f | (t, f) <- targets] >>= setTargets
  sf <- load LoadAllTargets
  case sf of
    Succeeded -> pure ()
    Failed -> fail "GHC.load returned Failed."

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {frontend = frontendAction}
