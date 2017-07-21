{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.WithIRs
  ( IR(..)
  , toRunPhase
  ) where

import Cmm
import CoreSyn
import Data.IORef
import HscTypes
import qualified Language.Haskell.GHC.Kit.RunPhase as RP
import StgSyn
import qualified Stream

data IR = IR
  { core :: CgGuts
  , corePrep :: CoreProgram
  , stgFromCore, stg :: [StgTopBinding]
  , cmmFromStg, cmm :: Stream.Stream IO CmmGroup ()
  , cmmRaw :: Stream.Stream IO RawCmmGroup ()
  }

toRunPhase :: (ModSummary -> IR -> IO ()) -> IO RP.RunPhase
toRunPhase cont = do
  mod_summary_ref <- newIORef undefined
  core_ref <- newIORef undefined
  corePrep_ref <- newIORef undefined
  stgFromCore_ref <- newIORef undefined
  stg_ref <- newIORef undefined
  cmmFromStg_ref <- newIORef undefined
  cmm_ref <- newIORef undefined
  cmmRaw_ref <- newIORef undefined
  pure
    RP.defaultRunPhase
    { RP.core =
        \mod_summary cgguts ->
          writeIORef mod_summary_ref mod_summary *> writeIORef core_ref cgguts
    , RP.corePrep = fill_ref corePrep_ref
    , RP.stgFromCore = fill_ref stgFromCore_ref
    , RP.stg = fill_ref stg_ref
    , RP.cmmFromStg = fill_ref cmmFromStg_ref
    , RP.cmm = fill_ref cmm_ref
    , RP.cmmRaw = fill_ref cmmRaw_ref
    , RP.onleave =
        \_ _ _ -> do
          mod_summary <- readIORef mod_summary_ref
          ir <-
            IR <$> readIORef core_ref <*> readIORef corePrep_ref <*>
            readIORef stgFromCore_ref <*>
            readIORef stg_ref <*>
            readIORef cmmFromStg_ref <*>
            readIORef cmm_ref <*>
            readIORef cmmRaw_ref
          cont mod_summary ir
    }
  where
    fill_ref ref _ = writeIORef ref
