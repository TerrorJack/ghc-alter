{-# LANGUAGE LambdaCase #-}
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

data HookFlag
  = Unentered
  | Entered
  | Invoked

toRunPhase :: (ModSummary -> IR -> IO ()) -> IO RP.RunPhase
toRunPhase cont = do
  flag_ref <- newIORef Unentered
  mod_summary_ref <- new_ref
  core_ref <- new_ref
  corePrep_ref <- new_ref
  stgFromCore_ref <- new_ref
  stg_ref <- new_ref
  cmmFromStg_ref <- new_ref
  cmm_ref <- new_ref
  cmmRaw_ref <- new_ref
  pure
    RP.defaultRunPhase
    { RP.core =
        \mod_summary cgguts -> do
          modifyIORef'
            flag_ref
            (\case
               Unentered -> Entered
               Entered -> error "Impossible happened in toRunPhase"
               Invoked -> Invoked)
          writeIORef mod_summary_ref mod_summary
          writeIORef core_ref cgguts
    , RP.corePrep = fill_ref corePrep_ref
    , RP.stgFromCore = fill_ref stgFromCore_ref
    , RP.stg = fill_ref stg_ref
    , RP.cmmFromStg = fill_ref cmmFromStg_ref
    , RP.cmm = fill_ref cmm_ref
    , RP.cmmRaw = fill_ref cmmRaw_ref
    , RP.onleave =
        \_ _ _ -> do
          flag <- readIORef flag_ref
          case flag of
            Entered -> do
              mod_summary <- readIORef mod_summary_ref
              ir <-
                IR <$> readIORef core_ref <*> readIORef corePrep_ref <*>
                readIORef stgFromCore_ref <*>
                readIORef stg_ref <*>
                readIORef cmmFromStg_ref <*>
                readIORef cmm_ref <*>
                readIORef cmmRaw_ref
              cont mod_summary ir
              writeIORef flag_ref Invoked
            _ -> pure ()
    }
  where
    new_ref = newIORef undefined
    fill_ref ref _ = writeIORef ref
