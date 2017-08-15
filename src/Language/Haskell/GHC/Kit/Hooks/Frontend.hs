{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.Hooks.Frontend
  ( Frontend(..)
  , defaultFrontend
  , genericHscFrontendWith
  ) where

import Control.Monad.IO.Class
import DynFlags
import FastString
import Fingerprint
import HscMain
import HscTypes
import Module
import SrcLoc
import TcBackpack
import TcRnTypes

data Frontend = Frontend
  { onenter :: ModSummary -> IO ()
  , onleave :: ModSummary -> TcGblEnv -> IO ()
  , parsed :: ModSummary -> HsParsedModule -> IO ()
  }

defaultFrontend :: Frontend
defaultFrontend =
  Frontend
  {onenter = \_ -> pure (), onleave = \_ _ -> pure (), parsed = \_ _ -> pure ()}

hscSimpleIface_ ::
     HscEnv -> TcGblEnv -> Maybe Fingerprint -> IO (ModIface, Bool, ModDetails)
hscSimpleIface_ hsc_env tc_result mb_old_iface =
  runHsc hsc_env $ hscSimpleIface' tc_result mb_old_iface

hscTypecheckWith ::
     Frontend -> Bool -> ModSummary -> Maybe HsParsedModule -> Hsc TcGblEnv
hscTypecheckWith Frontend {..} keep_rn mod_summary mb_rdr_module = do
  hsc_env <- getHscEnv
  let hsc_src = ms_hsc_src mod_summary
      dflags = hsc_dflags hsc_env
      outer_mod = ms_mod mod_summary
      mod_name = moduleName outer_mod
      outer_mod' = mkModule (thisPackage dflags) mod_name
      inner_mod = canonicalizeHomeModule dflags mod_name
      src_filename = ms_hspp_file mod_summary
      real_loc = realSrcLocSpan $ mkRealSrcLoc (mkFastString src_filename) 1 1
  if hsc_src == HsigFile && not (isHoleModule inner_mod)
    then ioMsgMaybe $ tcRnInstantiateSignature hsc_env outer_mod' real_loc
    else do
      hpm <-
        case mb_rdr_module of
          Just hpm -> return hpm
          Nothing -> hscParse' mod_summary
      liftIO $ parsed mod_summary hpm
      tc_result0 <- tcRnModule' hsc_env mod_summary keep_rn hpm
      if hsc_src == HsigFile
        then do
          (iface, _, _) <- liftIO $ hscSimpleIface_ hsc_env tc_result0 Nothing
          ioMsgMaybe $ tcRnMergeSignatures hsc_env hpm tc_result0 iface
        else return tc_result0

hscFileFrontEndWith :: Frontend -> ModSummary -> Hsc TcGblEnv
hscFileFrontEndWith f mod_summary = hscTypecheckWith f False mod_summary Nothing

genericHscFrontendWith :: Frontend -> ModSummary -> Hsc FrontendResult
genericHscFrontendWith f@Frontend {..} mod_summary = do
  liftIO $ onenter mod_summary
  r <- hscFileFrontEndWith f mod_summary
  liftIO $ onleave mod_summary r
  pure $ FrontendTypecheck r
