{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.Hooks.Frontend
  ( Frontend(..)
  , defaultFrontend
  , genericHscFrontendWith
  ) where

import Control.Monad.IO.Class
import HscMain
import HscTypes
import TcRnTypes

data Frontend = Frontend
  { onenter :: ModSummary -> IO ()
  , onleave :: ModSummary -> TcGblEnv -> IO ()
  }

defaultFrontend :: Frontend
defaultFrontend = Frontend {onenter = \_ -> pure (), onleave = \_ _ -> pure ()}

genericHscFrontendWith :: Frontend -> ModSummary -> Hsc FrontendResult
genericHscFrontendWith Frontend {..} mod_summary = do
  liftIO $ onenter mod_summary
  r <- hscFileFrontEnd mod_summary
  liftIO $ onleave mod_summary r
  pure $ FrontendTypecheck r
