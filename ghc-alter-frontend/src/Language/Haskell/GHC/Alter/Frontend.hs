{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Alter.Frontend
  ( Frontend
  , FrontendConfig(..)
  , toFrontendPlugin
  ) where

import Control.Monad.IO.Class
import GHC
import Plugins

type Frontend a = a -> [(String, Maybe Phase)] -> Ghc ()

newtype FrontendConfig a = FrontendConfig
  { frontendGet :: FilePath -> IO a
  }

toFrontendPlugin :: FrontendConfig a -> Frontend a -> FrontendPlugin
toFrontendPlugin FrontendConfig {..} f =
  defaultFrontendPlugin
  { frontend =
      \args targets ->
        case args of
          [path] -> do
            conf <- liftIO $ frontendGet path
            f conf targets
          _ ->
            fail
              "ghc-alter frontend expects a single argument, which is the file path of configuration"
  }
