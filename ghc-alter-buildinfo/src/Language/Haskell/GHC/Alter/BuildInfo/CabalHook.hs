module Language.Haskell.GHC.Alter.BuildInfo.CabalHook
  ( withBuildInfo
  , defaultMainWithBuildInfo
  ) where

import Data.Binary
import Distribution.Simple

withBuildInfo :: UserHooks -> UserHooks
withBuildInfo hooks =
  hooks
  { postConf =
      \args flags pkg_descr lbi -> do
        encodeFile ".buildinfo" (args, flags, pkg_descr, lbi)
        postConf hooks args flags pkg_descr lbi
  }

defaultMainWithBuildInfo :: IO ()
defaultMainWithBuildInfo = defaultMainWithHooks $ withBuildInfo simpleUserHooks
