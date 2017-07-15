{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Kit.GHCWrapper where

import Distribution.Simple.Compiler
import Language.Haskell.GHC.Kit.BuildInfo
import System.Environment
import System.Process

newtype WrapperOptions = WrapperOptions
  { pluginModule :: String
  }

subst :: WrapperOptions -> [String] -> [String]
subst WrapperOptions {..} args = do
  arg <- args
  case arg of
    "--make" -> ["--frontend", pluginModule] ++ pkgdb
    _ -> [arg]
  where
    pkgdb =
      case last pkgDbStack of
        GlobalPackageDB -> ["-global-package-db"]
        UserPackageDB -> ["-user-package-db"]
        SpecificPackageDB p -> ["-package-db", p]

wrapperMain :: WrapperOptions -> IO ()
wrapperMain opts = do
  args <- getArgs
  callProcess ghc $ subst opts args
