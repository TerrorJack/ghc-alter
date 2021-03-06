{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Alter.GHCWrapper
  ( WrapperOptions(..)
  , wrapperMain
  ) where

import Distribution.Simple.Compiler
import Language.Haskell.GHC.Alter.BuildInfo
import System.Environment
import System.Process

data WrapperOptions = WrapperOptions
  { pluginModule, pluginPackage :: String
  }

subst :: WrapperOptions -> [String] -> [String]
subst WrapperOptions {..} args = do
  arg <- args
  case arg of
    "--make" ->
      ["--frontend", pluginModule, "-plugin-package", pluginPackage] ++
      pkgdb (last $ init pkgDbStack) ++ pkgdb (last pkgDbStack)
    _ -> [arg]
  where
    pkgdb entry =
      case entry of
        GlobalPackageDB -> ["-global-package-db"]
        UserPackageDB -> ["-user-package-db"]
        SpecificPackageDB p -> ["-package-db", p]

wrapperMain :: WrapperOptions -> IO ()
wrapperMain opts = do
  args <- getArgs
  callProcess ghc $ subst opts args
