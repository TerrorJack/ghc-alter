{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.GHCWrapper
  ( WrapperOptions(..)
  , wrapperMain
  ) where

import Distribution.Simple.Compiler
import Language.Haskell.GHC.Kit.BuildInfo
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
      ["--frontend", pluginModule, "-plugin-package", pluginPackage] ++ pkgdb
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
  let args' = subst opts args
  putStrLn $ "Original ghc args: " ++ show args
  putStrLn $ "Substituted ghc args: " ++ show args'
  callProcess ghc $ subst opts args
