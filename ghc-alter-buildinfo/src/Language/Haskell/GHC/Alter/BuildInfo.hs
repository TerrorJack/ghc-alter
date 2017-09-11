{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.GHC.Alter.BuildInfo
  ( ghc
  , ghcPkg
  , ghcLibDir
  , pkgDbStack
  ) where

import qualified Distribution.Simple.Compiler as Cabal
import qualified Distribution.Simple.Program as Cabal
import Language.Haskell.GHC.Alter.BuildInfo.Splices.Untyped

ghc :: FilePath
{-# NOINLINE ghc #-}
ghc = Cabal.programPath $(ghcQ)

ghcPkg :: FilePath
{-# NOINLINE ghcPkg #-}
ghcPkg = Cabal.programPath $(ghcPkgQ)

ghcLibDir :: FilePath
{-# NOINLINE ghcLibDir #-}
ghcLibDir = $(ghcLibDirQ)

pkgDbStack :: Cabal.PackageDBStack
{-# NOINLINE pkgDbStack #-}
pkgDbStack = $(packageDbStackQ)
