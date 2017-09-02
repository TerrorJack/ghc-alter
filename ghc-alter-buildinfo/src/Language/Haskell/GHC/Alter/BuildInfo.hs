{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.GHC.Alter.BuildInfo
  ( bindir
  , libdir
  , datadir
  , ghc
  , ghcPkg
  , ghcLibDir
  , pkgDbStack
  , pkgName
  ) where

import qualified Distribution.Simple.Compiler as Cabal
import qualified Distribution.Simple.Program as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import Language.Haskell.GHC.Alter.BuildInfo.TypedSplices

bindir :: FilePath
{-# NOINLINE bindir #-}
bindir = $$(bindirQ)

libdir :: FilePath
{-# NOINLINE libdir #-}
libdir = $$(libdirQ)

datadir :: FilePath
{-# NOINLINE datadir #-}
datadir = $$(datadirQ)

ghc :: FilePath
{-# NOINLINE ghc #-}
ghc = Cabal.programPath $$(ghcQ)

ghcPkg :: FilePath
{-# NOINLINE ghcPkg #-}
ghcPkg = Cabal.programPath $$(ghcPkgQ)

ghcLibDir :: FilePath
{-# NOINLINE ghcLibDir #-}
ghcLibDir = $$(ghcLibDirQ)

pkgDbStack :: Cabal.PackageDBStack
{-# NOINLINE pkgDbStack #-}
pkgDbStack = $$(packageDbStackQ)

pkgName :: String
{-# NOINLINE pkgName #-}
pkgName = Cabal.unPackageName $ Cabal.pkgName $ Cabal.package $$(packageDescriptionQ)
