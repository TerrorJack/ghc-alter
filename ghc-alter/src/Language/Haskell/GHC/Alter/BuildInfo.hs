{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.GHC.Alter.BuildInfo
  ( bindir
  , libdir
  , datadir
  , ghc
  , ghcPkg
  , ghcLibdir
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
bindir = $$(bindirQ)

libdir :: FilePath
libdir = $$(libdirQ)

datadir :: FilePath
datadir = $$(datadirQ)

ghc :: FilePath
ghc = Cabal.programPath $$(ghcQ)

ghcPkg :: FilePath
ghcPkg = Cabal.programPath $$(ghcPkgQ)

ghcLibdir :: FilePath
ghcLibdir = $$(ghcLibDirQ)

pkgDbStack :: Cabal.PackageDBStack
pkgDbStack = $$(packageDbStackQ)

pkgName :: String
pkgName = Cabal.unPackageName $ Cabal.pkgName $ Cabal.package $$(packageDescriptionQ)
