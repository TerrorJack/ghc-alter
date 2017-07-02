{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.GHC.Kit.BuildInfo where

import Distribution.Simple.Compiler
import Language.Haskell.GHC.Kit.BuildInfo.Splices

bindir :: FilePath
bindir = $(bindirQ)

libdir :: FilePath
libdir = $(libdirQ)

datadir :: FilePath
datadir = $(datadirQ)

ghc :: FilePath
ghc = $(ghcQ)

ghcPkg :: FilePath
ghcPkg = $(ghcPkgQ)

ghcLibdir :: FilePath
ghcLibdir = $(ghcLibdirQ)

pkgDbStack :: PackageDBStack
pkgDbStack = $(pkgDbStackQ)
