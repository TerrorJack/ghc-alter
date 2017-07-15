{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Kit.BuildInfo.Splices
  ( bindirQ
  , libdirQ
  , datadirQ
  , ghcQ
  , ghcPkgQ
  , ghcLibdirQ
  , pkgDbStackQ
  , pkgNameQ
  ) where

import Data.Binary
import Distribution.Simple.Compiler
import Language.Haskell.TH.Syntax

deriving instance Lift PackageDB

dirQ :: FilePath -> Q Exp
dirQ k = do
  s <- qRunIO $ decodeFile $ k ++ ".buildinfo"
  lift (s :: FilePath)

bindirQ :: Q Exp
bindirQ = dirQ "bindir"

libdirQ :: Q Exp
libdirQ = dirQ "libdir"

datadirQ :: Q Exp
datadirQ = dirQ "datadir"

ghcQ :: Q Exp
ghcQ = dirQ "ghc"

ghcPkgQ :: Q Exp
ghcPkgQ = dirQ "ghc-pkg"

ghcLibdirQ :: Q Exp
ghcLibdirQ = dirQ "ghc-libdir"

pkgDbStackQ :: Q Exp
pkgDbStackQ = do
  pkgdb <- qRunIO $ decodeFile "pkgdbstack.buildinfo"
  lift (pkgdb :: PackageDBStack)

pkgNameQ :: Q Exp
pkgNameQ = dirQ "pkgname"
