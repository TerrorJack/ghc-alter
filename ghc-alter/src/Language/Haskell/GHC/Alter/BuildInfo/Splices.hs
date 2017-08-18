{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Alter.BuildInfo.Splices
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
import GHC.Exts
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath

deriving instance Lift PackageDB

as :: Proxy# a -> a -> a
as _ = id

anyQ :: (Binary a, Lift a) => Proxy# a -> FilePath -> Q (TExp a)
anyQ p k = do
  s <-
    qRunIO $ do
      pwd_sub <- doesFileExist "Setup.hs"
      decodeFile $
        let bi = k <.> "buildinfo"
        in if pwd_sub
             then bi
             else "ghc-alter" </> bi
  unsafeTExpCoerce $ lift $ as p s

dirQ :: FilePath -> Q (TExp FilePath)
dirQ = anyQ (proxy# :: Proxy# FilePath)

bindirQ :: Q (TExp FilePath)
bindirQ = dirQ "bindir"

libdirQ :: Q (TExp FilePath)
libdirQ = dirQ "libdir"

datadirQ :: Q (TExp FilePath)
datadirQ = dirQ "datadir"

ghcQ :: Q (TExp FilePath)
ghcQ = dirQ "ghc"

ghcPkgQ :: Q (TExp FilePath)
ghcPkgQ = dirQ "ghc-pkg"

ghcLibdirQ :: Q (TExp FilePath)
ghcLibdirQ = dirQ "ghc-libdir"

pkgDbStackQ :: Q (TExp PackageDBStack)
pkgDbStackQ = anyQ (proxy# :: Proxy# PackageDBStack) "pkgdbstack"

pkgNameQ :: Q (TExp FilePath)
pkgNameQ = dirQ "pkgname"
