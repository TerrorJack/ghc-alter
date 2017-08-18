{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Binary
import Data.Char
import Data.Foldable
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.InstallDirs
       (CopyDest(NoCopyDest), InstallDirs(..))
import Distribution.Simple.LocalBuildInfo
       (absoluteInstallDirs, localPkgDescr, withPackageDB, withPrograms)
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Types.PackageDescription
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
    { postConf =
        \_args flags pkg_descr lbi -> do
          let installdirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
          let dump k = encodeFile $ k <.> "buildinfo"
          for_ [("bindir", bindir), ("libdir", libdir), ("datadir", datadir)] $ \(k, v) ->
            dump k $ v installdirs
          let conf p = fromJust $ lookupProgram p $ withPrograms lbi
          let ghc = conf ghcProgram
          let ghcPkg = conf ghcPkgProgram
          for_ [("ghc", ghc), ("ghc-pkg", ghcPkg)] $ \(k, v) ->
            dump k $ programPath v
          ghcLibdir <-
            getProgramOutput
              (fromFlag (configVerbosity flags))
              ghc
              ["--print-libdir"]
          dump "ghc-libdir" $ takeWhile (not . isSpace) ghcLibdir
          dump "pkgdbstack" $ withPackageDB lbi
          dump "pkgname" $ unPackageName $ pkgName $ package $ localPkgDescr lbi
          postConf simpleUserHooks _args flags pkg_descr lbi
    }
