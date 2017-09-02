module Language.Haskell.GHC.Alter.BuildInfo.Splices.Untyped
  ( buildInfoQ
  , installDirsQ
  , prefixQ
  , bindirQ
  , libdirQ
  , dynlibdirQ
  , flibdirQ
  , libexecdirQ
  , includedirQ
  , datadirQ
  , docdirQ
  , mandirQ
  , htmldirQ
  , haddockdirQ
  , sysconfdirQ
  , packageDescriptionQ
  , configuredProgramQ
  , ghcQ
  , ghcPkgQ
  , packageDbStackQ
  , ghcLibDirQ
  ) where

import qualified Language.Haskell.GHC.Alter.BuildInfo.Splices.Typed
       as T
import Language.Haskell.TH.Syntax
import Distribution.Simple.InstallDirs
import Distribution.Simple.Program

buildInfoQ :: Q Exp
buildInfoQ = unTypeQ T.buildInfoQ

installDirsQ :: (InstallDirs FilePath -> FilePath) -> Q Exp
installDirsQ = unTypeQ . T.installDirsQ

prefixQ :: Q Exp
prefixQ = unTypeQ T.prefixQ

bindirQ :: Q Exp
bindirQ = unTypeQ T.bindirQ

libdirQ :: Q Exp
libdirQ = unTypeQ T.libdirQ

dynlibdirQ :: Q Exp
dynlibdirQ = unTypeQ T.dynlibdirQ

flibdirQ :: Q Exp
flibdirQ = unTypeQ T.flibdirQ

libexecdirQ :: Q Exp
libexecdirQ = unTypeQ T.libexecdirQ

includedirQ :: Q Exp
includedirQ = unTypeQ T.includedirQ

datadirQ :: Q Exp
datadirQ = unTypeQ T.datadirQ

docdirQ :: Q Exp
docdirQ = unTypeQ T.docdirQ

mandirQ :: Q Exp
mandirQ = unTypeQ T.mandirQ

htmldirQ :: Q Exp
htmldirQ = unTypeQ T.htmldirQ

haddockdirQ :: Q Exp
haddockdirQ = unTypeQ T.haddockdirQ

sysconfdirQ :: Q Exp
sysconfdirQ = unTypeQ T.sysconfdirQ

packageDescriptionQ :: Q Exp
packageDescriptionQ = unTypeQ T.packageDescriptionQ

configuredProgramQ :: Program -> Q Exp
configuredProgramQ = unTypeQ . T.configuredProgramQ

ghcQ :: Q Exp
ghcQ = unTypeQ T.ghcQ

ghcPkgQ :: Q Exp
ghcPkgQ = unTypeQ T.ghcPkgQ

packageDbStackQ :: Q Exp
packageDbStackQ = unTypeQ T.packageDbStackQ

ghcLibDirQ :: Q Exp
ghcLibDirQ = unTypeQ T.ghcLibDirQ
