{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.GHC.Alter.BuildInfo.TypedSplices
  ( buildInfoQ
  , bindirQ
  , libdirQ
  , datadirQ
  , packageDescriptionQ
  , configuredProgramQ
  , ghcQ
  , ghcPkgQ
  , packageDbStackQ
  , ghcLibDirQ
  ) where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Char
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Types.PackageDescription
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

liftLazyByteString :: LBS.ByteString -> Exp
liftLazyByteString lbs =
  AppE
    (VarE 'LBS.fromStrict)
    (AppE
       (VarE 'unsafePerformIO)
       (AppE
          (AppE
             (VarE 'BS.unsafePackAddressLen)
             (LitE (IntegerL (fromIntegral (LBS.length lbs)))))
          (LitE (StringPrimL (LBS.unpack lbs)))))

liftTyped :: Binary a => a -> Q Type -> Q (TExp a)
liftTyped x qt = do
  t <- qt
  unsafeTExpCoerce $
    pure $
    SigE (AppE (VarE 'Data.Binary.decode) (liftLazyByteString $ encode x)) t

buildInfo :: Q (Args, ConfigFlags, PackageDescription, LocalBuildInfo)
buildInfo = decode <$> qRunIO (LBS.readFile ".buildinfo")

buildInfoQ :: Q (TExp (Args, ConfigFlags, PackageDescription, LocalBuildInfo))
buildInfoQ = do
  bi <- buildInfo
  liftTyped bi [t|(Args, ConfigFlags, PackageDescription, LocalBuildInfo)|]

installDirsQ :: Q (InstallDirs FilePath)
installDirsQ = do
  (_, _, pkg_descr, lbi) <- buildInfo
  pure $ absoluteInstallDirs pkg_descr lbi NoCopyDest

bindirQ :: Q (TExp FilePath)
bindirQ = do
  dirs <- installDirsQ
  liftTyped (bindir dirs) [t|FilePath|]

libdirQ :: Q (TExp FilePath)
libdirQ = do
  dirs <- installDirsQ
  liftTyped (libdir dirs) [t|FilePath|]

datadirQ :: Q (TExp FilePath)
datadirQ = do
  dirs <- installDirsQ
  liftTyped (datadir dirs) [t|FilePath|]

packageDescriptionQ :: Q (TExp PackageDescription)
packageDescriptionQ = do
  (_, _, _, lbi) <- buildInfo
  liftTyped (localPkgDescr lbi) [t|PackageDescription|]

configuredProgramQ :: Program -> Q (TExp ConfiguredProgram)
configuredProgramQ prog = do
  (_, _, _, lbi) <- buildInfo
  liftTyped
    (fromJust $ lookupProgram prog $ withPrograms lbi)
    [t|ConfiguredProgram|]

ghcQ :: Q (TExp ConfiguredProgram)
ghcQ = configuredProgramQ ghcProgram

ghcPkgQ :: Q (TExp ConfiguredProgram)
ghcPkgQ = configuredProgramQ ghcPkgProgram

packageDbStackQ :: Q (TExp PackageDBStack)
packageDbStackQ = do
  (_, _, _, lbi) <- buildInfo
  liftTyped (withPackageDB lbi) [t|PackageDBStack|]

ghcLibDirQ :: Q (TExp FilePath)
ghcLibDirQ = do
  (_, flags, _, lbi) <- buildInfo
  raw_libdir <-
    qRunIO $
    getProgramOutput
      (fromFlag $ configVerbosity flags)
      (fromJust $ lookupProgram ghcProgram $ withPrograms lbi)
      ["--print-libdir"]
  liftTyped (takeWhile (not . isSpace) raw_libdir) [t|FilePath|]
