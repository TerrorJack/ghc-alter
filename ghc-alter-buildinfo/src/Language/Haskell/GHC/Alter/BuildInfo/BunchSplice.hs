{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.GHC.Alter.BuildInfo.BunchSplice
  ( buildInfoQ
  ) where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.LocalBuildInfo
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

buildInfoQ :: Q (TExp (Args, ConfigFlags, PackageDescription, LocalBuildInfo))
buildInfoQ = do
  lbs <- qRunIO $ LBS.readFile ".buildinfo"
  pure $
    TExp $
    SigE
      (AppE (VarE 'decode) (liftLazyByteString lbs))
      (AppT
         (AppT
            (AppT (AppT (TupleT 4) (ConT ''Args)) (ConT ''ConfigFlags))
            (ConT ''PackageDescription))
         (ConT ''LocalBuildInfo))
