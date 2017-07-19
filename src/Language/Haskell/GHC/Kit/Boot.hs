{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.Boot
  ( BootTask(..)
  , defaultBootTask
  , bootLib
  , boot
  ) where

import qualified Language.Haskell.GHC.Kit.BuildInfo as P
import System.Directory
import System.Environment
import System.FilePath
import System.Process

data BootTask = BootTask
  { top, ghc :: FilePath
  , ghcOpts, confOpts :: [String]
  }

defaultBootTask :: IO BootTask
defaultBootTask = do
  pwd <- getCurrentDirectory
  pure BootTask {top = pwd, ghc = P.ghc, ghcOpts = [], confOpts = []}

bootLib :: BootTask -> IO ()
bootLib BootTask {..} = do
  callProcess P.ghc [top </> "Setup.hs"]
  withCurrentDirectory top $ do
    callProcess "./Setup" $
      ["configure", "--with-ghc=" ++ ghc] ++
      ["--ghc-options=" ++ opt | opt <- ghcOpts] ++ confOpts
    callProcess "./Setup" ["build"]

boot :: BootTask -> IO ()
boot BootTask {..} = do
  unsetEnv "GHC_PACKAGE_PATH"
  withCurrentDirectory top $ do
    bootLib $ BootTask "ghc-prim" ghc ghcOpts confOpts
    bootLib $ BootTask "integer-gmp" ghc ghcOpts confOpts
    bootLib $ BootTask "base" ghc ghcOpts ("-finteger-gmp" : confOpts)
