{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.Boot where

import qualified Language.Haskell.GHC.Kit.BuildInfo as P
import Language.Haskell.GHC.Kit.Utils.Shell
import System.Directory
import System.FilePath

data BootLibTask = BootLibTask
  { lib, ghc :: FilePath
  , ghcOpts, confOpts :: [String]
  }

bootLib :: BootLibTask -> Shell ()
bootLib BootLibTask {..} = do
  proc ghc [lib </> "Setup.hs"]
  withCd lib $ do
    proc (lib </> "Setup") $
      ["configure", "--with-ghc=" ++ ghc] ++
      ["--ghc-options=" ++ opt | opt <- ghcOpts] ++ confOpts
    proc (lib </> "Setup") ["build"]

data BootTask = BootTask
  { top, ghc :: FilePath
  , ghcOpts, confOpts :: [String]
  }

defaultBootTask :: IO BootTask
defaultBootTask = do
  pwd <- getCurrentDirectory
  pure BootTask {top = pwd, ghc = P.ghc, ghcOpts = [], confOpts = []}

boot :: BootTask -> Shell ()
boot BootTask {..} = do
  bootLib $ BootLibTask (top </> "ghc-prim") ghc ghcOpts confOpts
  bootLib $ BootLibTask (top </> "integer-gmp") ghc ghcOpts confOpts
  bootLib $
    BootLibTask (top </> "base") ghc ghcOpts ("-finteger-gmp" : confOpts)
