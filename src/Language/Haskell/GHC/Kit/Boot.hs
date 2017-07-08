{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.Boot where

import Language.Haskell.GHC.Kit.Utils.Shell
import System.FilePath

data BootLibTask = BootLibTask
  { lib, ghc :: FilePath
  , ghcOpts, confOpts :: [String]
  }

bootLib :: BootLibTask -> Shell ()
bootLib BootLibTask {..} = do
  proc ghc [lib </> "Setup.hs"]
  withCd lib $ do
    proc "./Setup" $
      ["configure", "--with-ghc=" ++ ghc] ++
      ["--ghc-options=" ++ opt | opt <- ghcOpts] ++ confOpts
    proc "./Setup" ["build"]

data BootTask = BootTask
  { top, ghc :: FilePath
  , ghcOpts, confOpts :: [String]
  }

boot :: BootTask -> Shell ()
boot BootTask {..} = do
  bootLib $ BootLibTask (top </> "ghc-prim") ghc ghcOpts confOpts
  bootLib $ BootLibTask (top </> "integer-gmp") ghc ghcOpts confOpts
  bootLib $
    BootLibTask (top </> "base") ghc ghcOpts ("-finteger-gmp" : confOpts)
