{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.GHC.Kit.Boot where

import qualified Language.Haskell.GHC.Kit.BuildInfo as P
import Language.Haskell.GHC.Kit.Utils.Shell
import System.Directory
import System.FilePath

data BootTask = BootTask
  { top, ghc :: FilePath
  , ghcOpts, confOpts :: [String]
  }

defaultBootTask :: IO BootTask
defaultBootTask = do
  pwd <- getCurrentDirectory
  pure BootTask {top = pwd, ghc = P.ghc, ghcOpts = [], confOpts = []}

bootLib :: BootTask -> Shell ()
bootLib BootTask {..} = do
  proc P.ghc [top </> "Setup.hs"]
  withCd top $ do
    proc (top </> "Setup") $
      ["configure", "--with-ghc=" ++ ghc] ++
      ["--ghc-options=" ++ opt | opt <- ghcOpts] ++ confOpts
    proc (top </> "Setup") ["build"]

boot :: BootTask -> Shell ()
boot BootTask {..} = do
  bootLib $ BootTask (top </> "ghc-prim") ghc ghcOpts confOpts
  bootLib $ BootTask (top </> "integer-gmp") ghc ghcOpts confOpts
  bootLib $ BootTask (top </> "base") ghc ghcOpts ("-finteger-gmp" : confOpts)
