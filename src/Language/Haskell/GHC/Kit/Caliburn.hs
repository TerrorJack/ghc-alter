{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Kit.Caliburn
  ( initCompiler
  ) where

import HscTypes
import Language.Haskell.GHC.Kit.Compiler
import Language.Haskell.GHC.Kit.CompilerStore
import Language.Haskell.GHC.Kit.LoneWolf ()
import Outputable
import System.Directory
import System.FilePath
import Text.Show.Pretty

initCompiler :: IO Compiler
initCompiler = do
  pwd <- getCurrentDirectory
  let conf =
        CompilerConfig
        { topdir = pwd </> ".boot"
        , ext = "raw.txt"
        , rawGet = readFile
        , rawPut = writeFile
        }
  raw_store <- newCompilerStore conf
  pretty_store <- newCompilerStore conf {ext = "pretty.txt"}
  pure $
    Compiler $ \ModSummary {..} IR {..} -> do
      modulePut raw_store ms_mod (ppShow stg)
      modulePut pretty_store ms_mod (showSDocUnsafe $ ppr stg)
