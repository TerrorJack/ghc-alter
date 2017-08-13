{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Kit.Caliburn
  ( initCompiler
  ) where

import HscTypes
import Language.Haskell.GHC.Kit.Compiler
import Language.Haskell.GHC.Kit.CompilerStore
import Language.Haskell.GHC.Kit.LoneWolf ()
import Outputable
import qualified Stream
import System.Directory
import System.FilePath
import Text.Show.Pretty

initCompiler :: IO Compiler
initCompiler = do
  pwd <- getCurrentDirectory
  let conf =
        CompilerConfig
        { topdir = pwd </> ".boot"
        , ext = "txt"
        , rawGet = readFile
        , rawPut = writeFile
        }
  core_raw_store <-
    newCompilerStore conf {topdir = topdir conf </> "raw" </> "core"}
  core_pretty_store <-
    newCompilerStore conf {topdir = topdir conf </> "pretty" </> "core"}
  stg_raw_store <-
    newCompilerStore conf {topdir = topdir conf </> "raw" </> "stg"}
  stg_pretty_store <-
    newCompilerStore conf {topdir = topdir conf </> "pretty" </> "stg"}
  cmmRaw_raw_store <-
    newCompilerStore conf {topdir = topdir conf </> "raw" </> "cmmRaw"}
  cmmRaw_pretty_store <-
    newCompilerStore conf {topdir = topdir conf </> "pretty" </> "cmmRaw"}
  pure $
    Compiler $ \ModSummary {..} IR {..} -> do
      let dump_raw store raw = modulePut store ms_mod (ppShow raw)
          dump_pretty store pretty =
            modulePut store ms_mod (showSDocUnsafe $ ppr pretty)
      dump_raw core_raw_store core
      dump_pretty core_pretty_store (cg_binds core)
      dump_raw stg_raw_store stg
      dump_pretty stg_pretty_store stg
      cmmRaw_list <- Stream.collect cmmRaw
      dump_raw cmmRaw_raw_store cmmRaw_list
      dump_pretty cmmRaw_pretty_store cmmRaw_list
