module Main where

import Language.Haskell.GHC.Kit.Boot
import Language.Haskell.GHC.Kit.Utils.Shell
import System.Environment
import System.FilePath

main :: IO ()
main = do
  unsetEnv "GHC_PACKAGE_PATH"
  t <- defaultBootTask
  runShell $ boot $ t {top = top t </> "boot-lib", ghc = "ghc-wrapper"}
