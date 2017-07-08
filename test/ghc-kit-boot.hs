module Main where

import Language.Haskell.GHC.Kit.Boot
import qualified Language.Haskell.GHC.Kit.BuildInfo as P
import Language.Haskell.GHC.Kit.Utils.Shell
import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
  unsetEnv "GHC_PACKAGE_PATH"
  t <- getCurrentDirectory
  runShell $ boot $ BootTask (t </> "boot-lib") P.ghc [] []
