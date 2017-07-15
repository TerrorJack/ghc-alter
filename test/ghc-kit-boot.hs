module Main where

import Language.Haskell.GHC.Kit.Boot
import System.FilePath

main :: IO ()
main = do
  t <- defaultBootTask
  boot $ t {top = top t </> "boot-lib", ghc = "ghc-wrapper"}
