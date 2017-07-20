module Main
  ( main
  ) where

import Language.Haskell.GHC.Kit.Boot
import System.FilePath

main :: IO ()
main = do
  t <- defaultBootTask
  boot $ t {ghc = "ghc-wrapper"}
