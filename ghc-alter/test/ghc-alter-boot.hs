module Main
  ( main
  ) where

import Language.Haskell.GHC.Alter.Boot

main :: IO ()
main = boot $ defaultBootTask {ghc = "ghc-wrapper"}
