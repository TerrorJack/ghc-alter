module Main
  ( main
  ) where

import Language.Haskell.GHC.Alter.Boot

main :: IO ()
main = do
  t <- defaultBootTask
  boot $ t {ghc = "ghc-wrapper"}
