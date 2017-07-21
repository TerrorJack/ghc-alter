module Main
  ( main
  ) where

import Language.Haskell.GHC.Kit.Boot

main :: IO ()
main = do
  t <- defaultBootTask
  boot $ t {ghc = "ghc-wrapper", confOpts = ["--verbose"]}
