module Main where

import System.Directory
import System.Process

main :: IO ()
main = do
  setCurrentDirectory "./test"
  callProcess "ghc-wrapper" ["fact.hs"]
  r <- readProcess "./fact" [] ""
  case r of
    "120\n" -> pure ()
    _ -> fail $ "Result of fact 5 expected to be 120, got " ++ r
