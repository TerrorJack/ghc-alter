module Main
  ( main
  ) where

import System.Directory
import System.Process

main :: IO ()
main = do
  setCurrentDirectory "./test/fact"
  callProcess "rm" ["-f", "fact", "fact.hi", "fact.o"]
  callProcess
    "ghc-wrapper"
    ["--make", "fact.hs", "-ffrontend-opt", "trick", "-ffrontend-opt", "treat"]
  r <- readProcess "./fact" [] ""
  case r of
    "120\n" -> pure ()
    _ -> fail $ "Result of fact 5 expected to be 120, got " ++ r
