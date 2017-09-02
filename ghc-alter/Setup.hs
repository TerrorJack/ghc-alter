{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where

import Language.Haskell.GHC.Alter.BuildInfo.CabalHook

main :: IO ()
main = defaultMainWithBuildInfo
