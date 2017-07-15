{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Distribution.Simple.Compiler
import Language.Haskell.GHC.Kit.BuildInfo
import Language.Haskell.GHC.Kit.GHCWrapper

pkgDb :: String
pkgDb =
  case last pkgDbStack of
    SpecificPackageDB p -> p

subst :: [String] -> [String]
subst args = do
  arg <- args
  case arg of
    "--make" ->
      [ "--frontend"
      , "Language.Haskell.GHC.Kit.FrontendPlugin"
      , "-plugin-package"
      , "ghc-kit"
      , "-package-db"
      , pkgDb
      ]
    _ -> [arg]

main :: IO ()
main = wrapperMain (WrapperOptions subst)
