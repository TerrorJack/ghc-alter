module Main where

import Language.Haskell.GHC.Kit.GHCWrapper

main :: IO ()
main =
  wrapperMain
    WrapperOptions {pluginModule = "Language.Haskell.GHC.Kit.FrontendPlugin"}
