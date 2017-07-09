module Main where

import Language.Haskell.GHC.Kit.GHCWrapper

main :: IO ()
main = wrapperMain defaultWrapperOptions
