module Main
  ( main
  ) where

import Language.Haskell.GHC.Alter.BuildInfo
import Language.Haskell.GHC.Alter.GHCWrapper

main :: IO ()
main =
  wrapperMain
    WrapperOptions
    { pluginModule = "Language.Haskell.GHC.Alter.FrontendPlugin"
    , pluginPackage = pkgName
    }
