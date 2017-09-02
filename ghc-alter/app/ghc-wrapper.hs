{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Language.Haskell.GHC.Alter.BuildInfo.Splices.Untyped
import Language.Haskell.GHC.Alter.GHCWrapper

main :: IO ()
main =
  wrapperMain
    WrapperOptions
    { pluginModule = "Language.Haskell.GHC.Alter.FrontendPlugin"
    , pluginPackage = unPackageName $ pkgName $ package $(packageDescriptionQ)
    }
