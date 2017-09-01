{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.GHC.Alter.Eval.Internals
  ( ghcLibDir
  ) where

import Language.Haskell.GHC.Alter.BuildInfo.TypedSplices

ghcLibDir :: FilePath
ghcLibDir = $$(ghcLibDirQ)
