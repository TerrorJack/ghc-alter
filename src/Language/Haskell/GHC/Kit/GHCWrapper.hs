module Language.Haskell.GHC.Kit.GHCWrapper where

import Language.Haskell.GHC.Kit.BuildInfo
import System.Environment
import System.Process

newtype WrapperOptions = WrapperOptions
  { substGHCArgs :: [String] -> [String]
  }

defaultWrapperOptions :: WrapperOptions
defaultWrapperOptions = WrapperOptions id

wrapperMain :: WrapperOptions -> IO ()
wrapperMain opts = do
  args <- getArgs
  callProcess ghc $ substGHCArgs opts args
