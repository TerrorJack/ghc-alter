{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where

import Data.Binary
import Distribution.Simple

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
    { postConf =
        \args flags pkg_descr lbi -> do
          encodeFile ".buildinfo" (args, flags, pkg_descr, lbi)
          postConf simpleUserHooks args flags pkg_descr lbi
    }
