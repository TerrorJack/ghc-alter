{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.GHC.Kit.Utils.Shell where

import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Functor
import System.Directory
import System.FilePath
import System.Process

newtype Shell a = Shell
  { unShell :: StateT CreateProcess IO a
  } deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

runShell :: Shell a -> IO a
runShell (Shell sh) = evalStateT sh $ shell ""

cmd :: String -> Shell ()
cmd c = void $ cmdRead c ""

cmdRead :: String -> String -> Shell String
cmdRead c in_ =
  Shell $ do
    cp <- get
    liftIO $ readCreateProcess cp {cmdspec = ShellCommand c} in_

proc :: FilePath -> [String] -> Shell ()
proc p args = void $ procRead p args ""

procRead :: FilePath -> [String] -> String -> Shell String
procRead p args in_ =
  Shell $ do
    cp <- get
    liftIO $ readCreateProcess cp {cmdspec = RawCommand p args} in_

cd :: FilePath -> Shell ()
cd p =
  Shell $ do
    cp <- get
    p' <-
      liftIO $ do
        p0 <-
          case cwd cp of
            Just p0_ -> pure p0_
            Nothing -> getCurrentDirectory
        p1 <- canonicalizePath $ p0 </> p
        pure p1
    put cp {cwd = Just p'}
