{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.GHC.Kit.Utils.Shell
  ( Shell
  , runShell
  , proc
  , cd
  , withCd
  ) where

import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Functor
import Prelude hiding (fail)
import System.Directory
import System.Exit
import System.FilePath
import System.Process hiding (proc)

newtype Shell a = Shell
  { unShell :: StateT CreateProcess IO a
  } deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

runShell :: Shell a -> IO a
runShell (Shell sh) = evalStateT sh $ shell ""

proc :: FilePath -> [String] -> Shell ()
proc p args =
  Shell $ do
    cp <- get
    liftIO $
      withCreateProcess
        cp
        { cmdspec = RawCommand p args
        , std_in = Inherit
        , std_out = Inherit
        , std_err = Inherit
        } $ \_ _ _ h -> do
        r <- waitForProcess h
        case r of
          ExitSuccess -> pure ()
          ExitFailure c ->
            fail $
            "Call to " ++
            show p ++
            " with args " ++ show args ++ " failed with exit code " ++ show c

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

withCd :: FilePath -> Shell a -> Shell a
withCd p (Shell m) =
  Shell $ do
    cp0 <- get
    let p0 = cwd cp0
    unShell $ cd p
    r <- m
    modify' $ \cp -> cp {cwd = p0}
    pure r
