{-# LANGUAGE MagicHash #-}

module Language.Haskell.GHC.Alter.Eval
  ( unsafeImport
  ) where

import Control.Monad.IO.Class
import Data.Functor
import DynFlags
import GHC
import GHC.Exts
import GHC.IO (evaluate)
import GHCi
import Language.Haskell.GHC.Alter.Eval.Internals
import Linker

unsafeImport :: [PackageDBFlag] -> [PackageFlag] -> ModuleName -> String -> IO a
unsafeImport pkgdbs pkgs mod_name var_name =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
  runGhc (Just ghcLibDir) $ do
    dflags' <- getSessionDynFlags
    void $
      setSessionDynFlags dflags' {packageDBFlags = pkgdbs, packageFlags = pkgs}
    setContext [IIDecl $ simpleImportDecl mod_name]
    [name] <- parseName var_name
    dflags <- getSessionDynFlags
    hsc_env <- getSession
    liftIO $ do
      r <- getHValue hsc_env name
      v <- wormhole dflags r
      evaluate $ unsafeCoerce# v
