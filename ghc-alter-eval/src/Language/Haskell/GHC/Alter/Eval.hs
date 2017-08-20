{-# LANGUAGE MagicHash #-}

module Language.Haskell.GHC.Alter.Eval where

import Control.Monad.IO.Class
import Data.Functor
import DynFlags
import GHC
import GHC.Exts
import GHC.IO (evaluate)
import Language.Haskell.GHC.Alter.BuildInfo

unsafeEval :: GhcMonad m => String -> m a
unsafeEval expr = do
  hval <- compileExpr expr
  liftIO $ evaluate $ unsafeCoerce# hval

unsafeEvalIO :: [ModuleName] -> String -> IO a
unsafeEvalIO mod_names expr =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
  runGhc (Just ghcLibdir) $ do
    dflags <- getSessionDynFlags
    void $ setSessionDynFlags dflags
    setContext [IIDecl $ simpleImportDecl mod_name | mod_name <- mod_names]
    unsafeEval expr
