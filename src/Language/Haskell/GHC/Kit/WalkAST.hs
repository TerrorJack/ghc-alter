{-# LANGUAGE GADTs #-}

module Language.Haskell.GHC.Kit.WalkAST where

import Data.Data hiding (TypeRep, typeOf, typeRep)
import Type.Reflection
import Var

vars :: Data t => t -> [Var]
vars t = concat (gmapQ f t)
  where
    f st =
      case eqTypeRep (typeOf st) (typeRep :: TypeRep Var) of
        Just HRefl -> [st]
        _ -> vars st
