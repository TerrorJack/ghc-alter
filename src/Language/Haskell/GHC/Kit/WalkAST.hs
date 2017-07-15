{-# LANGUAGE GADTs #-}

module Language.Haskell.GHC.Kit.WalkAST where

import Data.Data hiding (TypeRep, typeOf, typeRep)
import qualified Data.Set as Set
import Type.Reflection
import Var

rawvars :: Data t => t -> [Var]
rawvars t = concat (gmapQ f t)
  where
    f st =
      case eqTypeRep (typeOf st) (typeRep :: TypeRep Var) of
        Just HRefl -> [st]
        _ -> rawvars st

vars :: Data t => t -> Set.Set Var
vars = Set.filter isGlobalId . Set.fromList . rawvars
