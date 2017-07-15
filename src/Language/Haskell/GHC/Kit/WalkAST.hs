{-# LANGUAGE GADTs #-}

module Language.Haskell.GHC.Kit.WalkAST where

import Data.Data hiding (TypeRep, typeOf, typeRep)
import qualified Data.Set as Set
import Name
import Type.Reflection
import Var

extnames :: Data t => t -> Set.Set Name
extnames t = Set.unions (gmapQ f t)
  where
    f st =
      case eqTypeRep (typeOf st) (typeRep :: TypeRep Var) of
        Just HRefl ->
          let n = getName st
          in case nameModule_maybe n of
               Just _ -> Set.singleton n
               _ -> Set.empty
        _ -> extnames st
