{-# LANGUAGE GADTs #-}

module Language.Haskell.GHC.Kit.WalkAST
  ( extmods
  ) where

import Data.Data hiding (TypeRep, typeOf, typeRep)
import qualified Data.Set as Set
import Module
import Name
import Type.Reflection hiding (Module)
import Var

extmods :: Data t => t -> Set.Set Module
extmods t = Set.unions (gmapQ f t)
  where
    f st =
      case eqTypeRep (typeOf st) (typeRep :: TypeRep Var) of
        Just HRefl ->
          let n = getName st
          in case nameModule_maybe n of
               Just _ ->
                 case nameModule_maybe n of
                   Just m -> Set.singleton m
                   _ -> Set.empty
               _ -> Set.empty
        _ -> extmods st
