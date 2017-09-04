{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Alter.Orphans.Show
  (
  ) where

import BasicTypes
import CoAxiom
import CoreSyn
import CostCentre
import DataCon
import Literal
import Module
import Name
import Outputable
import Text.Show.Functions ()
import TyCoRep
import TyCon
import Var

stubShow :: Outputable a => String -> a -> String
stubShow n x = "(" ++ n ++ " " ++ show (showSDocUnsafe $ ppr x) ++ ")"

namedThingString :: NamedThing a => a -> String
namedThingString a =
  let n = getName a
      r = nameStableString n
  in if isSystemName n
       then r ++ "_" ++ show (nameUnique n)
       else r

stubShowNamedThing :: NamedThing a => String -> a -> String
stubShowNamedThing n x = "(" ++ n ++ " " ++ show (namedThingString x) ++ ")"

instance Show ModuleName where
  show = show . moduleNameString

deriving instance Show Module

deriving instance Show IsCafCC

deriving instance Show CostCentre

deriving instance Show id => Show (Tickish id)

instance Show Var where
  show = stubShowNamedThing "Var"

deriving instance Show FunctionOrData

deriving instance
         (Show tyvar, Show argf) => Show (TyVarBndr tyvar argf)

instance Show TyCon where
  show = stubShowNamedThing "TyCon"

deriving instance Show ArgFlag

deriving instance Show TyLit

deriving instance Show CoAxBranch

instance Show (Branches br) where
  show = show . fromBranches

instance Show Name where
  show = stubShowNamedThing "Name"

deriving instance Show Role

deriving instance Show (CoAxiom br)

instance Show CoercionHole where
  show = stubShow "CoercionHole"

deriving instance Show UnivCoProvenance

deriving instance Show CoAxiomRule

deriving instance Show LeftOrRight

deriving instance Show Coercion

deriving instance Show Type

deriving instance Show Literal

instance Show DataCon where
  show = stubShowNamedThing "DataCon"

deriving instance Show AltCon

deriving instance Show b => Show (Expr b)

deriving instance Show b => Show (Bind b)
