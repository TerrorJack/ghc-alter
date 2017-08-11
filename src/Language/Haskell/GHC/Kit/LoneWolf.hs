{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Kit.LoneWolf where

import BasicTypes
import CoAxiom
import CoreSyn
import CostCentre
import DataCon
import ForeignCall
import Literal
import Module
import Name
import PrimOp
import StgSyn
import TyCoRep
import TyCon
import Var

deriving instance Show UpdateFlag

instance Show StgBinderInfo where
  show = undefined

instance Show CostCentreStack where
  show = undefined

deriving instance Show AltCon

deriving instance Show AltType

deriving instance Show CCallConv

deriving instance Show CCallTarget

deriving instance Show CCallSpec

deriving instance Show ForeignCall

deriving instance Show PrimCall

deriving instance Show PrimOpVecCat

deriving instance Show PrimOp

deriving instance Show StgOp

instance Show DataCon where
  show = undefined

deriving instance Show IsCafCC

instance Show ModuleName where
  show = undefined

deriving instance Show Module

deriving instance Show CostCentre

deriving instance Show bndr => Show (Tickish bndr)

deriving instance Show LeftOrRight

instance Show CoAxiomRule where
  show = undefined

instance Show CoercionHole where
  show = undefined

deriving instance Show UnivCoProvenance

deriving instance Show Role

instance Show Name where
  show = undefined

instance Show (Branches br) where
  show = undefined

deriving instance Show (CoAxiom br)

deriving instance Show Coercion

deriving instance Show TyLit

deriving instance Show ArgFlag

instance Show TyCon where
  show = undefined

instance Show Var where
  show = undefined

deriving instance
         (Show tyvar, Show argf) => Show (TyVarBndr tyvar argf)

deriving instance Show Type

deriving instance Show FunctionOrData

deriving instance Show Literal

deriving instance Show occ => Show (GenStgArg occ)

deriving instance
         (Show bndr, Show occ) => Show (GenStgExpr bndr occ)

deriving instance
         (Show bndr, Show occ) => Show (GenStgRhs bndr occ)

deriving instance
         (Show bndr, Show occ) => Show (GenStgBinding bndr occ)

deriving instance
         (Show bndr, Show occ) => Show (GenStgTopBinding bndr occ)
