{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Kit.LoneWolf
  (
  ) where

import BasicTypes
import CoAxiom
import CoreSyn
import CostCentre
import DataCon
import ForeignCall
import Literal
import Module
import Name
import Outputable
import PrimOp
import StgSyn
import TyCoRep
import TyCon
import Var

stubShow :: Outputable a => String -> a -> String
stubShow n x = n ++ " " ++ show (showSDocUnsafe $ ppr x)

deriving instance Show UpdateFlag

instance Show StgBinderInfo where
  show sbi =
    if satCallsOnly sbi
      then "SatCallsOnly"
      else "NoStgBinderInfo"

instance Show CostCentreStack where
  show = stubShow "CostCentreStack"

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
  show = stubShow "DataCon"

deriving instance Show IsCafCC

instance Show ModuleName where
  show n = "ModuleName " ++ show (moduleNameString n)

deriving instance Show Module

deriving instance Show CostCentre

deriving instance Show bndr => Show (Tickish bndr)

deriving instance Show LeftOrRight

instance Show CoAxiomRule where
  show = stubShow "CoAxiomRule"

instance Show CoercionHole where
  show = stubShow "CoercionHole"

deriving instance Show UnivCoProvenance

deriving instance Show Role

instance Show Name where
  show n = "Name " ++ show (nameStableString n)

instance Show (Branches br) where
  show _ = "Branches"

deriving instance Show (CoAxiom br)

deriving instance Show Coercion

deriving instance Show TyLit

deriving instance Show ArgFlag

instance Show TyCon where
  show = stubShow "TyCon"

instance Show Var where
  show = stubShow "Var"

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
