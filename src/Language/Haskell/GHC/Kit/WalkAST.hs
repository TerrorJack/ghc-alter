{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Kit.WalkAST where

import CmmType
import CostCentre
import Data.Data
import ForeignCall
import PrimOp
import StgSyn
import TyCon
import Unique

deriving instance Data occ => Data (GenStgArg occ)

deriving instance Data Width

deriving instance Data PrimOpVecCat

deriving instance Data PrimOp

deriving instance Data PrimCall

instance Data Unique

deriving instance Data CCallSpec

deriving instance Data ForeignCall

deriving instance Data StgOp

deriving instance Data PrimElemRep

deriving instance Data PrimRep

deriving instance (Data AltType)

deriving instance
         (Data bndr, Data occ) => Data (GenStgExpr bndr occ)

deriving instance (Data UpdateFlag)

instance Data StgBinderInfo

instance Data CostCentreStack

deriving instance
         (Data bndr, Data occ) => Data (GenStgRhs bndr occ)

deriving instance
         (Data bndr, Data occ) => Data (GenStgBinding bndr occ)

deriving instance
         (Data bndr, Data occ) => Data (GenStgTopBinding bndr occ)
