{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Kit.LoneWolf
  (
  ) where

import BasicTypes
import CLabel
import Cmm
import CoAxiom
import Compiler.Hoopl.Internals
import CoreSyn
import CostCentre
import DataCon
import ForeignCall
import HscTypes
import Literal
import Module
import Name
import Outputable
import PrimOp
import SMRep
import StgSyn
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

deriving instance Show SptEntry

instance Show ModBreaks where
  show _ = "ModBreaks"

deriving instance Show HpcInfo

deriving instance Show InstalledUnitId

instance Outputable SDoc where
  ppr = id

instance Show SDoc where
  show = stubShow "SDoc"

deriving instance Show ForeignStubs

deriving instance Show CgGuts

deriving instance Show b => Show (Expr b)

deriving instance Show b => Show (Bind b)

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
  show = stubShowNamedThing "DataCon"

deriving instance Show IsCafCC

instance Show ModuleName where
  show n = "(ModuleName " ++ show (moduleNameString n) ++ ")"

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
  show = stubShowNamedThing "Name"

instance Show (Branches br) where
  show _ = "Branches"

deriving instance Show (CoAxiom br)

deriving instance Show Coercion

deriving instance Show TyLit

deriving instance Show ArgFlag

instance Show TyCon where
  show = stubShowNamedThing "TyCon"

instance Show Var where
  show = stubShowNamedThing "Var"

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

deriving instance Show CmmReturnInfo

deriving instance Show ForeignHint

deriving instance Show ForeignConvention

deriving instance Show ForeignTarget

deriving instance Show Area

deriving instance Show LocalReg

deriving instance Show CmmReg

instance Show CmmType where
  show = stubShow "CmmType"

deriving instance Show CmmExpr

deriving instance Show CmmTickScope

deriving instance Show (CmmNode e x)

deriving instance Show t => Show (MaybeO ex t)

deriving instance
         (Show (n C O), Show (n O C), Show (n O O)) => Show (Block n e x)

deriving instance
         (Show (block n O C), Show (block n C C), Show (block n C O),
          Show (block n O O)) =>
         Show (Graph' block n e x)

deriving instance
         (Show (Block n C C), Show (Block n C O), Show (Block n O C),
          Show (Block n O O)) =>
         Show (GenCmmGraph n)

deriving instance Show CmmStackInfo

instance Show StgHalfWord where
  show = stubShow "StgHalfWord"

deriving instance Show C_SRT

deriving instance Show ProfilingInfo

deriving instance Show ArgDescr

deriving instance Show ClosureTypeInfo

deriving instance Show SMRep

deriving instance Show CmmInfoTable

deriving instance Show CmmTopInfo

deriving instance Show CmmLit

deriving instance Show CmmStatic

deriving instance Show CmmStatics

deriving instance Show Section

instance Show CLabel where
  show = stubShow "CLabel"

deriving instance
         (Show d, Show h, Show g) => Show (GenCmmDecl d h g)
