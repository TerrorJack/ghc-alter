{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Alter.Orphans.Show
  (
  ) where

import BasicTypes
import CLabel
import Cmm
import CoAxiom
import CoreSyn
import CostCentre
import DataCon
import ForeignCall
import Hoopl.Block
import Hoopl.Graph
import Literal
import Module
import Name
import Outputable
import PrimOp
import SMRep
import StgSyn
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

deriving instance Show occ => Show (GenStgArg occ)

deriving instance Show PrimOpVecCat

deriving instance Show PrimOp

deriving instance Show PrimCall

deriving instance Show CCallTarget

deriving instance Show CCallConv

deriving instance Show CCallSpec

deriving instance Show ForeignCall

deriving instance Show StgOp

deriving instance Show AltType

deriving instance
         (Show bndr, Show occ) => Show (GenStgExpr bndr occ)

instance Show CostCentreStack where
  show ccs =
    case maybeSingletonCCS ccs of
      Just cc -> "SingletonCCS (" ++ show cc ++ ")"
      _
        | noCCSAttached ccs -> "NoCCS"
        | isCurrentCCS ccs -> "CurrentCCS"
        | otherwise -> "DontCareCCS"

instance Show StgBinderInfo where
  show sbi =
    if satCallsOnly sbi
      then "SatCallsOnly"
      else "NoStgBinderInfo"

deriving instance Show UpdateFlag

deriving instance
         (Show bndr, Show occ) => Show (GenStgRhs bndr occ)

deriving instance
         (Show bndr, Show occ) => Show (GenStgBinding bndr occ)

deriving instance
         (Show bndr, Show occ) => Show (GenStgTopBinding bndr occ)

instance Show CLabel where
  show = stubShow "CLabel"

deriving instance Show Section

deriving instance
         (Show d, Show h, Show g) => Show (GenCmmDecl d h g)

deriving instance Show CmmLit

deriving instance Show CmmStatic

deriving instance Show CmmStatics

deriving instance Show ArgDescr

deriving instance Show ClosureTypeInfo

deriving instance Show SMRep

deriving instance Show ProfilingInfo

instance Show StgHalfWord where
  show shw = "StgHalfWord " ++ show (fromStgHalfWord shw)

deriving instance Show C_SRT

deriving instance Show CmmInfoTable

deriving instance Show CmmStackInfo

deriving instance Show CmmTopInfo

deriving instance Show t => Show (MaybeO ex t)

deriving instance
         (Show (block n C C), Show (block n O C), Show (block n C O),
          Show (block n O O)) =>
         Show (Graph' block n e x)

deriving instance
         (Show (n C O), Show (n O C), Show (n O O), Show (n C O)) =>
         Show (Block n e x)

deriving instance
         (Show (n C O), Show (n O C), Show (n O O)) => Show (GenCmmGraph n)

deriving instance Show CmmTickScope

instance Show CmmType where
  show = stubShow "CmmType"

deriving instance Show LocalReg

deriving instance Show CmmReg

deriving instance Show Area

deriving instance Show CmmExpr

deriving instance Show ForeignHint

deriving instance Show CmmReturnInfo

deriving instance Show ForeignConvention

deriving instance Show ForeignTarget

deriving instance Show (CmmNode e x)
