{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Alter.Orphans.Show
  (
  ) where

import Annotations
import Avail
import BasicTypes
import ByteCodeTypes
import CLabel
import Class
import Cmm
import CoAxiom
import CoreSyn
import CostCentre
import DataCon
import FamInstEnv
import ForeignCall
import GHC.Serialized
import GHCi.RemoteTypes
import Hoopl.Block
import Hoopl.Graph
import HscTypes
import InstEnv
import Literal
import Module
import Name
import Outputable
import PatSyn
import PrimOp
import SMRep
import SrcLoc
import StgSyn
import Text.Show.Functions ()
import TyCoRep
import TyCon
import UniqDFM
import UniqFM
import UniqSet
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

instance Outputable a => Show (OccEnv a) where
  show = stubShow "OccEnv"

instance Show a => Show (UniqFM a) where
  show = show . nonDetUFMToList

instance Outputable a => Show (UniqDFM a) where
  show = stubShow "UniqDFM"

deriving instance Show a => Show (FieldLbl a)

deriving instance Show AvailInfo

deriving instance Show Dependencies

deriving instance Show Usage

deriving instance Show FixityDirection

deriving instance Show Fixity

deriving instance Show FixItem

instance Show Class where
  show = stubShow "Class"

deriving instance Show OverlapMode

deriving instance Show OverlapFlag

deriving instance Show IsOrphan

deriving instance Show ClsInst

deriving instance Show FamFlavor

deriving instance Show FamInst

instance Show PatSyn where
  show = stubShow "PatSyn"

deriving instance Show Activation

deriving instance Show CoreRule

deriving instance (Show l, Show e) => Show (GenLocated l e)

deriving instance Show StringLiteral

deriving instance Show WarningTxt

deriving instance Show Warnings

deriving instance Show name => Show (AnnTarget name)

deriving instance Show Serialized

deriving instance Show Annotation

deriving instance Show CompleteMatch

deriving instance Show CoreVect

instance Show a => Show (UniqSet a) where
  show = show . nonDetEltsUniqSet

deriving instance Show VectInfo

deriving instance Show ModGuts

instance Show SDoc where
  show d = "(SDoc " ++ show (showSDocUnsafe d) ++ ")"

deriving instance Show ForeignStubs

deriving instance Show InstalledUnitId

deriving instance Show HpcInfo

instance Show (ForeignRef a) where
  show _ = "ForeignRef"

instance Show NameSpace where
  show ns
    | isDataConNameSpace ns = "DataName"
    | isTcClsNameSpace ns = "TcClsName"
    | isTvNameSpace ns = "TvName"
    | otherwise = "VarName"

instance Show OccName where
  show n =
    "(OccName " ++ show (occNameSpace n) ++ " " ++ show (occNameString n) ++ ")"

deriving instance Show CgBreakInfo

deriving instance Show ModBreaks

deriving instance Show SptEntry

deriving instance Show CgGuts

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
      Just cc -> "(SingletonCCS (" ++ show cc ++ "))"
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
  show shw = "(StgHalfWord " ++ show (fromStgHalfWord shw) ++ ")"

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
