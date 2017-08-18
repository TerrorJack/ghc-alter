{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Alter.LoneWolf
  (
  ) where

import Bag
import BasicTypes
import BooleanFormula
import CLabel
import Class
import Cmm
import CoAxiom
import Compiler.Hoopl.Internals
import ConLike
import CoreSyn
import CostCentre
import DataCon
import ForeignCall
import HsSyn
import HscTypes
import InstEnv
import Literal
import Module
import Name
import Outputable
import PatSyn
import PrimOp
import RdrName
import SMRep
import SrcLoc
import StgSyn
import TcEvidence
import TyCoRep
import TyCon
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

deriving instance (Show l, Show e) => Show (GenLocated l e)

instance Show OccName where
  show = stubShow "OccName"

deriving instance Show RdrName

deriving instance Show (IEWrappedName RdrName)

deriving instance Show (FieldLbl RdrName)

deriving instance Show IEWildcard

deriving instance Show (IE RdrName)

deriving instance Show StringLiteral

deriving instance Show (ImportDecl RdrName)

deriving instance Show PlaceHolder

deriving instance Show (FieldOcc RdrName)

deriving instance
         Show (HsRecField' (FieldOcc RdrName) (LPat RdrName))

deriving instance Show (HsRecFields RdrName (LPat RdrName))

deriving instance Show (HsConPatDetails RdrName)

deriving instance Show (HsTyVarBndr RdrName)

deriving instance Show (HsAppType RdrName)

deriving instance Show (HsSplicedThing RdrName)

instance Show ThModFinalizers where
  show _ = "ThModFinalizers"

deriving instance Show (HsSplice RdrName)

deriving instance Show (ConDeclField RdrName)

deriving instance Show (HsWildCardInfo RdrName)

deriving instance Show HsTupleSort

deriving instance Show HsIPName

deriving instance Show SrcUnpackedness

deriving instance Show SrcStrictness

deriving instance Show HsSrcBang

deriving instance Show HsTyLit

deriving instance Show (HsType RdrName)

deriving instance Show (LHsSigType RdrName)

deriving instance Show (LHsSigWcType RdrName)

deriving instance Show EvLit

deriving instance Show EvCallStack

deriving instance Show EvTypeable

deriving instance Show EvTerm

instance Show a => Show (Bag a) where
  show = show . bagToList

instance Show EvBindsVar where
  show _ = "EvBindsVar"

deriving instance Show EvBind

deriving instance Show TcEvBinds

deriving instance Show HsWrapper

deriving instance Show (SyntaxExpr RdrName)

deriving instance Show OverLitVal

deriving instance Show (HsOverLit RdrName)

deriving instance Show Boxity

instance Show PatSyn where
  show = stubShow "PatSyn"

deriving instance Show ConLike

deriving instance Show HsLit

deriving instance Show (Pat RdrName)

deriving instance Show (ApplicativeArg RdrName RdrName)

deriving instance Show FixityDirection

deriving instance Show Fixity

deriving instance Show (FixitySig RdrName)

deriving instance Show a => Show (BooleanFormula a)

deriving instance Show Activation

deriving instance Show InlinePragma

deriving instance Show (Sig RdrName)

deriving instance Show (HsTyVarBndr Name)

deriving instance Show (HsAppType Name)

deriving instance Show (FieldOcc Name)

deriving instance Show (HsRecField' (FieldOcc Name) (LPat Name))

deriving instance Show (HsRecFields Name (LPat Name))

deriving instance Show (HsConPatDetails Name)

deriving instance Show (SyntaxExpr Name)

deriving instance Show (HsOverLit Name)

deriving instance Show (Pat Name)

deriving instance Show (ApplicativeArg Name Name)

deriving instance Show (GRHS Name (LHsExpr Name))

deriving instance Show (GRHSs Name (LHsExpr Name))

deriving instance Show (HsStmtContext Name)

deriving instance Show LexicalFixity

deriving instance Show (HsMatchContext Name)

deriving instance Show (Match Name (LHsExpr Name))

deriving instance Show Origin

deriving instance Show (MatchGroup Name (LHsExpr Name))

instance Show a => Show (UniqSet a) where
  show = show . nonDetUFMToList . getUniqSet

deriving instance Show (RecordPatSynField (Located Name))

deriving instance Show (HsPatSynDetails (Located Name))

deriving instance Show (HsPatSynDir Name)

deriving instance Show (PatSynBind Name Name)

deriving instance Show TcSpecPrag

deriving instance Show TcSpecPrags

deriving instance Show (ABExport Name)

deriving instance Show (HsBindLR Name Name)

deriving instance Show RecFlag

deriving instance Show (HsValBindsLR Name Name)

deriving instance Show (IPBind Name)

deriving instance Show (HsIPBinds Name)

deriving instance Show (HsLocalBindsLR Name Name)

deriving instance Show (ParStmtBlock Name Name)

deriving instance Show (FieldOcc Id)

deriving instance Show (HsRecField' (FieldOcc Id) (LPat Id))

deriving instance Show (HsRecFields Id (LPat Id))

deriving instance Show (HsConPatDetails Id)

deriving instance Show (HsTyVarBndr Id)

deriving instance Show (HsAppType Id)

deriving instance Show (HsSplicedThing Id)

deriving instance Show (HsSplice Id)

deriving instance Show (ConDeclField Id)

deriving instance Show (HsWildCardInfo Id)

deriving instance Show (HsType Id)

deriving instance Show (LHsSigType Id)

deriving instance Show (LHsSigWcType Id)

deriving instance Show (SyntaxExpr Id)

deriving instance Show (HsOverLit Id)

deriving instance Show (Pat Id)

deriving instance Show (ApplicativeArg Id Id)

deriving instance Show (GRHS Id (LHsExpr Id))

deriving instance Show (GRHSs Id (LHsExpr Id))

deriving instance Show (Match Id (LHsExpr Id))

deriving instance Show (MatchGroup Id (LHsExpr Id))

deriving instance Show (RecordPatSynField (Located Id))

deriving instance Show (HsPatSynDetails (Located Id))

deriving instance Show (HsPatSynDir Id)

deriving instance Show (PatSynBind Id Id)

deriving instance Show (ABExport Id)

deriving instance Show (HsBindLR Id Id)

deriving instance Show (FixitySig Id)

deriving instance Show (Sig Id)

deriving instance Show (HsValBindsLR Id Id)

deriving instance Show (IPBind Id)

deriving instance Show (HsIPBinds Id)

deriving instance Show (HsLocalBindsLR Id Id)

deriving instance Show (ParStmtBlock Id Id)

deriving instance Show TransForm

deriving instance Show (StmtLR Id Id (LHsExpr Id))

deriving instance Show (LHsWcType Id)

deriving instance Show (LHsWcType Name)

deriving instance Show (HsRecField' (FieldOcc Id) (LHsExpr Id))

deriving instance Show (HsRecordBinds Id)

deriving instance Show (AmbiguousFieldOcc Id)

deriving instance
         Show (HsRecField' (AmbiguousFieldOcc Id) (LHsExpr Id))

deriving instance Show (HsTupArg Id)

deriving instance Show (ArithSeqInfo Id)

deriving instance Show (LHsQTyVars Id)

deriving instance Show (TyFamEqn Id (LHsQTyVars Id))

deriving instance Show (HsTyPats Id)

deriving instance Show (TyFamEqn Id (HsTyPats Id))

deriving instance Show (FamilyInfo Id)

deriving instance Show (FamilyResultSig Id)

deriving instance Show (InjectivityAnn Id)

deriving instance Show (FamilyDecl Id)

deriving instance Show (HsConDeclDetails Id)

deriving instance Show (ConDecl Id)

deriving instance Show DerivStrategy

deriving instance Show (HsDerivingClause Id)

deriving instance Show NewOrData

deriving instance Show Header

deriving instance Show CType

deriving instance Show (HsDataDefn Id)

deriving instance Show DocDecl

deriving instance Show (TyClDecl Id)

deriving instance Show (TyFamInstDecl Id)

deriving instance Show (DataFamInstDecl Id)

deriving instance Show OverlapMode

deriving instance Show (ClsInstDecl Id)

deriving instance Show (InstDecl Id)

deriving instance Show (DerivDecl Id)

deriving instance Show (DefaultDecl Id)

deriving instance Show CImportSpec

deriving instance Show ForeignImport

deriving instance Show CExportSpec

deriving instance Show ForeignExport

deriving instance Show (ForeignDecl Id)

deriving instance Show WarningTxt

deriving instance Show (WarnDecl Id)

deriving instance Show (WarnDecls Id)

deriving instance Show (AnnProvenance Id)

deriving instance Show (AnnDecl Id)

deriving instance Show (RuleBndr Id)

deriving instance Show (RuleDecl Id)

deriving instance Show (RuleDecls Id)

instance Show Class where
  show = stubShow "Class"

deriving instance Show OverlapFlag

deriving instance Show IsOrphan

deriving instance Show ClsInst

deriving instance Show (VectDecl Id)

deriving instance Show SpliceExplicitFlag

deriving instance Show (SpliceDecl Id)

deriving instance Show (RoleAnnotDecl Id)

deriving instance Show (HsDecl Id)

deriving instance Show (TyClGroup Id)

deriving instance Show (HsGroup Id)

deriving instance Show (HsBracket Id)

deriving instance Show (LHsQTyVars Name)

deriving instance Show (TyFamEqn Name (LHsQTyVars Name))

deriving instance Show (HsTyPats Name)

deriving instance Show (TyFamEqn Name (HsTyPats Name))

deriving instance Show (FamilyInfo Name)

deriving instance Show (FamilyResultSig Name)

deriving instance Show (InjectivityAnn Name)

deriving instance Show (FamilyDecl Name)

deriving instance Show (ConDeclField Name)

deriving instance Show (HsConDeclDetails Name)

deriving instance Show (ConDecl Name)

deriving instance Show (HsDerivingClause Name)

deriving instance Show (HsDataDefn Name)

deriving instance Show (TyClDecl Name)

deriving instance Show (TyFamInstDecl Name)

deriving instance Show (DataFamInstDecl Name)

deriving instance Show (ClsInstDecl Name)

deriving instance Show (InstDecl Name)

deriving instance Show (DerivDecl Name)

deriving instance Show (DefaultDecl Name)

deriving instance Show (ForeignDecl Name)

deriving instance Show (WarnDecl Name)

deriving instance Show (WarnDecls Name)

deriving instance Show (AnnProvenance Name)

deriving instance Show (AnnDecl Name)

deriving instance Show (RuleBndr Name)

deriving instance Show (RuleDecl Name)

deriving instance Show (RuleDecls Name)

deriving instance Show (VectDecl Name)

deriving instance Show (SpliceDecl Name)

deriving instance Show (RoleAnnotDecl Name)

deriving instance Show (HsDecl Name)

deriving instance Show (TyClGroup Name)

deriving instance Show (FixitySig Name)

deriving instance Show (HsGroup Name)

deriving instance Show (HsBracket Name)

deriving instance Show (GRHS Id (LHsCmd Id))

deriving instance Show (GRHSs Id (LHsCmd Id))

deriving instance Show (Match Id (LHsCmd Id))

deriving instance Show (MatchGroup Id (LHsCmd Id))

deriving instance Show (StmtLR Id Id (LHsCmd Id))

deriving instance Show HsArrAppType

deriving instance Show (HsCmd Id)

deriving instance Show (HsCmdTop Id)

instance Show a => Show (OccEnv a) where
  show = show . occEnvElts

deriving instance Show Parent

deriving instance Show ImpDeclSpec

deriving instance Show ImpItemSpec

deriving instance Show ImportSpec

deriving instance Show GlobalRdrElt

deriving instance Show UnboundVar

deriving instance Show UntypedSpliceFlavour

deriving instance Show PendingRnSplice

deriving instance Show PendingTcSplice

deriving instance Show (HsExpr Id)

deriving instance Show (StmtLR Name Name (LHsExpr Name))

deriving instance Show (HsRecField' (FieldOcc Name) (LHsExpr Name))

deriving instance Show (HsRecordBinds Name)

deriving instance Show (AmbiguousFieldOcc Name)

deriving instance
         Show (HsRecField' (AmbiguousFieldOcc Name) (LHsExpr Name))

deriving instance Show (HsTupArg Name)

deriving instance Show (ArithSeqInfo Name)

deriving instance Show (StmtLR Name Name (LHsCmd Name))

deriving instance Show (GRHS Name (LHsCmd Name))

deriving instance Show (GRHSs Name (LHsCmd Name))

deriving instance Show (Match Name (LHsCmd Name))

deriving instance Show (MatchGroup Name (LHsCmd Name))

deriving instance Show (HsCmd Name)

deriving instance Show (HsCmdTop Name)

deriving instance Show (HsExpr Name)

deriving instance Show (HsSplicedThing Name)

deriving instance Show (HsSplice Name)

deriving instance Show (HsWildCardInfo Name)

deriving instance Show (HsType Name)

deriving instance Show (LHsSigType Name)

deriving instance Show (LHsSigWcType Name)

deriving instance Show (Sig Name)

deriving instance Show (HsValBindsLR RdrName RdrName)

deriving instance Show (IPBind RdrName)

deriving instance Show (HsIPBinds RdrName)

deriving instance Show (HsLocalBindsLR RdrName RdrName)

deriving instance Show (ParStmtBlock RdrName RdrName)

deriving instance Show (StmtLR RdrName RdrName (LHsExpr RdrName))

deriving instance Show (GRHS RdrName (LHsExpr RdrName))

deriving instance Show (GRHSs RdrName (LHsExpr RdrName))

deriving instance Show (HsStmtContext RdrName)

deriving instance Show (HsMatchContext RdrName)

deriving instance Show (Match RdrName (LHsExpr RdrName))

deriving instance Show (MatchGroup RdrName (LHsExpr RdrName))

deriving instance Show (LHsWcType RdrName)

deriving instance
         Show (HsRecField' (FieldOcc RdrName) (LHsExpr RdrName))

deriving instance Show (HsRecordBinds RdrName)

deriving instance Show (AmbiguousFieldOcc RdrName)

deriving instance
         Show (HsRecField' (AmbiguousFieldOcc RdrName) (LHsExpr RdrName))

deriving instance Show (HsTupArg RdrName)

deriving instance Show (ArithSeqInfo RdrName)

deriving instance Show (SpliceDecl RdrName)

deriving instance Show (LHsQTyVars RdrName)

deriving instance Show (TyFamEqn RdrName (LHsQTyVars RdrName))

deriving instance Show (HsTyPats RdrName)

deriving instance Show (TyFamEqn RdrName (HsTyPats RdrName))

deriving instance Show (FamilyInfo RdrName)

deriving instance Show (FamilyResultSig RdrName)

deriving instance Show (InjectivityAnn RdrName)

deriving instance Show (FamilyDecl RdrName)

deriving instance Show (HsConDeclDetails RdrName)

deriving instance Show (ConDecl RdrName)

deriving instance Show (HsDerivingClause RdrName)

deriving instance Show (HsDataDefn RdrName)

deriving instance Show (TyClDecl RdrName)

deriving instance Show (RoleAnnotDecl RdrName)

deriving instance Show (TyFamInstDecl RdrName)

deriving instance Show (DataFamInstDecl RdrName)

deriving instance Show (ClsInstDecl RdrName)

deriving instance Show (InstDecl RdrName)

deriving instance Show (TyClGroup RdrName)

deriving instance Show (DerivDecl RdrName)

deriving instance Show (DefaultDecl RdrName)

deriving instance Show (ForeignDecl RdrName)

deriving instance Show (WarnDecl RdrName)

deriving instance Show (WarnDecls RdrName)

deriving instance Show (AnnProvenance RdrName)

deriving instance Show (AnnDecl RdrName)

deriving instance Show (RuleBndr RdrName)

deriving instance Show (RuleDecl RdrName)

deriving instance Show (RuleDecls RdrName)

deriving instance Show (VectDecl RdrName)

deriving instance Show (HsGroup RdrName)

deriving instance Show (HsBracket RdrName)

deriving instance Show (StmtLR RdrName RdrName (LHsCmd RdrName))

deriving instance Show (GRHS RdrName (LHsCmd RdrName))

deriving instance Show (GRHSs RdrName (LHsCmd RdrName))

deriving instance Show (Match RdrName (LHsCmd RdrName))

deriving instance Show (MatchGroup RdrName (LHsCmd RdrName))

deriving instance Show (HsCmd RdrName)

deriving instance Show (HsCmdTop RdrName)

deriving instance Show (HsExpr RdrName)

deriving instance Show (RecordPatSynField (Located RdrName))

deriving instance Show (HsPatSynDetails (Located RdrName))

deriving instance Show (HsPatSynDir RdrName)

deriving instance Show (PatSynBind RdrName RdrName)

deriving instance Show (ABExport RdrName)

deriving instance Show (HsBind RdrName)

deriving instance Show (HsDecl RdrName)

deriving instance Show (HsModule RdrName)

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
