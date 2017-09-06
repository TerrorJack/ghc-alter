{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Alter.Hooks.ByteCode
  ( WithByteCode(..)
  , defaultWithByteCode
  , hscCompileCoreExprWith
  ) where

import ByteCodeAsm
import ByteCodeInstr
import ByteCodeTypes
import Control.Monad
import CoreLint
import CorePrep
import CoreSyn
import CoreTidy
import DynFlags
import ErrUtils
import FastString
import GHCi.RemoteTypes
import HscTypes
import Id
import Language.Haskell.GHC.Alter.Unexported.ByteCodeGen
import Linker
import Module
import Name
import Outputable
import SimplCore
import SrcLoc
import UniqSupply
import Unique
import Util
import VarEnv

data WithByteCode = WithByteCode
  { onenter, onleave :: HscEnv -> SrcSpan -> CoreExpr -> IO ()
  , protoBCO :: HscEnv -> Module -> CoreExpr -> ProtoBCO Name -> IO ()
  , unlinkedBCO :: HscEnv -> Module -> CoreExpr -> UnlinkedBCO -> IO ()
  }

defaultWithByteCode :: WithByteCode
defaultWithByteCode =
  WithByteCode {onenter = d3, onleave = d3, protoBCO = d4, unlinkedBCO = d4}
  where
    d3 _ _ _ = pure ()
    d4 _ = d3

hscCompileCoreExprWith ::
     WithByteCode -> HscEnv -> SrcSpan -> CoreExpr -> IO ForeignHValue
hscCompileCoreExprWith wbc@WithByteCode {..} hsc_env srcspan ds_expr = do
  onenter hsc_env srcspan ds_expr
  let dflags = hsc_dflags hsc_env
  simpl_expr <- simplifyExpr dflags ds_expr
  let tidy_expr = tidyExpr emptyTidyEnv simpl_expr
  prepd_expr <- corePrepExpr dflags hsc_env tidy_expr
  lintInteractiveExpr "hscCompileExpr" hsc_env prepd_expr
  bcos <-
    coreExprToBCOsWith
      wbc
      hsc_env
      (icInteractiveModule (hsc_IC hsc_env))
      prepd_expr
  hval <- linkExpr hsc_env srcspan bcos
  onleave hsc_env srcspan ds_expr
  pure hval

coreExprToBCOsWith ::
     WithByteCode -> HscEnv -> Module -> CoreExpr -> IO UnlinkedBCO
coreExprToBCOsWith WithByteCode {..} hsc_env this_mod expr =
  withTiming
    (pure dflags)
    (text "ByteCodeGen" <+> brackets (ppr this_mod))
    (const ()) $ do
    let invented_name =
          mkSystemVarName (mkPseudoUniqueE 0) (fsLit "ExprTopLevel")
        invented_id = Id.mkLocalId invented_name (panic "invented_id's type")
    us <- mkSplitUniqSupply 'y'
    (BcM_State _dflags _us _this_mod _final_ctr mallocd _ _ _, proto_bco) <-
      runBc hsc_env us this_mod Nothing emptyVarEnv $
      schemeTopBind (invented_id, simpleFreeVars expr)
    when
      (notNull mallocd)
      (panic "ByteCodeGen.coreExprToBCOs: missing final emitBc?")
    protoBCO hsc_env this_mod expr proto_bco
    dumpIfSet_dyn dflags Opt_D_dump_BCOs "Proto-BCOs" (ppr proto_bco)
    bcos <- assembleOneBCO hsc_env proto_bco
    unlinkedBCO hsc_env this_mod expr bcos
    pure bcos
  where
    dflags = hsc_dflags hsc_env
