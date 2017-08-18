{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Alter.Lovecraft where

import CoreSyn
import Data.Data (Data(..))
import qualified Data.Set as Set
import HscTypes
import Language.Haskell.GHC.Alter.Compiler
import Type.Reflection
       ((:~~:)(..), TypeRep, eqTypeRep, typeOf, typeRep)
import Var

vars :: Data t => t -> Set.Set Var
vars = Set.unions . gmapQ f
  where
    f :: Data a => a -> Set.Set Var
    f x =
      case eqTypeRep (typeOf x) (typeRep :: TypeRep Var) of
        Just HRefl -> Set.singleton x
        _ -> vars x

coreBindLHSVars :: CoreBind -> Set.Set Var
coreBindLHSVars (NonRec b _) = Set.singleton b
coreBindLHSVars (Rec bs) = Set.fromList [b | (b, _) <- bs]

coreProgLHSVars :: CoreProgram -> Set.Set Var
coreProgLHSVars = Set.unions . map coreBindLHSVars

coreBindRHSVars :: CoreBind -> Set.Set Var
coreBindRHSVars (NonRec _ expr) = vars expr
coreBindRHSVars (Rec bs) = Set.unions [vars expr | (_, expr) <- bs]

coreProgRHSVars :: CoreProgram -> Set.Set Var
coreProgRHSVars = Set.unions . map coreBindRHSVars

compiler :: Compiler
compiler =
  defaultCompiler
  { runCompiler =
      \_ IR {core = CgGuts {..}} -> do
        let lhs_vars = coreProgLHSVars cg_binds
            rhs_vars = coreProgRHSVars cg_binds
        putStrLn $ "Num of Core LHS Vars: " ++ show (Set.size lhs_vars)
        putStrLn $ "Num of Core RHS Vars: " ++ show (Set.size rhs_vars)
  }
