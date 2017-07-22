{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Kit.RabbitHole where

import CoreSyn
import Data.Data hiding (TypeRep, typeOf, typeRep)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import FastString
import HscTypes
import Language.Haskell.GHC.Kit.Compiler
import Module
import Name
import Outputable
import Type.Reflection hiding (Module)
import Var (Var)

newtype TopName =
  TopName (Module, FastString)
  deriving (Eq, Ord)

instance Outputable TopName where
  ppr (TopName (mod_key, bind_key)) =
    let (k0, k1) = moduleKey mod_key
    in text $ k0 ++ "." ++ k1 ++ "." ++ unpackFS bind_key

type TopNameSet = Set.Set TopName

type TopNameMap = Map.Map Name TopNameSet

topName :: Name -> Maybe TopName
topName n = do
  mod_key <- nameModule_maybe n
  if nameSpacesRelated varName ns
    then Just $ TopName (mod_key, fs)
    else Nothing
  where
    occn = nameOccName n
    ns = occNameSpace occn
    fs = occNameFS occn

topNames :: Data t => t -> TopNameSet
topNames = Set.unions . gmapQ f
  where
    f t =
      case eqTypeRep (typeOf t) (typeRep :: TypeRep Var) of
        Just HRefl ->
          case topName $ getName t of
            Just tn -> Set.singleton tn
            _ -> Set.empty
        _ -> topNames t

bindTopNames :: CoreBind -> TopNameMap
bindTopNames (NonRec b expr) = Map.singleton (getName b) (topNames expr)
bindTopNames (Rec bs) =
  Map.fromList [(getName b, topNames expr) | (b, expr) <- bs]

progTopNames :: CoreProgram -> TopNameMap
progTopNames = Map.unions . map bindTopNames

rabbitHole :: Compiler ()
rabbitHole =
  Compiler $ \_ ModSummary {..} IR {core = CgGuts {..}} -> do
    let tn_map = progTopNames cg_binds
    putStrLn $ showSDocUnsafe $ ppr tn_map
