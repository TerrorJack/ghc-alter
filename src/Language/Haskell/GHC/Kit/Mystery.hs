{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Kit.Mystery where

import Control.Monad
import qualified Data.Set as Set
import Language.Haskell.GHC.Kit.Compiler
import Name
import StgSyn

stgProgLHS :: [StgTopBinding] -> Set.Set Name
stgProgLHS = Set.unions . map stgTopBindLHS

stgTopBindLHS :: StgTopBinding -> Set.Set Name
stgTopBindLHS (StgTopLifted b) = stgBindLHS b
stgTopBindLHS (StgTopStringLit v _) = Set.singleton $ getName v

stgBindLHS :: StgBinding -> Set.Set Name
stgBindLHS (StgNonRec v _) = Set.singleton $ getName v
stgBindLHS (StgRec bs) = Set.fromList [getName v | (v, _) <- bs]

compiler :: Compiler
compiler =
  Compiler $ \_ IR {..} -> do
    let ns = stgProgLHS stg
        os = Set.map occName ns
        ss = Set.map occNameString os
    putStrLn $ "Num of stg bindings: " ++ show (Set.size ns)
    when (Set.size ns /= Set.size os) $
      putStrLn $ "Bibibi!! Num of occNames: " ++ show (Set.size os)
    when (Set.size os /= Set.size ss) $
      putStrLn $ "Bibibi!! Num of String occNames: " ++ show (Set.size ss)
    putStrLn $
      "Num of external stg bindings: " ++
      show (Set.size $ Set.filter isExternalName ns)
    putStrLn $
      "Num of system stg bindings: " ++
      show (Set.size $ Set.filter isSystemName ns)
    putStrLn $
      "Num of wired-in stg bindings: " ++
      show (Set.size $ Set.filter isWiredInName ns)
