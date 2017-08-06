{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.GHC.Kit.Mystery where

import Control.Monad
import Data.Foldable
import qualified Data.Set as Set
import Language.Haskell.GHC.Kit.Compiler
import Name
import Outputable
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
    for_
      [ ("All", const True)
      , ("External", isExternalName)
      , ("System", isSystemName)
      , ("WiredIn", isWiredInName)
      ] $ \(n, f) -> do
      let local_ns = Set.filter f ns
          local_ss = Set.map nameStableString local_ns
          local_os = Set.map occName local_ns
      putStrLn $ "Num of " ++ n ++ " stg bindings: " ++ show (Set.size local_ns)
      when (Set.size local_ns < 64) $ putStrLn $ showSDocUnsafe $ ppr local_ns
      when (Set.size local_ns /= Set.size local_os) $ do
        putStrLn $
          "BiBiBi!! Num of " ++
          n ++ " occName stg bindings: " ++ show (Set.size local_os)
        when (Set.size local_os < 64) $ putStrLn $ showSDocUnsafe $ ppr local_os
        when (n == "System") $ putStrLn $ showSDocUnsafe $ ppr stg
      when (Set.size local_ns /= Set.size local_ss) $
        putStrLn $
        "Boom!! Num of " ++
        n ++ " (un)stable string names: " ++ show (Set.size local_ss)
