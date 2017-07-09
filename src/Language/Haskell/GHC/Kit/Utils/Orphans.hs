{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.GHC.Kit.Utils.Orphans where

import CoreSyn
import GHC.Generics
import StgSyn

deriving instance Generic (Bind b)

deriving instance Generic (Expr b)

deriving instance Generic (Tickish id)

deriving instance Generic (GenStgExpr bndr occ)

deriving instance Generic (GenStgRhs bndr occ)

deriving instance Generic (GenStgBinding bndr occ)

deriving instance Generic (GenStgTopBinding bndr occ)
