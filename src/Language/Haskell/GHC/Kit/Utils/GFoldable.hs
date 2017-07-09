{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Haskell.GHC.Kit.Utils.GFoldable where

class GFoldable rep a where
  gfoldMap :: Monoid m => (a -> m) -> rep p -> m
  default gfoldMap :: (Monoid m, GFoldable' rep a) =>
    (a -> m) -> rep p -> m
  gfoldMap = gfoldMap'

class GFoldable' rep a where
  gfoldMap' :: Monoid m => (a -> m) -> rep p -> m
