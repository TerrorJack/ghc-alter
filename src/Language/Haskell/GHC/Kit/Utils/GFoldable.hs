{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.GHC.Kit.Utils.GFoldable where

import GHC.Generics

class GFoldable t a where
  gfoldMap :: Monoid m => (a -> m) -> t -> m
  default gfoldMap :: (Monoid m, Generic t, GFoldable' (Rep t) a) =>
    (a -> m) -> t -> m
  gfoldMap f x = gfoldMap' f (from x)

instance GFoldable a a where
  gfoldMap = ($)

class GFoldable' rep a where
  gfoldMap' :: Monoid m => (a -> m) -> rep p -> m

instance GFoldable' V1 a where
  gfoldMap' _ _ = undefined

instance GFoldable' U1 a where
  gfoldMap' _ _ = mempty

instance (GFoldable c a) => GFoldable' (K1 i c) a where
  gfoldMap' f (K1 x) = gfoldMap f x

instance (GFoldable' f a) => GFoldable' (M1 i c f) a where
  gfoldMap' f (M1 x) = gfoldMap' f x

instance (GFoldable' f a, GFoldable' g a) => GFoldable' (f :+: g) a where
  gfoldMap' f (L1 x) = gfoldMap' f x
  gfoldMap' f (R1 x) = gfoldMap' f x

instance (GFoldable' f a, GFoldable' g a) => GFoldable' (f :*: g) a where
  gfoldMap' f (x :*: y) = gfoldMap' f x `mappend` gfoldMap' f y
