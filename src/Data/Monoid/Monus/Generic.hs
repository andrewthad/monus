{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.Monoid.Monus.Generic
  ( GMonus(..)
  , gmonus
  , WrappedMonus(..)
  ) where

import Data.Semigroup (Semigroup)
import GHC.Generics
import Data.Monoid.Monus
import Prelude hiding (Num(..))

-- | Useful with -XDerivingVia.
newtype WrappedMonus a = WrappedMonus a
  deriving (Generic, Semigroup, Monoid)

instance Monus a => Monus (WrappedMonus a) where
  monus = gmonus;

gmonus :: (Generic a, GMonus (Rep a)) => a -> a -> a
gmonus x y = to (from x `gmonus'` from y)

class GMonus f where
  {-# MINIMAL gmonus' #-}
  gmonus' :: f a -> f a -> f a

instance GMonus U1 where
  gmonus' _ _ = U1

instance (GMonus a, GMonus b) => GMonus (a :*: b) where
  gmonus' (a :*: b) (c :*: d) = gmonus' a c :*: gmonus' b d

instance GMonus a => GMonus (M1 i c a) where
  gmonus' (M1 x) (M1 y) = M1 (gmonus' x y)

instance Monus a => GMonus (K1 i a) where
  gmonus' (K1 x) (K1 y) = K1 (monus x y)
