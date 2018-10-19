{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}

#if !MIN_VERSION_containers(0,5,9)
module Data.Map.Annihilate
  (
  ) where
#else

module Data.Map.Annihilate
  ( Map
  , singleton
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.Semigroup (Semigroup((<>)))
import Data.Monoid (Monoid(mempty))
import Data.Monoid.Monus (Monus(monus))
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM

-- todo: manually write Show instance
newtype Map k v = Map (M.Map k v)
  deriving (Eq,Ord,Functor,Foldable,Show)

singleton :: (Monoid v, Eq v) => k -> v -> Map k v
singleton k v = if v == mempty
  then Map M.empty
  else Map (M.singleton k v)

lookup :: (Ord k, Monoid v) => k -> Map k v -> v
lookup k (Map m) = fromMaybe mempty (M.lookup k m)

instance (Ord k, Monoid v, Eq v) => Semigroup (Map k v) where
  Map x <> Map y = Map
    ( MM.merge
      MM.dropMissing
      MM.dropMissing
      ( MM.zipWithMaybeMatched
        (\_ a b ->
          let c = a `mappend` b
           in if c == mempty then Nothing else Just c
        )
      )
      x 
      y
    )

instance (Ord k, Monoid v, Eq v) => Monoid (Map k v) where
  mempty = Map M.empty
#if !MIN_VERSION_base(4,11,0)
  mappend x y = x <> y
#endif

instance (Ord k, Monus v, Eq v) => Monus (Map k v) where
  monus (Map x) (Map y) = Map
    ( MM.merge
      MM.dropMissing
      MM.dropMissing
      ( MM.zipWithMaybeMatched
        (\_ a b ->
          let c = monus a b
           in if c == mempty then Nothing else Just c
        )
      )
      x 
      y
    )

#endif
