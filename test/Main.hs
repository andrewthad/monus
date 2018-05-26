{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language StandaloneDeriving #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

import Prelude hiding ((-))

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.SmallCheck.Series (Serial(series))

import Data.Proxy (Proxy(..))
import Data.Monoid (Any(..),All(..),Sum(..))
import Data.Set (Set)
import Data.Monoid.Monus (Monus,(-))
import Type.Reflection (typeRep,TypeRep)
import Numeric.Natural (Natural)

import qualified Data.Set as S
import qualified Data.Map.Annihilate as MA

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Monus"
  [ props (typeRep @Any)
  , props (typeRep @All)
  , props (typeRep @(Set Bool))
  , qcProps (typeRep @(Sum Natural))
  , qcProps (typeRep @(MA.Map Int (Set Int)))
  ]

props :: forall m a. (Serial IO a, Monus a, Eq a, Show a) => TypeRep a -> TestTree
props r = testGroup (show r)
  [ SC.testProperty "x <> (y - x) = y <> (x - y)" $
      \(x :: a) y -> x <> (y - x) == y <> (x - y)
  , SC.testProperty "(x - y) - z = x - (y <> z)" $
      \(x :: a) y z -> (x - y) - z == x - (y <> z)
  , SC.testProperty "x - x = mempty" $
      \(x :: a) -> x - x == mempty
  , SC.testProperty "mempty - x = mempty" $
      \(x :: a) -> mempty - x == mempty
  ]

qcProps :: forall m a. (Arbitrary a, Monus a, Eq a, Show a) => TypeRep a -> TestTree
qcProps r = testGroup (show r)
  [ QC.testProperty "x <> (y - x) = y <> (x - y)" $
      \(x :: a) y -> x <> (y - x) == y <> (x - y)
  , QC.testProperty "(x - y) - z = x - (y <> z)" $
      \(x :: a) y z -> (x - y) - z == x - (y <> z)
  , QC.testProperty "x - x = mempty" $
      \(x :: a) -> x - x == mempty
  , QC.testProperty "mempty - x = mempty" $
      \(x :: a) -> mempty - x == mempty
  ]

instance Monad m => Serial m Any where
  series = fmap Any series

instance Monad m => Serial m All where
  series = fmap All series

instance (Monad m, Ord a, Serial m a) => Serial m (Set a) where
  series = fmap S.fromList series

instance Arbitrary Natural where
  arbitrary = fmap (fromIntegral . abs) (arbitrary @Integer)

instance (Arbitrary k, Ord k, Arbitrary v, Monoid v, Eq v) => Arbitrary (MA.Map k v) where
  arbitrary = fmap (foldMap (\(k,v) -> MA.singleton k v)) (arbitrary @[(k,v)])

