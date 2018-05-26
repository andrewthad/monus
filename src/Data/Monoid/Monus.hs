{-# language TypeFamilies #-}

-- | A <https://en.wikipedia.org/wiki/Monus commutative monoid with monus>
-- is a 'Monoid' equipped with a subtraction operator.
module Data.Monoid.Monus
  ( Monus(..)
  , (-)
  ) where

import Prelude hiding ((-))
import Data.Set (Set)
import Data.Monoid (Any(..),All(..),Sum(..))
import Control.Applicative (liftA2)
import Numeric.Natural (Natural)

import qualified Prelude as P
import qualified Data.Set as S

infixl 6 -

-- | A commutative monoid that supports subtraction. The following
-- laws must hold:
--
-- > x <> (y - x) = y <> (x - y)
-- > (x - y) - z = x - (y <> z)
-- > x - x = mempty
-- > mempty - x = mempty
class Monoid a => Monus a where
  monus :: a -> a -> a

-- | An infix synonym for 'subtraction'.
(-) :: Monus a => a -> a -> a
(-) = monus

instance Ord a => Monus (Set a) where
  monus = S.difference

-- | Unlike the subtraction provided by the 'Num' instance of
-- 'Natural', this subtraction is total.
instance (a ~ Natural) => Monus (Sum a) where
  monus (Sum x) (Sum y) = Sum (if x > y then x P.- y else 0)

-- | Defined as @P - Q = P ∧ ¬Q@
instance Monus Any where
  monus (Any x) (Any y) = case x of
    False -> Any False
    True -> Any (not y)

-- | Defined as @P - Q = P ∨ ¬Q@
instance Monus All where
  monus (All x) (All y) = case x of
    False -> All (not y)
    True -> All True

instance Monus () where
  monus _ _ = ()

instance (Monus a, Monus b) => Monus (a,b) where
  monus (a1,b1) (a2,b2) = (monus a1 a2,monus b1 b2)

instance (Monus a, Monus b, Monus c) => Monus (a,b,c) where
  monus (a1,b1,c1) (a2,b2,c2) = (monus a1 a2,monus b1 b2,monus c1 c2)

instance (Monus a, Monus b, Monus c,Monus d) => Monus (a,b,c,d) where
  monus (a1,b1,c1,d1) (a2,b2,c2,d2) =
    (monus a1 a2,monus b1 b2,monus c1 c2,monus d1 d2)

instance (Monus a, Monus b, Monus c,Monus d,Monus e) => Monus (a,b,c,d,e) where
  monus (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = 
    (monus a1 a2,monus b1 b2,monus c1 c2,monus d1 d2,monus e1 e2)

instance Monus b => Monus (a -> b) where
  monus = liftA2 monus

instance Monus a => Monus (IO a) where
  monus = liftA2 monus
