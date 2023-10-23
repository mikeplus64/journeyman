{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Counting (Counting (..), HasCount (..), counting, alignCounting) where

import Acc qualified
import Acc.NeAcc qualified as NeAcc
import Data.Align
import Data.These
import Data.Vector qualified as V

data Counting a = Counting !Int a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (Counting a) where
  Counting an a <> Counting bn b = Counting (an + bn) (a <> b)

instance Monoid a => Monoid (Counting a) where
  mempty = Counting 0 mempty

instance One a => One (Counting a) where
  type OneItem (Counting a) = OneItem a
  one = Counting 1 . one

counting :: Foldable f => f a -> Counting (f a)
counting a = Counting (length a) a

class HasCount a where
  count :: a -> Int

instance HasCount (V.Vector a) where
  count = length

instance HasCount (Counting a) where
  count (Counting l _) = l

instance Semialign Counting where
  align (Counting la a) (Counting lb b) = Counting (max la lb) (These a b)

alignCounting :: Align f => (These a b -> c) -> Counting (f a) -> Counting (f b) -> Counting (f c)
alignCounting f (Counting al fa) (Counting bl fb) =
  Counting (max al bl) (alignWith f fa fb)

--------------------------------------------------------------------------------
-- Orphans

instance One (Acc.Acc a) where
  type OneItem (Acc.Acc a) = a
  one a = Acc.cons a mempty

instance One (NeAcc.NeAcc a) where
  type OneItem (NeAcc.NeAcc a) = a
  one a = fromList [a]
