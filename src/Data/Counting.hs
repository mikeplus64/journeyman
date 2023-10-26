{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Counting (
  Counting (..),
  CountList,
  CountAcc,
  CountNeAcc,
  counting,
  uncounting,
) where

import Acc qualified
import Acc.NeAcc qualified as NeAcc
import Data.Align
import Data.Foldable (Foldable (..))
import Prelude hiding (elem, product, sum)

data Counting f a = Counting !Int (f a)
  deriving stock (Show, Eq, Ord, Functor, Traversable)

type CountList a = Counting []

type CountAcc a = Counting Acc.Acc
type CountNeAcc a = Counting NeAcc.NeAcc

instance Foldable f => Foldable (Counting f) where
  foldMap f = foldMap f . uncounting
  foldMap' f = foldMap' f . uncounting
  foldr f z = foldr f z . uncounting
  foldl f z = foldl f z . uncounting
  foldr1 f = foldr1 f . uncounting
  foldl1 f = foldl1 f . uncounting
  foldr' f z = foldr' f z . uncounting
  foldl' f z = foldl' f z . uncounting
  elem a = elem a . uncounting
  maximum = maximum . uncounting
  minimum = minimum . uncounting
  sum = sum . uncounting
  product = product . uncounting
  null (Counting i _) = i == 0
  length (Counting i _) = i

instance Semigroup (f a) => Semigroup (Counting f a) where
  Counting an a <> Counting bn b = Counting (an + bn) (a <> b)

instance Monoid (f a) => Monoid (Counting f a) where
  mempty = Counting 0 mempty

instance One (f a) => One (Counting f a) where
  type OneItem (Counting f a) = OneItem (f a)
  one = Counting 1 . one

counting :: Foldable f => f a -> Counting f a
counting a = Counting (length a) a

uncounting :: Counting f a -> f a
uncounting (Counting _ l) = l

instance Semialign f => Semialign (Counting f) where
  align (Counting la a) (Counting lb b) = Counting (max la lb) (align a b)

instance Align f => Align (Counting f) where
  nil = Counting 0 nil

--------------------------------------------------------------------------------
-- Orphans

instance One (Acc.Acc a) where
  type OneItem (Acc.Acc a) = a
  one a = Acc.cons a mempty

instance One (NeAcc.NeAcc a) where
  type OneItem (NeAcc.NeAcc a) = a
  one a = fromList [a]
