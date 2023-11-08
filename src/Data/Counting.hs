{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Counting (
  Counting (..),
  counting,
  uncounting,
  countAccToVec,
  uncons1,

  -- * Type synonyms
  CountList,
  CountDList,
  CountAcc,
  CountNeAcc,
) where

import Acc qualified
import Acc.NeAcc qualified as NeAcc
import Control.Lens
import Control.Monad (liftM2)
import Data.Align
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Foldable (Foldable (..))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Prelude hiding (drop, elem, product, sum, take, uncons)

data Counting f a = Counting !Int (f a)
  deriving stock (Show, Eq, Ord, Functor, Traversable)

type CountList = Counting []
type CountDList = Counting DList
type CountAcc = Counting Acc.Acc
type CountNeAcc = Counting NeAcc.NeAcc

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

instance (forall x y. Cons (f x) (f y) x y, forall x. Monoid (Counting f x)) => Cons (Counting f a) (Counting f b) a b where
  {-# INLINE _Cons #-}
  _Cons = prism (uncurry consCounting) unconsCounting
    where
      {-# INLINE consCounting #-}
      {-# INLINE unconsCounting #-}
      consCounting :: b -> Counting f b -> Counting f b
      consCounting x (Counting n xs) = Counting (n + 1) (x :< xs)

      unconsCounting :: Counting f a -> Either (Counting f b) (a, Counting f a)
      unconsCounting (Counting n xss)
        | n > 0, x :< xs <- xss = Right (x, Counting (n - 1) xs)
        | otherwise = Left mempty

instance (forall x y. Snoc (f x) (f y) x y, forall x. Monoid (Counting f x)) => Snoc (Counting f a) (Counting f b) a b where
  {-# INLINE _Snoc #-}
  _Snoc = prism (uncurry snocCounting) unsnocCounting
    where
      {-# INLINE snocCounting #-}
      {-# INLINE unsnocCounting #-}
      snocCounting :: Counting f b -> b -> Counting f b
      snocCounting (Counting n xs) x = Counting (n + 1) (xs :> x)

      unsnocCounting :: Counting f a -> Either (Counting f b) (Counting f a, a)
      unsnocCounting (Counting n xss)
        | n > 0, xs :> x <- xss = Right (Counting (n - 1) xs, x)
        | otherwise = Left mempty

instance (Applicative m, Foldable m, forall x. Monoid (m x)) => Applicative (Counting m) where
  pure a = Counting 1 (pure a)
  liftA2 = liftM2

instance (Applicative m, Foldable m, forall x. Monoid (m x)) => Monad (Counting m) where
  x >>= f = foldMap f x

counting :: Foldable f => f a -> Counting f a
counting a = Counting (length a) a

uncounting :: Counting f a -> f a
uncounting (Counting _ l) = l

countAccToVec :: CountAcc a -> Vector a
countAccToVec (Counting l a) = V.fromListN l (toList a)

instance Semialign f => Semialign (Counting f) where
  align (Counting la a) (Counting lb b) = Counting (max la lb) (align a b)

instance Align f => Align (Counting f) where
  nil = Counting 0 nil

-- class PrefixSlice f where
--   take :: Int -> f a -> f a
--   drop :: Int -> f a -> f a
--   default drop :: (Cons (f a) (f a) a a) => Int -> f a -> f a
--   default take :: (Cons (f a) (f a) a a, Monoid (f a)) => Int -> f a -> f a
--   drop = go
--     where
--       go i xss
--         | i > 0, Just (_, xs) <- uncons xss = go (i - 1) xs
--         | otherwise = xss
--   take = go
--     where
--       go i xss
--         | i > 0, Just (x, xs) <- uncons xss = x `cons` go (i - 1) xs
--         | otherwise = mempty

-- instance PrefixSlice CountList where
--   take n (Counting len xs) = Counting (min n len) (L.take n xs)
--   drop n (Counting len xs) = Counting (min (len - n) len) (L.drop n xs)

-- instance PrefixSlice Acc.Acc

-- instance PrefixSlice S.Seq where
--   take = S.take
--   drop = S.drop

-- instance Slice CountAcc where

--------------------------------------------------------------------------------
-- Orphans

instance One (Acc.Acc a) where
  type OneItem (Acc.Acc a) = a
  one a = Acc.cons a mempty

instance One (NeAcc.NeAcc a) where
  type OneItem (NeAcc.NeAcc a) = a
  one a = fromList [a]

instance One (DList a) where
  type OneItem (DList a) = a
  one = DL.singleton

instance Cons (Acc.Acc a) (Acc.Acc b) a b where
  _Cons = prism (uncurry Acc.cons) \a -> case Acc.uncons a of
    Just (x, xs) -> Right (x, xs)
    Nothing -> Left mempty

instance Snoc (Acc.Acc a) (Acc.Acc b) a b where
  _Snoc = prism (uncurry (flip Acc.snoc)) \a -> case Acc.unsnoc a of
    Just (xs, x) -> Right (x, xs)
    Nothing -> Left mempty

uncons1
  :: forall f g a
   . (Cons (f (g a)) (f (g a)) (g a) (g a), Cons (g a) (g a) a a)
  => f (g a)
  -> Maybe (a, f (g a))
uncons1 = withPrism _ConsF \consF unconsF -> withPrism _ConsG \_consG unconsG fga -> do
  (ga, fgas) <- rightToMaybe (unconsF fga)
  (a, gas) <- rightToMaybe (unconsG ga)
  pure (a, consF (gas, fgas))
  where
    _ConsF :: APrism (f (g a)) (f (g a)) (g a, f (g a)) (g a, f (g a))
    _ConsF = _Cons
    _ConsG :: APrism (g a) (g a) (a, g a) (a, g a)
    _ConsG = _Cons
