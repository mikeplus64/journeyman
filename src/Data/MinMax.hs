module Data.MinMax where

import Data.Tuple.Ordered

data MinMax a
  = MinMax !a !a
  | MinMaxEmpty
  deriving stock (Eq, Ord, Show, Generic)

instance Ord a => Monoid (MinMax a) where
  mempty = MinMaxEmpty

instance Ord a => Semigroup (MinMax a) where
  MinMax min0 max0 <> MinMax min1 max1 = MinMax (min min0 min1) (max max0 max1)
  MinMaxEmpty <> a = a
  a <> MinMaxEmpty = a

instance One (MinMax a) where
  type OneItem (MinMax a) = a
  one = toMinMax

getMinMax :: MinMax a -> Maybe (a, a)
getMinMax (MinMax min' max') = Just (min', max')
getMinMax MinMaxEmpty = Nothing

toMinMax :: a -> MinMax a
toMinMax a = MinMax a a

ordPairToMinMax :: Ord a => OrdPair a -> MinMax a
ordPairToMinMax (OrdPair_ a b) = MinMax a b
