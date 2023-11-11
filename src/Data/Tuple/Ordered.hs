{-# LANGUAGE NoFieldSelectors #-}

module Data.Tuple.Ordered (
  OrdPair,
  pattern OrdPair_,
  pattern OrdPair,
  larger,
  smaller,
  offset,
  multiply,
)
where

import Control.Lens

data OrdPair a = LH !a !a
  deriving stock (Eq, Ord, Show, Foldable)

larger, smaller :: Ord a => Lens' (OrdPair a) a
larger = lens (\(LH _ h) -> h) (\(LH l _) h -> OrdPair_ l h)
smaller = lens (\(LH l _) -> l) (\(LH _ h) l -> OrdPair_ l h)

offset :: Num a => a -> OrdPair a -> OrdPair a
offset a (LH l h) = LH (l + a) (h + a)

multiply :: (Ord a, Num a) => a -> OrdPair a -> OrdPair a
multiply a (LH l h)
  | a > 0 = LH (l * a) (h * a)
  | a < 0 = LH (h * a) (l * a)
  | otherwise = error "Data.Tuple.Ordered.multiply: negative operand"

pattern OrdPair :: Ord a => a -> a -> Maybe (OrdPair a)
pattern OrdPair mlow mhigh <- Just (LH mlow mhigh)
  where
    OrdPair a b = case compare a b of
      LT -> Just (LH a b)
      GT -> Just (LH b a)
      EQ -> Nothing

{-# COMPLETE OrdPair_ :: OrdPair #-}
pattern OrdPair_ :: Ord a => a -> a -> OrdPair a
pattern OrdPair_ low high <- LH low high
  where
    OrdPair_ a b = case compare a b of
      LT -> LH a b
      GT -> LH b a
      EQ -> error "OrdPair_: got equal inputs"
