{-# LANGUAGE NoFieldSelectors #-}

module Data.Tuple.Ordered (
  LowHigh,
  pattern LowHigh_,
  pattern LowHigh,
  high,
  low,
  offset,
  multiply,
)
where

import Control.Lens

data LowHigh a = LH !a !a
  deriving stock (Eq, Ord, Show)

high, low :: Ord a => Lens' (LowHigh a) a
high = lens (\(LH _ h) -> h) (\(LH l _) h -> LowHigh_ l h)
low = lens (\(LH l _) -> l) (\(LH _ h) l -> LowHigh_ l h)

instance Ord a => Field1 (LowHigh a) (LowHigh a) a a where _1 = low

instance Ord a => Field2 (LowHigh a) (LowHigh a) a a where _2 = high

offset :: Num a => a -> LowHigh a -> LowHigh a
offset a (LH l h) = LH (l + a) (h + a)

multiply :: (Ord a, Num a) => a -> LowHigh a -> LowHigh a
multiply a (LH l h)
  | a > 0 = LH (l * a) (h * a)
  | a < 0 = LH (h * a) (l * a)
  | otherwise = error "Data.Tuple.Ordered.multiply: negative operand"

pattern LowHigh :: Ord a => a -> a -> Maybe (LowHigh a)
pattern LowHigh mhigh mlow <- (id -> Just (LH mlow mhigh))
  where
    LowHigh a b = case compare a b of
      LT -> Just (LH a b)
      GT -> Just (LH b a)
      EQ -> Nothing

{-# COMPLETE LowHigh_ :: LowHigh #-}
pattern LowHigh_ :: Ord a => a -> a -> LowHigh a
pattern LowHigh_ high low <- (id -> LH low high)
  where
    LowHigh_ a b = case compare a b of
      LT -> LH a b
      GT -> LH b a
      EQ -> error "LowHigh_: got equal inputs"
