module Data.Tuple.Ordered (
  LowHigh,
  pattern LowHigh_,
  pattern LowHigh,
)
where

data LowHigh a = LH !a !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

pattern LowHigh :: Ord a => a -> a -> Maybe (LowHigh a)
pattern LowHigh {high, low} <- (id -> Just (LH low high))
  where
    LowHigh a b = case compare a b of
      LT -> Just (LH a b)
      GT -> Just (LH b a)
      EQ -> Nothing

{-# COMPLETE LowHigh_ :: LowHigh #-}
pattern LowHigh_ :: Ord a => a -> a -> LowHigh a
pattern LowHigh_ {high, low} <- (id -> LH low high)
  where
    LowHigh_ a b = case compare a b of
      LT -> LH a b
      GT -> LH b a
      EQ -> error "LowHigh_: got equal inputs"
