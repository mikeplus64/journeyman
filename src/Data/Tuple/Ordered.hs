module Data.Tuple.Ordered (
  LowHigh,
  pattern LowHigh_,
  pattern LowHigh,
)
where

data LowHigh a = LowHigh' !a !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

pattern LowHigh :: Ord a => a -> a -> Maybe (LowHigh a)
pattern LowHigh high low <- (id -> Just (LowHigh' low high))
  where
    LowHigh a b = case compare a b of
      LT -> Just (LowHigh' a b)
      GT -> Just (LowHigh' b a)
      EQ -> Nothing

{-# COMPLETE LowHigh_ :: LowHigh #-}
pattern LowHigh_ :: Ord a => a -> a -> LowHigh a
pattern LowHigh_ high low <- (id -> LowHigh' low high)
  where
    LowHigh_ a b = case compare a b of
      LT -> LowHigh' a b
      GT -> LowHigh' b a
      EQ -> error "LowHigh_: got equal inputs"
