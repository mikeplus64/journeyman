module Data.HighLow (HighLow, highLow) where

data HighLow a = HighLow !a !a
  deriving (Eq, Ord, Show, Read)

highLow :: Ord a => a -> a -> Maybe (HighLow a)
highLow a b = case compare a b of
  LT -> Just (HighLow a b)
  GT -> Just (HighLow b a)
  EQ -> Nothing

high :: HighLow a -> a
high (HighLow h _) = h

low :: HighLow a -> a
low (HighLow _ l) = l
