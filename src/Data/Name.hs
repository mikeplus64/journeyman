module Data.Name where

import Data.Counting
import Data.Dynamic

data Name a = Name Int

class NameStore s a where
  storeName :: a -> s -> (Name a, s)
  getName :: Name a -> s -> Maybe a

newtype DynamicNames = DynamicNames (Counting [Dynamic])

instance NameStore DynamicNames a where
  storeName a (DynamicNames (Counting n ds)) =
    DynamicNames (n + 1) (toDynamic a : ds)
