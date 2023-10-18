module Data.Lens.Deferred where

import GHC.TypeLits

data Crystal
  = Field Symbol
  | Index Natural

data Optic (parts :: [Crystal]) = Optic
