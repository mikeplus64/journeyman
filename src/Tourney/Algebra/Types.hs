module Tourney.Algebra.Types where

import Data.Tuple.Ordered
import Data.Vector.Unboxed qualified as U

type Player = Int

type PlayerCount = Int

type Standings = U.Vector Player

type Match = LowHigh Int
