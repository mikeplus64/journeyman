module Tourney.Algebra.Types where

import Data.Tuple.Ordered
import Data.Vector.Unboxed qualified as U
import Tourney.Match (Player)

type Standings = U.Vector Player

type PlayerCount = Int
