module Tourney.Match where

import Data.Tuple.Ordered
import Data.Vector.Unboxed qualified as U

type Player = Int

type Match = LowHigh Player

data MatchResult = MatchResult
  { player1, player2 :: !Player
  , score1, score2 :: !(U.Vector Int)
  }
  deriving stock (Show, Eq)

newtype MatchPoints = MatchPoints (U.Vector (Int, Int))
  deriving stock (Show, Eq)
