{-# LANGUAGE FieldSelectors #-}

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

winner1, loser1 :: MatchResult -> Maybe Bool
winner1 MatchResult {score1, score2} =
  case score1 `compare` score2 of
    LT -> Just True
    EQ -> Nothing
    GT -> Just False
loser1 MatchResult {score1, score2} =
  case score2 `compare` score1 of
    LT -> Just True
    EQ -> Nothing
    GT -> Just False

winner, loser :: MatchResult -> Maybe Player
winner m = winner1 m <&> \b -> m & if b then player1 else player2
loser m = loser1 m <&> \b -> m & if b then player1 else player2

winnerPoints, loserPoints :: MatchResult -> Maybe (U.Vector Int)
winnerPoints m = winner1 m <&> \b -> m & if b then score1 else score2
loserPoints m = loser1 m <&> \b -> m & if b then score1 else score2

data MatchPoints
  = MatchPoints
      {-# UNPACK #-} !(U.Vector Int)
      {-# UNPACK #-} !(U.Vector Int)
  deriving stock (Show, Eq)
