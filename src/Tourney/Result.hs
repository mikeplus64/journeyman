module Tourney.Result where

import Control.Lens
import Data.Generics.Labels ()
import GHC.Generics (Generic)

data Origin = High | Low
  deriving stock (Show, Read, Eq, Ord, Generic)

data Result a = Result
  { origin :: !Origin
  , points :: !Integer
  , player :: !a
  , index :: !Int
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data MatchResult a = MatchResult
  { winner :: !(Result a)
  , loser :: !(Result a)
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

matchResult :: (Int, Integer, a) -> (Int, Integer, a) -> Maybe (MatchResult a)
matchResult (result1 High -> high) (result1 Low -> low) =
  case compare (high ^. #points) (low ^. #points) of
    LT -> Just MatchResult {winner = low, loser = high}
    EQ -> Nothing
    GT -> Just MatchResult {winner = high, loser = low}

result1 :: Origin -> (Int, Integer, a) -> Result a
result1 origin (index, points, player) = Result {..}
