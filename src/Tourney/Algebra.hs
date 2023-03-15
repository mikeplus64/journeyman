module Tourney.Algebra where

newtype PlayerID = PlayerID Int
newtype MatchID = MatchID Int

data Player
  = LoserOf MatchId
  | WinnerOf MatchId
  | Seed Int

data T
  = Match MatchID Player
  | Connect T T
