module Tourney.Algebra where
import Data.Set (Set)
import Data.Set qualified as Set

type MatchID = (Int, Int)

data MatchPlayer
  = Winner
  | Loser
  | High
  | Low

data T
  = Connect T MatchPlayer T
  | Overlay T T
  | Match MatchID
  | Empty

data Graph = Graph
  { matches :: !(Set MatchID)
  , round :: !Int
  , focus :: T
  }

type Graphing a = ()

round :: Graphing a -> Graphing a
round = undefined

match :: Graphing MatchID
match = undefined
