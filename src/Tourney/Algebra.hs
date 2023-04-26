module Tourney.Algebra where

import Algebra.Graph.Labelled qualified as G
import Control.Lens
import Control.Monad (forM, forM_, when)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST.Strict
import Data.Generics.Labels ()
import Data.IntSet
import Data.List (sort, sortOn)
import Data.Monoid
import Data.Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Word
import GHC.Generics (Generic)
import Numeric.Natural
import Tourney.Result

-- type MatchID = (RoundNo, Int)

-- -- | A tournament network is a list of rounds in which each round is a set of matches.
-- --
-- -- The matches are represented by counting the pairs of combinations of matches of P players
-- -- as [ (a,b) | a <- [1..n], b <- [a+1..n] ] whose length is a triangular number.
-- -- This is so that it's impossible to feature the same player twice within a round.
-- type Network = [IntSet]

-- data Ranker s = Round (Set (Swap s))

data Ranker = Ranker
  { rounds :: [(MatchType, [(Int, Int)])]
  }

data MatchType
  = -- | The winner takes the highest rank
    CompareAndSwap
  | -- | Distribute points to N different point bins
    Points
  deriving stock (Show, Read, Eq, Ord, Generic)

runRankerM ::
  PrimMonad m =>
  (a -> a -> m (MatchResult a)) ->
  Ranker ->
  Vector a ->
  m [Vector (Integer, a)]
runRankerM runMatch Ranker {rounds} initialRanking = do
  ranking <- V.thaw initialRanking
  points <- V.thaw (V.replicate (V.length initialRanking) 0)
  forM rounds \(matchType, cmps) -> do
    forM_ cmps \(ia, ib) -> do
      a <- VM.read ranking ia
      b <- VM.read ranking ib
      result <- runMatch a b
      let ihigh = min ia ib
      let ilow = max ia ib
      case (matchType, result) of
        (CompareAndSwap, MatchResult {winner, loser}) -> do
          VM.write ranking ihigh (winner ^. #player)
          VM.write ranking ilow (loser ^. #player)
        (Points, MatchResult {winner, loser}) -> do
          VM.modify points (\p -> p + winner ^. #points) (winner ^. #index)
          VM.modify points (\p -> p + loser ^. #points) (loser ^. #index)
      when
        (matchType == Points)
        do
          ranking' <- V.freeze ranking
          points' <- V.freeze points
          let !next =
                V.fromListN
                  (V.length ranking')
                  ( sortOn
                      (\(p, _) -> p)
                      ( zip
                          (V.toList points')
                          (V.toList ranking')
                      )
                  )
          VM.copy points =<< V.thaw (V.map fst next)
          VM.copy ranking =<< V.thaw (V.map snd next)
    V.zip <$> V.freeze points <*> V.freeze ranking

--------------------------------------------------------------------------------

-- | Creates a round-robin with n participants.
roundRobin :: Int -> [(Int, Int)]
roundRobin n = [(a, b) | a <- [0 .. n - 1], b <- [a + 1 .. n - 1]]

isqrt, tri :: Int -> Int
isqrt = floor . sqrt . (fromIntegral :: Int -> Double)
tri n = n * (n - 1) `div` 2
