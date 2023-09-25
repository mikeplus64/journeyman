module Tourney.SortingNetwork where

import Control.Lens
import Control.Monad.Primitive (PrimMonad)
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM

import Data.Tuple.Ordered
import Tourney.Result

type Player = Int

-- | A tournament network is a list of rounds in which each round is a set of matches.
--
-- The matches are represented by counting the pairs of combinations of matches of P players
-- as [ (a,b) | a <- [1..n], b <- [a+1..n] ] whose length is a triangular number.
-- This is so that it's impossible to feature the same player twice within a round.
data Round = Round
  { matches :: Set (LowHigh Int)
  , type_ :: MatchType
  }
  deriving stock (Show, Generic)

data MatchType
  = -- | The winner takes the highest rank
    Swaps
  | -- | Distribute points to N different point bins
    Points
  deriving stock (Show, Read, Eq, Ord, Generic)

runRoundM
  :: (PrimMonad m)
  => (a -> a -> m (MatchResult a))
  -> Round
  -> Vector a
  -> m (Vector (Integer, a))
runRoundM runMatch Round {matches, type_} initialRanking = do
  ranking <- V.thaw initialRanking
  points <- V.thaw (V.replicate (V.length initialRanking) 0)
  forM_ matches \(LowHigh_ ilow ihigh) -> do
    a <- VM.read ranking ilow
    b <- VM.read ranking ihigh
    MatchResult {winner, loser} <- runMatch a b
    case type_ of
      Points -> do
        VM.modify points (\p -> p + winner ^. #points) (winner ^. #index)
        VM.modify points (\p -> p + loser ^. #points) (loser ^. #index)
        ranking' <- V.freeze ranking
        points' <- V.freeze points
        let !next =
              V.fromListN
                (V.length ranking')
                ( sortOn
                    fst
                    ( zip
                        (V.toList points')
                        (V.toList ranking')
                    )
                )
        VM.copy points =<< V.thaw (V.map fst next)
        VM.copy ranking =<< V.thaw (V.map snd next)
      Swaps -> do
        VM.write ranking ihigh (winner ^. #player)
        VM.write ranking ilow (loser ^. #player)
  V.zip <$> V.freeze points <*> V.freeze ranking

--------------------------------------------------------------------------------

isqrt, tri :: Int -> Int
isqrt = floor . sqrt . (fromIntegral :: Int -> Double)
tri n = n * (n - 1) `div` 2
