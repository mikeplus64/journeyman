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

import Data.Tuple.Ordered
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
  { rounds :: [Set (LowHigh Int)]
  , type_ :: MatchType
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
runRankerM runMatch Ranker {rounds, type_} initialRanking = do
  ranking <- V.thaw initialRanking
  points <- V.thaw (V.replicate (V.length initialRanking) 0)
  forM rounds \cmps -> do
    forM_ cmps \(LowHigh_ ilow ihigh) -> do
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
        CompareAndSwap -> do
          VM.write ranking ihigh (winner ^. #player)
          VM.write ranking ilow (loser ^. #player)
    V.zip <$> V.freeze points <*> V.freeze ranking

--------------------------------------------------------------------------------
--

slaughterSeeding :: Word32 -> Set (LowHigh (Maybe Word32))
slaughterSeeding 0 = Set.empty
slaughterSeeding 1 = Set.singleton (LowHigh_ Nothing (Just 0))
slaughterSeeding 2 = Set.singleton (LowHigh (Just 0) (Just 1))
slaughterSeeding p = _
  where
    n :: Word32
    n = 2 ^ (floor (logBase 2 (fromIntegral p :: Double)) :: Word32)

-- /// Create an array of matches between signup indices according to slaughter
-- /// seeding
-- pub fn slaughter_seeding(signups: u32) -> Vec<(Option<u32>, Option<u32>)> {
--     match signups {
--         0 => {
--             return vec![];
--         }
--         1 => {
--             return vec![(Some(0), None)];
--         }
--         2 => {
--             return vec![(Some(0), Some(1))];
--         }
--         _ => {}
--     };
--     let max_depth = (signups as f32).log2().ceil() as u32;
--     let winners = slaughter_seeding_flat(max_depth);
--     let mut r = Vec::new();
--     let check_signup = |i: u32| -> Option<u32> {
--         if i < signups {
--             Some(i)
--         } else {
--             None
--         }
--     };
--     for i in 0..(winners.len() / 2) {
--         let high = winners[i * 2].expect("no high seed index");
--         let low = winners[i * 2 + 1].expect("no low seed index");
--         r.push((check_signup(high), check_signup(low)));
--     }
--     r
-- }

-- singleElim :: Int -> Ranker
-- singleElim log2n =
--   Ranker
--     { type_ = CompareAndSwap
--     , rounds =
--       [ Set.fromList
--       [

--       ]
--       | r <- [0..log2n]
--       ]
--     }
--   where
--     n = 2^log2n

--------------------------------------------------------------------------------

-- | Creates a round-robin with n participants.
roundRobin :: Int -> [LowHigh Int]
roundRobin n = [LowHigh_ a b | a <- [0 .. n - 1], b <- [a + 1 .. n - 1]]

isqrt, tri :: Int -> Int
isqrt = floor . sqrt . (fromIntegral :: Int -> Double)
tri n = n * (n - 1) `div` 2
