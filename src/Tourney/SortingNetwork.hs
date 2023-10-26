module Tourney.SortingNetwork where

import Control.Lens
import Control.Monad.ST.Strict
import Data.Generics.Labels ()
import Data.Tuple.Ordered
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as U
import Tourney.Algebra.Step
import Tourney.Algebra.Types
import Tourney.Match

-- | A single round of a sorting network, expressed as a list of matches.
newtype SortingRound = SortingRound [(Maybe (MatchResult -> MatchPoints), LowHigh Int)]

runSwaps :: [MatchResult] -> Vector a -> Vector a
runSwaps matches = V.modify \ranking ->
  forM_ matches \r@MatchResult {player1, player2} -> do
    let LowHigh_ ilow ihigh = LowHigh_ player1 player2
    let result = do w <- winner r; l <- loser r; pure (w, l)
    forM_ result \(w, l) -> do
      VM.write ranking ihigh =<< VM.read ranking w
      VM.write ranking ilow =<< VM.read ranking l

runPoints :: [(MatchResult, MatchPoints)] -> Vector (U.Vector Int, a) -> Vector (U.Vector Int, a)
runPoints matches =
  sortByPoints . V.modify \ranking -> do
    forM_ matches \(r, MatchPoints points1 points2) -> do
      VM.modify ranking (first (U.zipWith (+) points1)) (player1 r)
      VM.modify ranking (first (U.zipWith (+) points2)) (player2 r)
  where
    sortByPoints v = V.fromListN (V.length v) (sortOn fst (V.toList v))

focussed :: Focus -> (Vector a -> Vector b) -> Vector a -> Vector (Either a b)
focussed Focus {focusStart, focusLength} f v = V.concat [before, middle, after]
  where
    before = V.map Left (V.take focusStart v)
    middle = V.map Right (f v)
    after = V.map Left (V.drop (focusStart + focusLength) v)

--------------------------------------------------------------------------------

isqrt, tri :: Int -> Int
isqrt = floor . sqrt . (fromIntegral :: Int -> Double)
tri n = n * (n - 1) `div` 2

-- The matches are represented by counting the pairs of combinations of matches
-- of P players as [ (a,b) | a <- [1..n], b <- [a+1..n] ] whose length is a
-- triangular number. This is so that it's impossible to feature the same player
-- twice within a round.
