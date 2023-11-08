module Tourney.SortingNetwork where

import Control.Lens
import Control.Monad.Primitive
import Data.Generics.Labels ()
import Data.Tuple.Ordered
import Data.Vector (Vector)
import Data.Vector.Algorithms.Intro qualified as IntroSort
import Data.Vector.Mutable qualified as VM
import Tourney.Algebra.Step
import Tourney.Match

runMatchesBy
  :: MonadPrim s m
  => (Match -> m MatchResult)
  -> Sorter
  -> Vector Match
  -> VM.MVector s (Points, a)
  -> m ()
runMatchesBy runMatch (Sorter focus method) matches mut = do
  results <- mapM runMatch matches
  case method of
    WinnerTakesHigh -> runSwaps results mut
    PointsAward alterPoints -> runPoints focus (fmap alterPoints results) mut

runSwaps :: MonadPrim s m => Vector MatchResult -> VM.MVector s (Points, a) -> m ()
runSwaps matches ranking =
  forM_ matches \r@MatchResult{player1, player2} -> do
    let OrdPair_ ilow ihigh = OrdPair_ player1 player2
    let result = liftA2 (,) (winner r) (loser r)
    forM_ result \((w, _), (l, _)) -> do
      VM.write ranking ihigh =<< VM.read ranking w
      VM.write ranking ilow =<< VM.read ranking l

runPoints
  :: MonadPrim s m
  => Focus
  -> Vector MatchResult
  -> VM.MVector s (Points, a)
  -> m ()
runPoints Focus{focusStart, focusLength} matches scores = do
  -- Add the points accumulated in the matches here
  forM_ matches \result -> do
    let w = winner result
    let l = loser result
    forM_ (liftA2 (,) w l) \((winPlayer, winPoints), (losePlayer, losePoints)) -> do
      VM.modify scores (_1 +~ winPoints) winPlayer
      VM.modify scores (_1 -~ losePoints) losePlayer
  -- Finally, sort
  IntroSort.sortByBounds (compare `on` fst) scores focusStart (focusStart + focusLength)

--------------------------------------------------------------------------------

isqrt, tri :: Int -> Int
isqrt = floor . sqrt . (fromIntegral :: Int -> Double)
tri n = n * (n - 1) `div` 2

-- The matches are represented by counting the pairs of combinations of matches
-- of P players as [ (a,b) | a <- [1..n], b <- [a+1..n] ] whose length is a
-- triangular number. This is so that it's impossible to feature the same player
-- twice within a round.
