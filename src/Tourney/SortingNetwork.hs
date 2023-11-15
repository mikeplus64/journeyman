module Tourney.SortingNetwork where

import Data.Generics.Labels ()
import Data.Vector.Algorithms.Intro qualified as IntroSort
import Data.Vector.Mutable qualified as VM
import Tourney.Algebra.Unified
import Tourney.Common
import Tourney.Match

--------------------------------------------------------------------------------

runMatchesBy
  :: MonadPrim s m
  => Sorter
  -> Vector MatchResult
  -> VM.MVector s (Points, a)
  -> m ()
runMatchesBy (Sorter focus method) results mut =
  case method of
    WinnerTakesHigh -> runSwaps results mut
    PointsAward -> runPoints focus results mut

runSwaps :: MonadPrim s m => Vector MatchResult -> VM.MVector s (Points, a) -> m ()
runSwaps matches ranking =
  forM_ matches \r@MatchResult{match = Match (Slot ilow) (Slot ihigh)} -> do
    let result = liftA2 (,) (winner r) (loser r)
    forM_ result \((Slot w, _), (Slot l, _)) -> do
      VM.write ranking ihigh =<< VM.read ranking w
      VM.write ranking ilow =<< VM.read ranking l

runPoints
  :: MonadPrim s m
  => Focus
  -> Vector MatchResult
  -> VM.MVector s (Points, a)
  -> m ()
runPoints focus matches scores = do
  -- Add the points accumulated in the matches here
  forM_ matches \result -> do
    let w = winner result
    let l = loser result
    forM_ (liftA2 (,) w l) \((Slot winPlayer, winPoints), (Slot losePlayer, losePoints)) -> do
      VM.modify scores (_1 +~ winPoints) winPlayer
      VM.modify scores (_1 -~ losePoints) losePlayer
  -- Finally, sort
  IntroSort.sortByBounds (compare `on` fst) scores (focusStart focus & asInt) (focusEnd focus & asInt)

--------------------------------------------------------------------------------

-- The matches are represented by counting the pairs of combinations of matches
-- of P players as [ (a,b) | a <- [1..n], b <- [a+1..n] ] whose length is a
-- triangular number. This is so that it's impossible to feature the same player
-- twice within a round.
