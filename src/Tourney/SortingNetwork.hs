module Tourney.SortingNetwork where

import Data.Generics.Labels ()
import Data.Vector.Algorithms.Intro qualified as IntroSort
import Data.Vector.Mutable qualified as VM
import Tourney.Algebra.Unified
import Tourney.Common
import Tourney.Match

--------------------------------------------------------------------------------

data SortAction
  = Swap !Slot !Slot
  deriving stock (Show, Eq)

runMatchesBy
  :: MonadPrim s m
  => Sorter
  -> Vector MatchResult
  -> VM.MVector s (Points, a)
  -> m [SortAction]
runMatchesBy (Sorter focus method) results mut =
  case method of
    WinnerTakesHigh -> runSwaps results mut
    PointsAward -> runPoints focus results mut

runSwaps :: MonadPrim s m => Vector MatchResult -> VM.MVector s (Points, a) -> m [SortAction]
runSwaps matches ranking =
  executingStateT [] $
    forM_ matches \r@MatchResult{match = Match ilow ihigh} -> do
      when (matchIsReversal r) do
        VM.swap ranking (fromSlot ihigh) (fromSlot ilow)
        modify' (Swap ihigh ilow :)

runPoints
  :: MonadPrim s m
  => Focus
  -> Vector MatchResult
  -> VM.MVector s (Points, a)
  -> m [SortAction]
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
  pure []

--------------------------------------------------------------------------------

-- The matches are represented by counting the pairs of combinations of matches
-- of P players as [ (a,b) | a <- [1..n], b <- [a+1..n] ] whose length is a
-- triangular number. This is so that it's impossible to feature the same player
-- twice within a round.
