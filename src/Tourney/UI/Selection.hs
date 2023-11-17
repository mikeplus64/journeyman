module Tourney.UI.Selection (
  Selection,
  new,
  merge,
  moveRight,
  moveLeft,
  current,
) where

import Data.List.NonEmpty.Zipper (Zipper)
import Data.List.NonEmpty.Zipper qualified as Zipper
import Tourney.Common
import Tourney.Match
import Tourney.Match.Matrix

newtype Selection = Selection {getZipper :: Maybe (Zipper (RoundNo, Zipper Match))}
  deriving stock (Show)

-- | Create a selection zipper from the round/match matrix.
new :: MapByRound (MapByMatches (Maybe Result)) -> Selection
new matches = Selection do
  (r : rs) <- Just (map (second Zipper.fromNonEmpty) (matchRoundList matches))
  pure (Zipper.fromNonEmpty (r :| rs))

-- | Merge the selection with the current round/match matrix. Useful for when
-- that gets updated
merge :: MapByRound (MapByMatches (Maybe Result)) -> Selection -> Selection
merge m (Selection Nothing) = new m
merge m (Selection (Just s0)) = Selection do
  getZipper (new m) <&> moveToSelection0
  where
    moveToSelection0 z
      | (rd, ms) <- Zipper.current z
      , rd == selectedRound =
          Zipper.replace (rd, moveToSelection1 ms) z
      | Just z' <- Zipper.right z = moveToSelection0 z'
      | otherwise = z
    moveToSelection1 z
      | cm <- Zipper.current z, cm == selectedMatch = z
      | Just z' <- Zipper.right z = moveToSelection1 z'
      | otherwise = z
    (selectedRound, selectedMatches) = Zipper.current s0
    selectedMatch = Zipper.current selectedMatches

-- | Move the selection one position to the right
moveRight :: Selection -> Selection
moveRight (Selection mz) = Selection do
  z <- mz
  let (curRoundNo, curMatches) = Zipper.current z
  case (Zipper.isEnd curMatches, Zipper.isEnd z) of
    (True, False) -> Zipper.right z
    (False, _) -> do
      nextMatches <- Zipper.right curMatches
      pure (Zipper.replace (curRoundNo, nextMatches) z)
    _ -> pure z

-- | Move the selection one position to the left
moveLeft :: Selection -> Selection
moveLeft (Selection mz) = Selection do
  z <- mz
  let (curRoundNo, curMatches) = Zipper.current z
  case (Zipper.isStart curMatches, Zipper.isStart z) of
    (True, False) -> Zipper.left z
    (False, _) -> do
      nextMatches <- Zipper.left curMatches
      pure (Zipper.replace (curRoundNo, nextMatches) z)
    _ -> pure z

-- | Get the current selected round/match
current :: Selection -> Maybe (RoundNo, Match)
current (Selection mz) = do
  (roundNo, matches) <- Zipper.current <$> mz
  pure (roundNo, Zipper.current matches)

--------------------------------------------------------------------------------0
-- Internals

matchRoundList :: MapByRound (MapByMatches a) -> [(RoundNo, NonEmpty Match)]
matchRoundList mm = onlyNonEmptyRounds
  where
    onlyNonEmptyRounds = [(rd, m :| ms) | (rd, m : ms) <- roundList]
    roundList = map (_2 %~ map fst . itoList) (itoList mm)
