{-# LANGUAGE NoFieldSelectors #-}

module Tourney.Match (
  -- * Matches
  Match (Match_, Match),
  createMatch,
  createCheckedMatch,
  validateMatch,
  likelyWinner,
  likelyLoser,
  matchIsWithin,
  matchIsReversal,

  -- ** Results
  MatchResult (..),
  Points (..),
  Result (..),
  didSlot1Win,
  didSlot2Win,
  winner,
  loser,
) where

import Data.Char (isNumber)
import Data.Ix qualified as Ix
import Data.Vector.Unboxed qualified as U
import GHC.IsList (IsList (..))
import GHC.Ix qualified as Ix
import Text.ParserCombinators.ReadP as R
import Text.Read as R
import Text.Show (Show (showsPrec))
import Tourney.Common (Focus (..), Slot, focusContains)

--------------------------------------------------------------------------------
-- Points

-- | A type for representing the score or points of a player.
--
-- The 'Eq', 'Ord', and 'Num' instances of this type operate element-wise on the
-- two inputs, treating values that are not present in one or the other, as
-- zeros.
--
-- You may construct this by its 'Num' instance which provides numeric literals
-- for 'Points', or via its 'IsList' instance, providing literal lists.
-- Alternatively, the constructor is provided to pass in a @ 'U.Vector' 'Int' @
-- directly.
--
-- @
-- Points [0,0,0] == Points [] = True
-- Points [0,0,1] == Points [1] = True
-- Points [1,0,1] + Points [2,2] = Points [1,2,3]
-- Points [1,0,1] + Points [2,2,2] = Points [3,2,3]
-- 1 == Points [1] = True -- fromInteger definition
-- @
newtype Points = Points (U.Vector Int)

instance Show Points where
  showsPrec _ (Points v) = (++) (intercalate "." (map show (toListOf each v)))

instance Read Points where
  readsPrec _ = readP_to_S pointsReader
    where
      pointsReader = do
        sections <- sepBy1 (many1 (satisfy isNumber)) (char '.')
        pure (Points (U.fromList (map (read :: String -> Int) sections)))

instance Eq Points where
  Points a == Points b = uncurry (coerce ((==) @(U.Vector Int))) (ensureSameLength a b)

instance Ord Points where
  compare (Points a) (Points b) = uncurry (coerce (compare @(U.Vector Int))) (ensureSameLength a b)

instance Num Points where
  fromInteger = Points . U.singleton . fromInteger
  (+) = alignedPointsBinOp (+)
  (-) = alignedPointsBinOp (-)
  (*) = alignedPointsBinOp (*)
  abs = coerce (U.map (abs @Int))
  negate = coerce (U.map (negate @Int))
  signum = coerce (U.map (signum @Int))

instance IsList Points where
  type Item Points = Int
  fromListN n p = Points (U.fromListN n p)
  fromList p = Points (U.fromList p)
  toList (Points p) = U.toList p

{-# INLINE alignedPointsBinOp #-}
alignedPointsBinOp :: (Int -> Int -> Int) -> Points -> Points -> Points
alignedPointsBinOp f (Points xs) (Points ys) = Points (uncurry (U.zipWith f) (ensureSameLength xs ys))

--------------------------------------------------------------------------------
-- Matches

-- | The basic type for a match is a pair of players. The 'Match' type ensures
-- by construction that the "lower" player always takes the first slot of the
-- 'Match', and the higher the second. Many functions in this library are
-- overloaded so that you can represent matches as ordinary Haskell tuples.
--
-- It is a runtime error to construct a match using the same player twice.
--
-- The first field of a 'Match' is always the smaller value. Matches with one
-- invalid 'Slot' for any reason should be interpretted as a bye for the valid
-- 'Slot' in that match.
data Match = Match_ !Slot !Slot
  deriving stock (Eq, Ord, Generic)

instance Show Match where
  showsPrec p (Match_ a b) = showsPrec p (a, b)

instance Ix.Ix Match where
  {-# INLINE range #-}
  {-# INLINE index #-}
  {-# INLINE unsafeIndex #-}
  {-# INLINE inRange #-}
  {-# INLINE rangeSize #-}
  {-# INLINE unsafeRangeSize #-}
  range (Match_ a0 b0, Match_ a1 b1) = uncurry Match_ <$> Ix.range ((a0, b0), (a1, b1))
  index (Match_ a0 b0, Match_ a1 b1) (Match_ i j) = Ix.index ((a0, b0), (a1, b1)) (i, j)
  unsafeIndex (Match_ a0 b0, Match_ a1 b1) (Match_ i j) = Ix.unsafeIndex ((a0, b0), (a1, b1)) (i, j)
  inRange (Match_ a0 b0, Match_ a1 b1) (Match_ i j) = Ix.inRange ((a0, b0), (a1, b1)) (i, j)
  rangeSize (Match_ a0 b0, Match_ a1 b1) = Ix.rangeSize ((a0, b0), (a1, b1))
  unsafeRangeSize (Match_ a0 b0, Match_ a1 b1) = Ix.unsafeRangeSize ((a0, b0), (a1, b1))

-- | Create a match, throwing an error if it has negative or equal slots
createMatch :: Slot -> Slot -> Match
createMatch a b
  | not (a >= 0 && b >= 0) = error ("invalid match due to negative indices: " <> show (a, b))
  | otherwise = case compare a b of
      LT -> Match_ a b
      GT -> Match_ b a
      EQ -> error ("invalid match due to non-unique players " <> show (a, b))

-- | Create a match within the given focus, returning 'Nothing' if it has negative or equal slots
{-# INLINE createCheckedMatch #-}
createCheckedMatch :: Focus -> Slot -> Slot -> Maybe Match
createCheckedMatch f a0 b0 = do
  m <- case compare a0 b0 of
    LT -> Just (Match_ a0 b0)
    GT -> Just (Match_ b0 a0)
    EQ -> Nothing
  guard (m `matchIsWithin` f)
  pure m

matchIsWithin :: Match -> Focus -> Bool
matchIsWithin (Match_ a b) f = focusContains f a && focusContains f b

validateMatch :: Focus -> Match -> Bool
validateMatch f (Match_ a b) = isJust (createCheckedMatch f a b)

likelyWinner, likelyLoser :: Lens' Match Slot
likelyWinner = position @1
likelyLoser = position @2

pattern Match :: Slot -> Slot -> Match
pattern Match a b <- Match_ a b
  where
    Match a b = createMatch a b

{-# COMPLETE Match #-}

data Result = Result !Points !Points
  deriving stock (Show, Eq, Ord, Generic)

data MatchResult = MatchResult
  { match :: !Match
  , result :: !Result
  }
  deriving stock (Show, Eq, Ord, Generic)

matchIsReversal :: MatchResult -> Bool
matchIsReversal (MatchResult _ (Result pa pb)) = pb > pa

didSlot1Win :: MatchResult -> Maybe Bool
didSlot1Win MatchResult{result = Result score1 score2} =
  case score1 `compare` score2 of
    LT -> Just True
    EQ -> Nothing
    GT -> Just False

didSlot2Win :: MatchResult -> Maybe Bool
didSlot2Win MatchResult{result = Result score1 score2} =
  case score2 `compare` score1 of
    LT -> Just True
    EQ -> Nothing
    GT -> Just False

winner :: MatchResult -> Maybe (Slot, Points)
winner m = didSlot1Win m <&> getResult m

loser :: MatchResult -> Maybe (Slot, Points)
loser m = didSlot2Win m <&> getResult m . not

getResult :: MatchResult -> Bool -> (Slot, Points)
getResult m didP1Win =
  if didP1Win
    then (m ^. #match . position @1, m ^. #result . position @1)
    else (m ^. #match . position @2, m ^. #result . position @2)

--------------------------------------------------------------------------------
-- Internals

{-# INLINE ensureSameLength #-}
ensureSameLength :: U.Vector Int -> U.Vector Int -> (U.Vector Int, U.Vector Int)
ensureSameLength xs ys
  | U.length xs == U.length ys = (xs, ys)
  | otherwise =
      let
        addPrefix v = U.replicate (len - U.length v) 0 <> v
        !len = max (U.length xs) (U.length ys)
        !pxs = addPrefix xs
        !pys = addPrefix ys
      in
        (pxs, pys)
