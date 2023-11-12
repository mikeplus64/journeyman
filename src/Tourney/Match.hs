{-# LANGUAGE NoFieldSelectors #-}

module Tourney.Match (
  -- * Matches
  Match (Match),
  createMatch,
  createCheckedMatch,
  validateMatch,
  Points (..),
  MatchResult (..),
  Result (..),
  didPlayer1Win,
  didPlayer2Win,
  likelyWinner,
  likelyLoser,
  winner,
  loser,
) where

import Control.Lens
import Data.Generics.Labels ()
import Data.Generics.Product.Positions (position)
import Data.Vector.Unboxed qualified as U
import GHC.IsList (IsList (..))
import Tourney.Common (Focus (..), Player, Slot)

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
  deriving stock (Show)

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

-- | The basic type for a match is a pair of players. The 'OrdPair' type ensures
-- by construction that the "lower" player always takes the first slot of the
-- 'OrdPair', and the higher the second.
--
-- Many functions in this library are overloaded so that you can represent
-- matches as ordinary Haskell tuples.
--
-- It is a runtime error to construct a match using the same player twice.
-- type Match = OrdPair Player
data Match = Match_ Slot Slot
  deriving stock (Show, Eq, Ord, Generic)

createMatch :: Player -> Player -> Match
createMatch a b
  | not (a >= 0 && b >= 0) = error ("invalid match due to negative indices: " <> show (a, b))
  | otherwise = case compare a b of
      LT -> Match_ a b
      GT -> Match_ b a
      EQ -> error ("invalid match due to non-unique players " <> show (a, b))

createCheckedMatch :: Focus -> Player -> Player -> Maybe Match
createCheckedMatch Focus{focusStart, focusLength} a b = do
  guard (a >= 0 && b >= 0)
  guard (a /= b)
  let (l, h) = if a < b then (a, b) else (b, a)
  guard (l >= focusStart && h < focusStart + focusLength)
  pure (Match_ l h)

validateMatch :: Focus -> Match -> Bool
validateMatch f (Match_ a b) = isJust (createCheckedMatch f a b)

likelyWinner, likelyLoser :: Lens' Match Player
likelyWinner = position @1
likelyLoser = position @2

pattern Match :: Player -> Player -> Match
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

didPlayer1Win :: MatchResult -> Maybe Bool
didPlayer1Win MatchResult{result = Result score1 score2} =
  case score1 `compare` score2 of
    LT -> Just True
    EQ -> Nothing
    GT -> Just False

didPlayer2Win :: MatchResult -> Maybe Bool
didPlayer2Win MatchResult{result = Result score1 score2} =
  case score2 `compare` score1 of
    LT -> Just True
    EQ -> Nothing
    GT -> Just False

winner :: MatchResult -> Maybe (Player, Points)
winner m = didPlayer1Win m <&> getResult m

loser :: MatchResult -> Maybe (Player, Points)
loser m = didPlayer2Win m <&> getResult m . not

getResult :: MatchResult -> Bool -> (Player, Points)
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
        prefixed v = U.replicate (len - U.length v) 0 <> v
        !len = max (U.length xs) (U.length ys)
        !pxs = prefixed xs
        !pys = prefixed ys
      in
        (pxs, pys)
