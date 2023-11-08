{-# LANGUAGE FieldSelectors #-}

module Tourney.Match (
  -- * Players
  Player,

  -- * Matches
  Match,
  Points (..),
  MatchResult (..),
  matchOf,
  didPlayer1Win,
  didPlayer2Win,
  winner,
  loser,
) where

import Data.Tuple.Ordered
import Data.Vector.Unboxed qualified as U
import GHC.IsList (IsList (..))

-- | The basic type for a player is simply their _current_ index. That is, the
-- position they occupy in equivalent sorting network of a round, at the
-- beginning of that round.
type Player = Int

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
type Match = OrdPair Player

data MatchResult = MatchResult
  { player1, player2 :: !Player
  , score1, score2 :: !Points
  }
  deriving stock (Show, Eq)

matchOf :: MatchResult -> Match
matchOf MatchResult{player1, player2} = OrdPair_ player1 player2

didPlayer1Win, didPlayer2Win :: MatchResult -> Maybe Bool
didPlayer1Win MatchResult{score1, score2} =
  case score1 `compare` score2 of
    LT -> Just True
    EQ -> Nothing
    GT -> Just False
didPlayer2Win MatchResult{score1, score2} =
  case score2 `compare` score1 of
    LT -> Just True
    EQ -> Nothing
    GT -> Just False

winner, loser :: MatchResult -> Maybe (Player, Points)
winner m = didPlayer1Win m <&> \b -> if b then (player1 m, score1 m) else (player2 m, score2 m)
loser m = didPlayer2Win m <&> \b -> if b then (player1 m, score1 m) else (player2 m, score2 m)

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
