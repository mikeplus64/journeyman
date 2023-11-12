-- | Common types and functions
module Tourney.Common where

import Data.Bits
import Data.Tuple.Ordered
import Data.Vector.Unboxed qualified as U

-- | The basic type for a player is simply their _current_ index. That is, the
-- position they occupy in equivalent sorting network of a round, at the
-- beginning of that round.
type Player = Int

-- | A slot in the sorting network
type Slot = Int

-- | Round number
type RoundNo = Int

-- | A mapping from slots to players
type Standings = U.Vector Player

-- | The focus of a tournament over a sorting network
data Focus = Focus {focusStart, focusLength :: Int}
  deriving stock (Show, Eq, Ord, Generic)

isWithin :: Focus -> Focus -> Bool
isWithin (Focus a l) (Focus b n) = b >= a && (b + n) <= (a + l)

createInitialStandings :: PlayerCount -> Standings
createInitialStandings count = U.enumFromTo 0 (count - 1)

playerPosition :: Standings -> Slot -> Player
playerPosition = (U.!)

type PlayerCount = Int

-- | Create an increasing [closed, open) interval
(..<) :: Int -> Int -> [Int]
(..<) a b = [a .. b - 1]

-- | Create a decreasing [closed, open) interval
(..>) :: Int -> Int -> [Int]
(..>) a b = [a, a - 1 .. b + 1]

infix 5 ..<
infix 5 ..>

-- | Expand an index to 2 dimensions
stride2 :: Int -> OrdPair Int
stride2 i = OrdPair_ (2 * i) (2 * i + 1)

-- | Round to the nearest power of 2 above the input
nearestPow2Above :: Int -> Int
nearestPow2Above n
  | popCount n == 1 = n
  | otherwise = bit (bitLog2 n + 1)

-- | Integer log2
bitLog2 :: Int -> Int
bitLog2 n = fromIntegral (finiteBitSize n - countLeadingZeros n - 1)

-- | Integer square root
isqrt :: Int -> Int
isqrt = floor . sqrt . (fromIntegral :: Int -> Double)

-- | Generate the N-th triangular number
tri :: Int -> Int
tri n = n * (n - 1) `div` 2
