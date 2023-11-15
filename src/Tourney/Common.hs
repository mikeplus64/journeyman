-- | Common types and functions
--
-- # Arithmetic functions
-- Where appropriate these are overloaded to work with types that have the
-- same representation as 'Int', such as 'Player', 'Slot', and 'RoundNo'.
module Tourney.Common (
  -- * Types
  asInt,
  PlayerCount,
  Player (..),
  Slot (..),
  RoundNo (..),
  Standings (..),
  createInitialStandings,
  modifyStandings,

  -- ** Ranges
  (..<),
  (..>),

  -- ** Sort methods
  SortMethod (..),
  Sorter (..),

  -- ** Focus (think: slice of a vector)
  Focus (..),
  focusEnd,
  focusWithin,
  focusContains,

  -- * Arithmetic functions
  stride2,
  nearestPow2Above,
  bitLog2,
  isqrt,
  triangular,
) where

import Data.Bits
import Data.Ix
import Data.Tuple.Ordered
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Text.Show (Show (show))

type PlayerCount = Int

--------------------------------------------------------------------------------
-- Type-safe around numeric types
--
-- This prevents mixing up two different ideas, namely 'Slot' and 'Player', due
-- to their representations being the same.

asInt :: Coercible a Int => a -> Int
asInt = coerce

-- | The basic type for a player is simply their _current_ index. That is, the
-- position they occupy in equivalent sorting network of a round, at the
-- beginning of that round.
newtype Player = Player Int
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Ix, Bits)

newtype instance U.Vector Player = UV_Player (U.Vector Int)
newtype instance MU.MVector s Player = MUV_Player (MU.MVector s Int)
deriving newtype instance VGM.MVector U.MVector Player
deriving newtype instance VG.Vector U.Vector Player
instance U.Unbox Player

----------------------------------------

-- | A slot in the sorting network
newtype Slot = Slot Int
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Ix, Bits)

newtype instance U.Vector Slot = UV_Slot (U.Vector Int)
newtype instance MU.MVector s Slot = MUV_Slot (MU.MVector s Int)
deriving newtype instance VGM.MVector U.MVector Slot
deriving newtype instance VG.Vector U.Vector Slot
instance U.Unbox Slot

-- | Create an increasing [closed, open) interval of slots
(..<) :: (Enum a, Num a) => a -> a -> [a]
(..<) a b = [a .. b - 1]

-- | Create a decreasing [closed, open) interval of slots
(..>) :: (Enum a, Num a) => a -> a -> [a]
(..>) a b = [a, a - 1 .. b + 1]

infix 5 ..<
infix 5 ..>

----------------------------------------

-- | Round number
newtype RoundNo = RoundNo Int
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real, Ix, Bits)

newtype instance U.Vector RoundNo = UV_RoundNo (U.Vector Int)
newtype instance MU.MVector s RoundNo = MUV_RoundNo (MU.MVector s Int)
deriving newtype instance VGM.MVector U.MVector RoundNo
deriving newtype instance VG.Vector U.Vector RoundNo
instance U.Unbox RoundNo

--------------------------------------------------------------------------------
-- Standings of a sorting network

-- | A mapping from slots to players
newtype Standings = StandingsBySlot (U.Vector Player)

type instance Index Standings = Slot
type instance IxValue Standings = Slot
instance Ixed Standings where ix (Slot i) = coerced . ix @(U.Vector Int) i . coerced

createInitialStandings :: PlayerCount -> Standings
createInitialStandings count = StandingsBySlot (U.enumFromTo 0 (Player count - 1))

modifyStandings :: Standings -> (forall s. U.MVector s Player -> ST s ()) -> Standings
modifyStandings (StandingsBySlot v) f = StandingsBySlot (U.modify f v)

--------------------------------------------------------------------------------
-- Focii

-- | The focus of a tournament over a sorting network
data Focus = Focus {focusStart :: !Slot, focusLength :: !Int}
  deriving stock (Eq, Ord, Generic)

instance Show Focus where
  show f@Focus{focusStart = start} = [fmt|[{start:s}, {end:s})|]
    where
      end = focusEnd f

focusEnd :: Focus -> Slot
focusEnd Focus{focusStart, focusLength} = focusStart + Slot focusLength

focusWithin :: Focus -> Focus -> Bool
focusWithin (Focus (Slot a) l) (Focus (Slot b) n) = b >= a && (b + n) <= (a + l)

focusContains :: Focus -> Slot -> Bool
focusContains f s = focusStart f >= s && s <= focusEnd f

--------------------------------------------------------------------------------
-- Sorting methods

data SortMethod
  = WinnerTakesHigh
  | -- | Award points and use that to sort the results in the end
    PointsAward
  deriving stock (Show, Eq, Ord)

instance Default SortMethod where
  def = WinnerTakesHigh

data Sorter = Sorter {sorterFocus :: !Focus, sorterMethod :: !SortMethod}
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Arithmetic utility functions
--

-- | Expand an index to 2 dimensions
stride2 :: Int -> OrdPair Int
stride2 i = OrdPair_ (2 * i) (2 * i + 1)

-- | Round to the nearest power of 2 above the input
nearestPow2Above :: Coercible Int a => a -> a
nearestPow2Above = coerce nearestPow2AboveInt

nearestPow2AboveInt :: Int -> Int
nearestPow2AboveInt n
  | popCount n == 1 = n
  | otherwise = bit (bitLog2Int n + 1)

-- | Integer log2
bitLog2 :: Coercible Int a => a -> a
bitLog2 = coerce bitLog2Int

-- | Integer square root
isqrt :: Coercible Int a => a -> a
isqrt = coerce isqrtInt

-- | Generate the N-th triangular number
triangular :: Coercible Int a => a -> a
triangular = coerce triInt

--------------------------------------------------------------------------------
-- Internals
triInt :: Int -> Int
triInt n = n * (n - 1) `div` 2

isqrtInt :: Int -> Int
isqrtInt = floor . sqrt . (fromIntegral :: Int -> Double)

bitLog2Int :: Int -> Int
bitLog2Int n = fromIntegral (finiteBitSize n - countLeadingZeros n - 1)
