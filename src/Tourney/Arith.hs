module Tourney.Arith where

import Data.Bits
import Data.Tuple.Ordered

-- | Create a rising [closed, open) interval
(..<) :: Int -> Int -> [Int]
(..<) a b = [a .. b - 1]

-- | Create a decreasing [closed, open) interval
(..>) :: Int -> Int -> [Int]
(..>) a b = [a, a - 1 .. b + 1]

infix 5 ..<
infix 5 ..>

fromPow2 :: Int -> [Int]
fromPow2 n = reverse [0 .. 2 ^ n - 1]

stride2 :: Int -> OrdPair Int
stride2 i = OrdPair_ (2 * i) (2 * i + 1)

-- | Round to the nearest power of 2 above the input
nearestPow2Above :: Int -> Int
nearestPow2Above n
  | popCount n == 1 = n
  | otherwise = bit (bitLog2 n + 1)

bitLog2 :: Int -> Int
bitLog2 n = fromIntegral (finiteBitSize n - countLeadingZeros n - 1)
