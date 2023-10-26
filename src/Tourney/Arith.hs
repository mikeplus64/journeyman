module Tourney.Arith where

import Data.Bits
import Data.Tuple.Ordered

fromPow2 :: Int -> [Int]
fromPow2 n = reverse [0 .. 2 ^ n - 1]

stride2 :: Int -> LowHigh Int
stride2 i = LowHigh_ (2 * i) (2 * i + 1)

-- | Round to the nearest power of 2 above the input
nearestPow2Above :: Int -> Int
nearestPow2Above n
  | popCount n == 1 = n
  | otherwise = bit (bitLog2 n + 1)

bitLog2 :: Int -> Int
bitLog2 n = fromIntegral (finiteBitSize n - countLeadingZeros n - 1)
