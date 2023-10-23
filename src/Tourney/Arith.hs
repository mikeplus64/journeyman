module Tourney.Arith where

import Data.Bits

fromPow2 :: Int -> [Int]
fromPow2 n = reverse [0 .. 2 ^ n - 1]

stride2 :: Int -> (Int, Int)
stride2 i = (2 * i, 2 * i + 1)

-- | Round to the nearest power of 2 above the input
nearestPow2Above :: Int -> Int
nearestPow2Above n
  | popCount n == 1 = n
  | otherwise = bit (bitLog2 n + 1)

bitLog2 :: Int -> Int
bitLog2 n = fromIntegral (finiteBitSize n - countLeadingZeros n - 1)
