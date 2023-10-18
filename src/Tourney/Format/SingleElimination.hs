module Tourney.Format.SingleElimination where

import Data.Bits
import Data.Tuple.Ordered
import Tourney.Monad
import Tourney.SortingNetwork

singleElimination :: forall s. HasDep 'PlayerCount s => TourneyM s ()
singleElimination = step Swaps do
  -- Initial slaughter seeding
  n <- fmap bitCeiling (dep @'PlayerCount)
  let depth = bitLog2 n
  mapM_ match (slaughterSeeding depth)
  -- Inner matches
  let
    loop :: Int -> TourneyM s ()
    loop d | d > 0 = step Swaps do
      mapM_ match (mapPairs (const []) LowHigh_ [0 .. 2 ^ d])
      pure (loop (d - 1))
    loop _ = pure ()
  pure (loop (depth - 1))

slaughterSeeding :: Int -> [LowHigh Int]
slaughterSeeding n | n <= 0 = []
slaughterSeeding 1 = [LowHigh_ 0 1]
slaughterSeeding d = [LowHigh_ p (2 ^ d - p - 1) | hl <- slaughterSeeding (d - 1), p <- toList hl]

bitCeiling :: Int -> Int
bitCeiling n =
  if popCount n == 1
    then n
    else bit (bitLog2 n + 1)

bitLog2 :: Int -> Int
bitLog2 n = finiteBitSize n - countLeadingZeros n - 1

alternatingConcat :: [a] -> [a] -> [a]
alternatingConcat xs ys = concat (zipWith (\a b -> [a, b]) xs ys)

oddsAndEvens :: [a] -> ([a], [a])
oddsAndEvens (x : y : zs) = (x : xs, y : ys) where !(xs, ys) = oddsAndEvens zs
oddsAndEvens (x : _) = ([x], [])
oddsAndEvens _ = ([], [])

mapPairs :: (a -> [b]) -> (a -> a -> b) -> [a] -> [b]
mapPairs zf f (x : y : zs) = f x y : mapPairs zf f zs
mapPairs zf _ (x : _) = zf x
mapPairs _ _ _ = []
