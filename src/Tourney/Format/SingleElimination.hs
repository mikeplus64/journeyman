module Tourney.Format.SingleElimination where

import Control.Lens
import Data.List ((!!))
import Data.Tuple.Ordered
import Tourney.Algebra
import Tourney.Arith

slaughter :: Int -> [Match]
slaughter r = slaughterOf [0 .. 2 ^ r - 1]

slaughterOf :: [Int] -> [Match]
slaughterOf ls = zipWith LowHigh_ highs (reverse lows)
  where
    mid = length ls `quot` 2
    (highs, lows) = splitAt mid (sort ls)

singleElimination :: Monad m => Steps m Int
singleElimination = do
  count <- getPlayerCount
  let depth = bitLog2 (nearestPow2Above count)
  mapM_ (step . slaughter) [depth, depth - 1 .. 1]
  pure depth
