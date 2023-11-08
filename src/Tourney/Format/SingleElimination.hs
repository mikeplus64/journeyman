module Tourney.Format.SingleElimination where

import Data.Tuple.Ordered
import Tourney.Algebra
import Tourney.Arith

-- | Slaughter seeding over a list of players. That is, favour the best players
-- most, and the worst players least
slaughterOf :: [Int] -> [Match]
slaughterOf ls = zipWith OrdPair_ highs (reverse lows)
  where
    mid = length ls `quot` 2
    (highs, lows) = splitAt mid (sort ls)

singleElimination :: Monad m => Steps m Int
singleElimination = do
  depth <- bitLog2 . nearestPow2Above <$> getPlayerCount
  forM_ [depth, depth - 1 .. 1] \d ->
    step (slaughterOf [0 .. 2 ^ d - 1])
  pure depth
