module Tourney.Format.SingleElimination where

import Data.Tuple.Ordered
import Tourney.Algebra
import Tourney.Arith

-- | Slaughter seeding over a list of players. That is, favour the best players
-- most, and the worst players least
slaughterOf :: [Player] -> [Match]
slaughterOf ls = collapseMatches (length ls `quot` 2) ls

-- zipWith OrdPair_ highs (reverse lows)
-- where
--   mid = length ls `quot` 2
--   (highs, lows) = splitAt mid (sort ls)

singleElimination :: Monad m => Steps m Int
singleElimination = do
  depth <- bitLog2 . nearestPow2Above <$> getPlayerCount
  forM_ [depth, depth - 1 .. 1] \d ->
    step (collapseMatches (2 ^ (d - 1)) [0 .. 2 ^ d - 1])
  pure depth

-- | Fold a list of players together around a midpoint of the list.
collapseMatches :: Int -> [Player] -> [Match]
collapseMatches midpoint players =
  take midpoint (zipWith OrdPair_ players (reverse players))
