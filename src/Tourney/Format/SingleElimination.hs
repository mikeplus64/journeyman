module Tourney.Format.SingleElimination where

import Tourney.Algebra

singleElimination :: Steps () ()
singleElimination = do
  count <- getPlayerCount
  let depth = bitLog2 (nearestPow2Above count)
  d <- list (depth ..> 0)
  swaps (round_ (foldAroundMidpoint (0 ..< 2 ^ d)))
