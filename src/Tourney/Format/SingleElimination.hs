module Tourney.Format.SingleElimination where

import Control.Lens
import Data.Bits
import Data.List ((!!))
import Data.Tuple.Ordered
import Tourney.Algebra
import Tourney.Arith
import Tourney.Match

slaughterSeeding :: [[Match]]
slaughterSeeding = [LowHigh_ 0 1] : imap next slaughterSeeding
  where
    next d a = [LowHigh_ p (2 ^ (d + 2) - p - 1) | p <- toList =<< a]

singleElimination :: Steps ()
singleElimination = do
  players <- getPlayerCount
  let depth = bitLog2 (nearestPow2Above players)
  step (slaughterSeeding !! depth)
  sequence_ do
    round <- [1 .. depth]
    pure (step (stride2 <$> fromPow2 round))
