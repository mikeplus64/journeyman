module Tourney.Format.ICantBelieveItCanSort where

import Tourney.Algebra

iCan'tBelieveItCanSort :: Steps () ()
iCan'tBelieveItCanSort = do
  n <- getPlayerCount
  i <- list (0 ..< Slot n)
  j <- list (0 ..< Slot n)
  when (i /= j) do
    round_ $ swaps $ match (Match i j)
