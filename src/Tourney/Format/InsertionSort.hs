module Tourney.Format.InsertionSort where

import Tourney.Algebra

insertionSortNaiive :: Steps () ()
insertionSortNaiive = do
  n <- getPlayerCount
  i <- list (0 ..< Slot n)
  j <- list (i ..> 0)
  swaps (round_ (match (Match j (j - 1))))

insertionSort :: Steps () ()
insertionSort = do
  n <- Slot <$> getPlayerCount
  i <- list ([0 .. n - 2] ++ reverse [0 .. n - 3])
  swaps . round_ . asRound $ do
    let (m, r) = i `divMod` 2
    j <- list [0 .. m]
    match (Match (j * 2 + r) (j * 2 + 1 + r))
