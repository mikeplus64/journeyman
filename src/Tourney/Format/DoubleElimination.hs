module Tourney.Format.DoubleElimination where

import Control.Monad.Free
import Data.Bits
import Data.Tuple.Ordered
import Tourney.Format.SingleElimination
import Tourney.Monad
import Tourney.SortingNetwork

doubleElimination :: forall s. HasDep 'PlayerCount s => TourneyM s ()
doubleElimination =
  singleElimination `interleave` do
    -- delay the first round of lower bracket by 1 step
    delay 1 $ step Swaps do
      -- first round: all players from UB
      n <- fmap bitCeiling (dep @'PlayerCount)
      let ubDepth = bitLog2 n :: Int
      let rounds = 2 ^ (ubDepth + 1) :: Int
      let firstRoundLosses = 2 ^ (ubDepth - 1) :: Int
      let firstLink = mapPairs (const []) LowHigh_ [0 .. firstRoundLosses]
      mapM_ match firstLink
      lowerBracketRounds ubDepth
  where
    lowerBracketRounds = do
      _

-- step m1 do
-- r <- dep @'RoundNumber
-- if even r
--   then pure (interleave2 top b)
--   else fmap TourneyM bot

linkFun :: Int -> [a] -> [a]
linkFun size =
  foldr (.) id (replicate size linkFunSwap)

linkFunSwap :: [a] -> [a]
linkFunSwap l = drop h l ++ take h l
  where
    h = length l `div` 2
