module Tourney.Format.DoubleElimination where

import Control.Monad.Free
import Data.Bits
import Data.Tuple.Ordered
import Tourney.Algebra
import Tourney.Format.SingleElimination
import Tourney.Monad

doubleElimination :: forall s. HasDep 'PlayerCount s => TourneyM s ()
doubleElimination = interleave upper lower
  where
    upper = singleElimination @s

    lower = delay 1 $ step Swaps do
      -- first round: all players from UB
      n <- fmap bitCeiling (dep @'PlayerCount)
      let ubDepth = bitLog2 n :: Int
      let rounds = 2 ^ (ubDepth + 1) :: Int
      let firstRoundLosses = 2 ^ (ubDepth - 1) :: Int
      let firstLink = mapPairs (const []) LowHigh_ [0 .. firstRoundLosses]
      mapM_ match firstLink
      lowerRec

    lowerRec = step Swaps do
      pure ()

-- step m1 do
-- r <- dep @'RoundNumber
-- if even r
--   then pure (interleave2 top b)
--   else fmap TourneyM bot

interleave :: TourneyM s () -> TourneyM s () -> TourneyM s ()
interleave (TourneyStepF m0 top) (TourneyStepF m1 bot) | m0 == m1 =
  TourneyStepF m0 do
    t <- top
    b <- bot
    pure (interleave (TourneyM t) (TourneyM b))
interleave (TourneyStepF m t) _ = TourneyStepF m t
interleave _ (TourneyStepF m t) = TourneyStepF m t
interleave _ _ = pure ()

linkFun :: Int -> [a] -> [a]
linkFun size =
  foldr (.) id (replicate size linkFunSwap)

linkFunSwap :: [a] -> [a]
linkFunSwap l = drop h l ++ take h l
  where
    h = length l `div` 2
