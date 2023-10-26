module Tourney.Format.DoubleElimination where

import Control.Lens
import Data.Bits
import Data.Maybe
import Data.Tuple.Ordered
import Tourney.Algebra
import Tourney.Arith
import Tourney.Format.SingleElimination

doubleElimination :: Monad m => Steps m ()
doubleElimination = do
  void singleElimination `concatMapSteps_` \round ubStep -> do
    rounds <- ask
    upper <- compilePure ubStep
    let upperLosers = map (view _2) upper
    let splitPoint = fromJust (maximumOf each upperLosers)
    traceM $ "round: " ++ show round
    traceM $ "splitPoint: " ++ show splitPoint
    step upper
      `interleave2_` if
          | round == 0 -> mempty
          | round == 1 -> step (slaughterOf upperLosers)
          | even round -> mempty
          | otherwise -> mempty

-- forM_ [0 .. depth - 1] \round -> do
--   let d = depth - round
--   let upper = slaughter d
--   let losers = map (view low) upper
--   let lower
--         | round == 0 = mempty
--         | round == 1 = slaughterOf losers
--         -- On even LB rounds, create matches with the players in the lower
--         -- bracket
--         | even round = evenRound
--         -- On odd LB rounds, create matches by linking in players from the
--         -- upper bracket
--         | otherwise = losers
--         where
--           evenRound m = map (offset m . stride2) [0 .. m - 1]
--           oddRound = zipWith LowHigh_ (map (view high) evenRound) (linkFun)

--   step (upper <> lower)

linkFun :: Int -> [a] -> [a]
linkFun size =
  foldr (.) id (replicate size linkFunSwap)

linkFunSwap :: [a] -> [a]
linkFunSwap l = drop h l ++ take h l
  where
    h = length l `div` 2
