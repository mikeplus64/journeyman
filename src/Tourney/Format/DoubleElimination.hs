module Tourney.Format.DoubleElimination where

import Control.Lens
import Data.Bits
import Data.Tuple.Ordered
import Tourney.Algebra
import Tourney.Format.SingleElimination

doubleElimination :: Steps ()
doubleElimination =
  singleElimination `concatMapSteps_` \s -> do
    step s

linkFun :: Int -> [a] -> [a]
linkFun size =
  foldr (.) id (replicate size linkFunSwap)

linkFunSwap :: [a] -> [a]
linkFunSwap l = drop h l ++ take h l
  where
    h = length l `div` 2
