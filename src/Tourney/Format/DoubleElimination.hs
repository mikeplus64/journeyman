{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Tourney.Format.DoubleElimination where

import Control.Lens
import Data.Tuple.Ordered
import Tourney.Algebra
import Tourney.Format.SingleElimination

--------------------------------------------------------------------------------
-- A direct implementation of Double Elimination tournaments.
--
-- The approach here is to:
-- 1. Construct a single elimination upper bracket
-- 2. Create an initial lower bracket round, from the lowers of the first round of
--    the single elimination bracket
-- 3. For each other upper bracket round:
--    3.1. Create a lower bracket round that accepts the losers from that round
--    3.2. Create a lower bracket round that plays off only LB players against
--         eachother.
--
-- Since we only depend on the rounds generated in the upper bracket, we can
-- parameterise that; so our "doubleElimination" function is generalisable to be
-- able to add an extra loser's bracket to _any_ tournament, although it is
-- likely to have strange results when given tournaments that are not in the
-- same shape as single-elimination.
--
-- Thus we can use this to create quadruple elimination brackets, but not
-- triple.

doubleElimination :: MonadFail m => Steps m ()
doubleElimination = addLosersBracket singleElimination

quadrupleElimination :: MonadFail m => Steps m ()
quadrupleElimination = addLosersBracket quadrupleElimination

addLosersBracket :: MonadFail m => Steps m a -> Steps m ()
addLosersBracket original = do
  (_depth, ub1 :< ub2 :< ubs) <- rehearsePure original
  let lowerRound1 = slaughterOf (ub1 ^.. each . larger)
  step ub1
  step ub2 ||| step lowerRound1
  statefully_ (lowerRound1 ^.. each . smaller) $ iforM_ ubs \i upper -> do
    lastWinners <- get
    let shuffledLosers = linkFun i (upper ^.. each . larger)
    -- Accept new losing players from the upper bracket
    let acceptRound = zipWith OrdPair_ lastWinners shuffledLosers
    -- Then perform a round of just lower bracket players being eliminated
    -- I.e., match up the winners of the step we just wrote
    let losersRound = slaughterOf (acceptRound ^.. each . smaller)
    -- Finally, add these steps, and store the winning players in losersRound
    -- for the next iteration
    step upper ||| step acceptRound
    step losersRound
    put (losersRound ^.. each . smaller)

linkFun :: Int -> [a] -> [a]
linkFun size = foldr (.) id (replicate size linkFunSwap)

linkFunSwap :: [a] -> [a]
linkFunSwap l = drop h l ++ take h l
  where
    h = length l `div` 2

--------------------------------------------------------------------------------
-- Alternatively
