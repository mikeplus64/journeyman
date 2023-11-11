{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Tourney.Format.DoubleElimination where

import Control.Lens
import Tourney.Algebra

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

addLosersBracket :: Steps () () -> Steps () ()
addLosersBracket original = do
  ub1 :< ubs <- inspect (ByRound Flat) original
  let lowerRound1 = foldAroundMidpoint (ub1 ^.. each . likelyLoser)
  round_ ub1
  round_ lowerRound1
  evaluatingStateT (lowerRound1 ^.. each . likelyWinner) $ iforM_ ubs \i upper -> do
    lastWinners <- get
    let shuffledLosers = linkFun i (upper ^.. each . likelyLoser)
    -- Accept new losing players from the upper bracket
    let acceptRound = zipWith Match lastWinners shuffledLosers
    -- Then perform a round of just lower bracket players being eliminated
    -- I.e., match up the winners of the round_ we just wrote
    let losersRound = foldAroundMidpoint (acceptRound ^.. each . likelyWinner)
    -- Finally, add these rounds, and store the winning players in losersRound
    -- for the next iteration
    round_ do
      toRound upper
      toRound acceptRound
    round_ losersRound
    put (losersRound ^.. each . likelyWinner)

linkFun :: Int -> [a] -> [a]
linkFun size = foldr (.) id (replicate size linkFunSwap)

linkFunSwap :: [a] -> [a]
linkFunSwap l = drop h l ++ take h l
  where
    h = length l `div` 2

--------------------------------------------------------------------------------
-- Alternatively
