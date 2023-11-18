-- | Swiss-style tournaments
--
-- These tournaments are implemented following the advice found about
-- Swiss-style tournaments on chess.com, available at
-- https://support.chess.com/article/758-what-is-a-swiss-tournament :
--
-- ** What is a Swiss tournament?
--
-- A Swiss tournament is similar to a Round-Robin tournament in that no players
-- are eliminated. Every player will play every round, and the player with the
-- highest number of points at the end of the tournament is the winner.
--
-- In a Swiss tournament, however, you don't necessarily play every other
-- player. If there are a lot of players in the tournament, there may not be
-- enough rounds for everyone to play everyone.
--
-- In the first round only, high rated players are intentionally paired with low
-- rated players. After the first round, pairings are based on performance:
-- those who won are matched with other winners, losers with losers. There may
-- be some exceptions to avoid color repetition.
module Tourney.Format.Swiss where

import Tourney.Algebra
import Tourney.Format.SingleElimination

simpleSwiss :: Int -> Steps () ()
simpleSwiss numRounds = do
  -- First round: Pair up the easiest/hardest matches
  count <- getPlayerCount
  let depth = bitLog2 (nearestPow2Above count)
  swaps (round_ (foldAroundMidpoint (0 ..< 2 ^ depth)))
