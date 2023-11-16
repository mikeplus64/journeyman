module Tourney.Format.RoundRobin where

import Tourney.Algebra

--------------------------------------------------------------------------------

-- Version 2: I begin implementing this using functions from indices to values,
-- rather than lists, since I suspected we'll be able to express the rotate
-- function much more simply, and without any infinite lists, while perhaps
-- discovering a "simpler" form for the round robin circle method algorithm:
--
-- 0 -> [0 .. n]
-- 1 -> 0 : n : [1 .. n - 1]
-- 2 -> 0 : n : n - 1 : [2 .. n - 2]
--
-- ==>
--      i -> 0 : [n .. n - i + 1] ++ [i .. n - i]
--
-- In the end, what I find is that rather than needing the tournament eDSL very
-- much at all, a "functional" approach to creating this ends up being much
-- superior, in terms of clarity and conciseness, to existing suggested
-- solutions.
--
-- That said, now that we have a 'roundRobin' definition, using it to create
-- group stages (using eDSL primitives) becomes trivial.
--

-- XXX Set the sorting method!

roundRobin :: PlayerCount -> [[Match]]
roundRobin count =
  [ foldAround midpoint (map Slot (0 : ((n - i) ..< n) ++ (1 ..< (n - i))))
  | i <- [0 .. n - 2]
  ]
  where
    (!midpoint, !r) = count `quotRem` 2
    !n = count + r

-- | Divide a tournament into 'n' groups, and perform a round-robin within each
-- group.
groupRoundRobin :: Int -> Steps () ()
groupRoundRobin numGroups =
  divideInto numGroups (steps roundRobin)

{-
For comparison, the original kuachi.gg code in Rust:

pub fn group_schedule(signups: usize, rematches: usize) -> Vec<Vec<(usize, usize, usize)>> {
    let mut rounds = Vec::new();
    let n = signups + signups % 2;
    let mut map = (0..n).collect::<Vec<_>>();
    let mid = n / 2;
    for _i in 0..n - 1 {
        let mut round: Vec<(usize, usize, usize)> = Vec::new();
        let up = (&map[..mid]).to_vec();
        let down = {
            let mut v = (&map[mid..]).to_vec();
            v.reverse();
            v
        };
        for j in 0..mid {
            let s1 = up[j];
            let s2 = down[j];
            for r in 0..=rematches {
                round.push((s1, s2, r));
            }
        }
        rounds.push(round);
        // map = map[mid:-1] + map[:mid] + map[-1:]
        map = [&map[mid..n - 1], &map[..mid], &[map[n - 1]]].concat();
    }
    rounds
}
-}

--------------------------------------------------------------------------------

-- | For another comparison, I take an existing implementation available here
-- https://hackage.haskell.org/package/Tournament-0.0.1/docs/src/Game-Tournament.html#robin
-- as an example and adapt it to the "journeyman" style. It is licensed MIT, and
-- its license is reproduced below:
--
-- (The MIT License)
--
-- Copyright (c) 2012 Eirik Albrigtsen
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- 'Software'), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
roundRobinEirikAlbrigtsen :: Int -> [[Match]]
roundRobinEirikAlbrigtsen count = map (foldAround midpoint . map Slot . toList) r
  where
    (!midpoint, !oddness) = count `quotRem` 2
    !end = count + oddness - 1
    r = take end (iterate rotate (0 :| [1 .. end]))
    rotate :: NonEmpty a -> NonEmpty a -- "Rotate" a list, keeping one element in place
    rotate (x :| (nonEmpty -> Just xs)) = x :| last xs : init xs
    rotate xs = xs
