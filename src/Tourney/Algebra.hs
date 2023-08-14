module Tourney.Algebra where

import Control.Lens
import Control.Monad.Primitive (PrimMonad)
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM

import Data.Tuple.Ordered
import Tourney.Result

type Player = Int

-- type MatchID = (RoundNo, Int)

-- -- | A tournament network is a list of rounds in which each round is a set of matches.
-- --
-- -- The matches are represented by counting the pairs of combinations of matches of P players
-- -- as [ (a,b) | a <- [1..n], b <- [a+1..n] ] whose length is a triangular number.
-- -- This is so that it's impossible to feature the same player twice within a round.
-- type Network = [IntSet]

-- data Ranker s = Round (Set (Swap s))

data Ranker = Ranker
  { rounds :: [Set (LowHigh Int)]
  , type_ :: MatchType
  }

data MatchType
  = -- | The winner takes the highest rank
    CompareAndSwap
  | -- | Distribute points to N different point bins
    Points
  deriving stock (Show, Read, Eq, Ord, Generic)

runRankerM ::
  (PrimMonad m) =>
  (a -> a -> m (MatchResult a)) ->
  Ranker ->
  Vector a ->
  m [Vector (Integer, a)]
runRankerM runMatch Ranker {rounds, type_} initialRanking = do
  ranking <- V.thaw initialRanking
  points <- V.thaw (V.replicate (V.length initialRanking) 0)
  forM rounds \cmps -> do
    forM_ cmps \(LowHigh_ ilow ihigh) -> do
      a <- VM.read ranking ilow
      b <- VM.read ranking ihigh
      MatchResult {winner, loser} <- runMatch a b
      case type_ of
        Points -> do
          VM.modify points (\p -> p + winner ^. #points) (winner ^. #index)
          VM.modify points (\p -> p + loser ^. #points) (loser ^. #index)
          ranking' <- V.freeze ranking
          points' <- V.freeze points
          let !next =
                V.fromListN
                  (V.length ranking')
                  ( sortOn
                      fst
                      ( zip
                          (V.toList points')
                          (V.toList ranking')
                      )
                  )
          VM.copy points =<< V.thaw (V.map fst next)
          VM.copy ranking =<< V.thaw (V.map snd next)
        CompareAndSwap -> do
          VM.write ranking ihigh (winner ^. #player)
          VM.write ranking ilow (loser ^. #player)
    V.zip <$> V.freeze points <*> V.freeze ranking

--------------------------------------------------------------------------------

data NetworkStep = NetworkStep
  { matches :: Set (LowHigh Word32)
  , type_ :: MatchType
  }

newtype Ranking a = Ranking {unRanking :: Word32 -> ([NetworkStep], a)}

--------------------------------------------------------------------------------

slaughterSeeding :: Word32 -> [LowHigh Word32]
slaughterSeeding d | d <= 1 = [LowHigh_ 0 1]
slaughterSeeding d = [LowHigh_ p (2 ^ d - p - 1) | hl <- slaughterSeeding (d - 1), p <- toList hl]

slaughterSeedingFlat :: Word32 -> [Word32]
slaughterSeedingFlat d | d <= 1 = [0, 1]
slaughterSeedingFlat d =
  alternate d1 (map (\p -> 2 ^ d - p - 1) d1)
  where
    d1 = slaughterSeedingFlat (d - 1)
    alternate xs ys = concat (zipWith (\a b -> [a, b]) xs ys)

data Bin = L | R

slaughterSeedingInf :: [[LowHigh Natural]]
slaughterSeedingInf = map pairs slaughterSeedingFlatInf
  where
    pairs :: [Natural] -> [LowHigh Natural]
    pairs (a : b : xs) = LowHigh_ a b : pairs xs
    pairs _ = []

lfswap :: [a] -> [a]
lfswap l = drop h l ++ take h l
  where
    h = length l `div` 2

lfrev :: [a] -> [a]
lfrev l = reverse (take h l) ++ reverse (drop h l)
  where
    h = length l `div` 2

qq :: [a] -> [a]
qq (x : xs) = xs ++ [x]
qq [] = []

slaughterSeedingFlatInf :: [[Natural]]
slaughterSeedingFlatInf =
  [0] : do
    (n, ps) <- zip pow2s slaughterSeedingFlatInf
    pure do
      p <- ps
      [p, n - p - 1]

pow2s :: [Natural]
pow2s = (2 ^) <$> [1 :: Natural ..]

ss x = ((sin (0.5 * pi * fromInteger x) + 1) / 2)

slaughterRec :: Word32 -> Word32 -> Word32
slaughterRec 0 0 = 0
slaughterRec 1 0 = 0
slaughterRec 1 1 = 1
slaughterRec d _ | d <= 1 = error "no"
slaughterRec d i = case i `quotRem` 2 of
  (i0, 0) -> slaughterRec (d - 1) i0
  (i0, 1) -> 2 ^ d - 1 - slaughterRec (d - 1) i0
  _ -> error "impossible"

slaughterRecNoBranch :: Word32 -> Word32 -> Bool -> Word32
slaughterRecNoBranch 0 0 _ = 0
slaughterRecNoBranch 1 0 False = 0
slaughterRecNoBranch 1 0 True = 1
slaughterRecNoBranch d _ _ | d <= 1 = error "no"
slaughterRecNoBranch d i False = slaughterRecNoBranch (d - 1) i1 (m == 1) where (i1, m) = quotRem i 2
slaughterRecNoBranch d i True = 2 ^ d - 1 - slaughterRecNoBranch (d - 1) i1 (m == 1) where (i1, m) = quotRem i 2

--------------------------------------------------------------------------------

-- | Creates a round-robin with n participants.
roundRobin :: Int -> [LowHigh Int]
roundRobin n = [LowHigh_ a b | a <- [0 .. n - 1], b <- [a + 1 .. n - 1]]

isqrt, tri :: Int -> Int
isqrt = floor . sqrt . (fromIntegral :: Int -> Double)
tri n = n * (n - 1) `div` 2
