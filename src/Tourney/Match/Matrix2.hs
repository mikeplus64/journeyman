module Tourney.Match.Matrix2 where

import Data.Generics.Labels ()
import Data.IntMap.Strict qualified as IntMap
import Tourney.Common
import Tourney.Match

--------------------------------------------------------------------------------
-- Sparse matrices for holding matches
--
-- Since the two players in 'Match' are guaranteed to be ordered (low, high) we
-- only need to ever check the low->high->match path in the nested intmaps.
--

-- Maps by matches (Match -> a)
----------------------------------------

newtype MapByMatches a = ByMatches (IntMap (IntMap a))
  deriving newtype (Eq, Show)

type instance IxValue (MapByMatches a) = a
type instance Index (MapByMatches a) = Match

instance Ixed (MapByMatches a) where
  {-# INLINE ix #-}
  ix (Match a b) = coerced . ixIntMaps
    where
      {-# INLINE ixIntMaps #-}
      ixIntMaps :: Traversal' (IntMap (IntMap a)) a
      ixIntMaps = ix (asInt a) . ix (asInt b)

instance At (MapByMatches a) where
  {-# INLINE at #-}
  at (Match a b) = coerced . atIntMaps
    where
      {-# INLINE atIntMaps #-}
      atIntMaps :: Lens' (IntMap (IntMap a)) (Maybe a)
      atIntMaps = at (asInt a) . non' _Empty . at (asInt b)

instance Monoid (MapByMatches a) where mempty = ByMatches mempty
instance Semigroup (MapByMatches a) where
  ByMatches a <> ByMatches b = ByMatches (IntMap.unionWith IntMap.union a b)

instance AsEmpty (MapByMatches a) where
  _Empty = coerced . _Empty @(IntMap (IntMap a))

-- Maps by round (RoundNo -> a)
----------------------------------------

newtype MapByRound a = ByRound (IntMap a)
  deriving newtype (Eq, Show)

type instance IxValue (MapByRound a) = a
type instance Index (MapByRound a) = RoundNo
instance Ixed (MapByRound a) where ix i = coerced . ix @(IntMap a) (asInt i)
instance At (MapByRound a) where at i = coerced . at @(IntMap a) (asInt i)
instance Semigroup a => Monoid (MapByRound a) where mempty = ByRound mempty
instance Semigroup a => Semigroup (MapByRound a) where
  ByRound a <> ByRound b = ByRound (IntMap.unionWith (<>) a b)

instance AsEmpty (MapByRound a) where
  _Empty = coerced . _Empty @(IntMap a)

--------------------------------------------------------------------------------

data MatchMatrix = MatchMatrix
  { matrixRef :: !(TVar (MapByRound (MapByMatches (TVar Matchup))))
  , roundNoRef :: !(TVar RoundNo)
  }
  deriving stock (Generic)

data Matchup = Matchup
  { result :: !(Maybe Result)
  }
  deriving stock (Generic)

getMatch :: MatchMatrix -> Match -> STM (TVar Matchup)
getMatch MatchMatrix{matrixRef, roundNoRef} match = do
  matrix <- readTVar matrixRef
  roundNo <- readTVar roundNoRef
  case matrix ^? ix roundNo . ix match of
    Just r -> pure r
    Nothing -> do
      r <- newTVar (Matchup Nothing)
      modifyTVar' matrixRef (at roundNo . non' _Empty . at match ?~ r)
      pure r

setMatchResult :: MatchMatrix -> Match -> Result -> STM ()
setMatchResult mm match result = do
  r <- getMatch mm match
  modifyTVar' r (#result ?~ result)

-- | Does what it says on the tin :-)
advanceMatchMatrixOneRound :: MatchMatrix -> STM ()
advanceMatchMatrixOneRound MatchMatrix{roundNoRef} = modifyTVar' roundNoRef (+ 1)
