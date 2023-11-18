{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

-- |
-- = Sparse matrices for holding matches
module Tourney.Match.Matrix (
  MatchMatrix,
  MapByRound (..),
  vectorMapByRound,
  MapByMatches (..),
  createMatchMatrix,
  readMatchMatrix,
  getMatch,
  getMatches,
  getMatchResults,
  addMatch,
  setMatchResult,
  getPendingMatches,
  pendingMatchCount,
  haveAnyPendingMatchesWithin,
) where

import Data.Generics.Labels ()
import Data.IntMap.Strict qualified as IntMap
import Data.Vector qualified as V
import Tourney.Common
import Tourney.Match

-- | An abstraction around a mutable store of rounds of matches and their
-- results. Conceptually it stores a matrix of matches by (round, player1,
-- player2), and a variable for the current round.
data MatchMatrix = MatchMatrix
  { matrixRef :: !(TVar (MapByRound (TVar MatchMatrixRound)))
  , playerCount :: !PlayerCount
  }
  deriving stock (Generic)

data MatchMatrixRound = MatchMatrixRound
  { pending :: !Int
  , complete :: !Int
  , total :: !Int
  , matches :: !(MapByMatches (Maybe Result))
  }
  deriving stock (Generic)

-- Construction
----------------------------------------

createMatchMatrix :: PlayerCount -> STM MatchMatrix
createMatchMatrix playerCount = do
  matrixRef <- newTVar Empty
  pure MatchMatrix{matrixRef, playerCount}

-- Rounds
----------------------------------------

getRound :: MatchMatrix -> RoundNo -> STM (TVar MatchMatrixRound)
getRound m roundNo = do
  matrix <- readTVar (matrixRef m)
  case matrix ^. at roundNo of
    Just t -> pure t
    Nothing -> do
      t <- newTVar MatchMatrixRound{pending = 0, complete = 0, total = 0, matches = mempty}
      modifyTVar' (matrixRef m) (at roundNo ?~ t)
      pure t

readMatchMatrix :: MatchMatrix -> STM (MapByRound (MapByMatches (Maybe Result)))
readMatchMatrix mm = do
  m <- readTVar (matrixRef mm)
  traverse (fmap (view #matches) . readTVar) m

getMatches :: MatchMatrix -> RoundNo -> STM (Vector (Match, Maybe Result))
getMatches mm roundNo = do
  rdRef <- getRound mm roundNo
  rd <- readTVar rdRef
  pure $! V.fromListN (total rd) (itoListOf ifolded (matches rd))

getMatchResults :: MatchMatrix -> RoundNo -> STM (Vector MatchResult)
getMatchResults mm roundNo = do
  rdRef <- getRound mm roundNo
  rd <- readTVar rdRef
  pure $!
    V.fromList
      [ MatchResult m mr
      | (m, Just mr) <- itoList (matches rd)
      ]

getPendingMatches :: MatchMatrix -> RoundNo -> STM (Vector Match)
getPendingMatches mm roundNo = do
  rdRef <- getRound mm roundNo
  rd <- readTVar rdRef
  pure $
    V.fromListN (pending rd) [m | (m, _) <- itoListOf (ifolded . _Nothing) (matches rd)]

pendingMatchCount :: MatchMatrix -> RoundNo -> STM Int
pendingMatchCount mm roundNo = do
  rdRef <- getRound mm roundNo
  rd <- readTVar rdRef
  pure (pending rd)

haveAnyPendingMatchesWithin :: MatchMatrix -> RoundNo -> Focus -> STM Bool
haveAnyPendingMatchesWithin mm roundNo focus = do
  rdRef <- getRound mm roundNo
  rd <- readTVar rdRef
  let r = [m | (m, Nothing) <- itoList (matches rd), m `matchIsWithin` focus]
  pure (not (null r))

-- Matches
----------------------------------------

-- | Add a match. It must not exist already in the current round
addMatch :: MatchMatrix -> RoundNo -> Match -> STM ()
addMatch mm roundNo match =
  -- Ignore matches that are outside of the player count
  --
  -- XXX This enforces a particular interpretation of byes under a points
  -- sorting system. With swaps it's fine, because a bye just maintains the
  -- position of the "real" player, which is the same as winning the match if
  -- they were two real players. But for points, a bye could mean points are
  -- awarded by default anyway; currently, this logic enforces that no points
  -- can be awarded from a bye under points sorting.
  when (match `matchIsWithin` Focus 0 (playerCount mm)) do
    roundRef <- getRound mm roundNo
    roundM <- readTVar roundRef
    when (isNothing (roundM ^. #matches . at match)) do
      modifyTVar' roundRef $ execState do
        #matches . at match ?= Nothing
        #pending += 1
        #total += 1

-- | Get a match
getMatch :: MatchMatrix -> RoundNo -> Match -> STM (Maybe (Maybe Result))
getMatch m roundNo match = do
  roundRef <- getRound m roundNo
  roundM <- readTVar roundRef
  pure (roundM ^. #matches . at match)

-- | Set a match result. It must already exist in the current round, by
-- 'addMatch'
setMatchResult :: MatchMatrix -> RoundNo -> Match -> Result -> STM ()
setMatchResult m roundNo match result = do
  roundRef <- getRound m roundNo
  roundM <- readTVar roundRef
  let haveMatch = isJust (roundM ^. #matches . at match)
  when haveMatch do
    modifyTVar' roundRef $ execState do
      #complete += 1
      #pending -= 1
      #matches . at match . non' _Empty ?= result

--------------------------------------------------------------------------------
-- Internals

-- Maps by matches (Match -> a)
----------------------------------------

-- | A sparse matrix for storing a map between matches and a value.
--
-- Since the two players in 'Match' are guaranteed to be ordered (low, high) we
-- only need to ever check the low->high->match path in the nested intmaps.
newtype MapByMatches a = ByMatches (IntMap (IntMap a))
  deriving newtype (Eq, Show)
  deriving (Functor, Foldable) via (Compose IntMap IntMap)

instance FoldableWithIndex Match MapByMatches where
  ifoldMap f (ByMatches m) =
    ifoldMap (\s0 m' -> ifoldMap (\s1 -> f (Match_ (Slot s0) (Slot s1))) m') m

instance Traversable MapByMatches where
  traverse f (ByMatches m) = ByMatches <$> traverse (traverse f) m

instance FunctorWithIndex Match MapByMatches where
  imap f (ByMatches m) = ByMatches (imap (\s0 -> imap (\s1 -> f (Match_ (Slot s0) (Slot s1)))) m)

instance TraversableWithIndex Match MapByMatches where
  itraverse f (ByMatches m) =
    ByMatches
      <$> itraverse (\s0 m' -> itraverse (\s1 -> f (Match_ (Slot s0) (Slot s1))) m') m

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
  deriving newtype (Eq, Show, Foldable, Functor)

vectorMapByRound :: Vector a -> MapByRound a
vectorMapByRound = ByRound . IntMap.fromList . V.toList . V.indexed

type instance IxValue (MapByRound a) = a
type instance Index (MapByRound a) = RoundNo
instance Ixed (MapByRound a) where ix i = coerced . ix @(IntMap a) (asInt i)
instance At (MapByRound a) where at i = coerced . at @(IntMap a) (asInt i)
instance Semigroup a => Monoid (MapByRound a) where mempty = ByRound mempty
instance Semigroup a => Semigroup (MapByRound a) where
  ByRound a <> ByRound b = ByRound (IntMap.unionWith (<>) a b)

instance FoldableWithIndex RoundNo MapByRound where
  ifoldMap f (ByRound m) = ifoldMap (f . RoundNo) m

instance FunctorWithIndex RoundNo MapByRound where
  imap f (ByRound m) = ByRound (imap (f . RoundNo) m)

instance Traversable MapByRound where
  traverse f (ByRound m) = ByRound <$> traverse f m

instance TraversableWithIndex RoundNo MapByRound where
  itraverse f (ByRound m) = ByRound <$> itraverse (f . RoundNo) m

instance AsEmpty (MapByRound a) where
  _Empty = coerced . _Empty @(IntMap a)

-- see
-- https://hackage.haskell.org/package/lens-5.2.3/docs/src/Control.Lens.Traversal.html#line-1232
-- for 'lens' implementation of these instances that this borrows from

instance TraverseMin RoundNo MapByRound where
  traverseMin f (ByRound m) = case IntMap.minViewWithKey m of
    Just ((k, a), _) -> indexed f (RoundNo k) a <&> \v -> ByRound (IntMap.updateMin (const (Just v)) m)
    Nothing -> pure (ByRound m)
  {-# INLINE traverseMin #-}

instance TraverseMax RoundNo MapByRound where
  traverseMax f (ByRound m) = case IntMap.maxViewWithKey m of
    Just ((k, a), _) -> indexed f (RoundNo k) a <&> \v -> ByRound (IntMap.updateMax (const (Just v)) m)
    Nothing -> pure (ByRound m)
  {-# INLINE traverseMax #-}
