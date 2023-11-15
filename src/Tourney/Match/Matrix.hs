{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -funbox-strict-fields -Wno-partial-type-signatures #-}

-- |
-- The goal here is to come up with a compact data structure for representing
-- the match results in a round. There are always at most n-1 matches per round
-- in a tournament, since you cannot match anyone twice in the same round. This
-- can be verified by looking at a round-robin schedule.
--
-- We also know that _all_ possible matches are represented by a round-robin. So
-- any round of any tournament must be a subset of some round of a round-robin.
--
-- Note a round robin represents a pairing of every player to every other player
-- only once. It is different from cartesian product because a match between
-- (a,b) and (b,a) for a != b are considered the same match so only included
-- once in a round-robin.
--
-- The strategy I come up with is ultimately using a flat array for mapping
-- players to players, where (-1) is used to denote if a player did not have a
-- match.
module Tourney.Match.Matrix (
  RoundMatchMatrix,
  createRoundMatchMatrix,
  clearMatchMatrix,
  addMatch,
  setMatchResult,
  readMatchResult,
  readMatchups,
  readPlayerResult,
  pendingMatchCount,
  pendingMatchesWithinCount,
  awaitZeroPendingMatches,
) where

import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TArray
import Data.Array.MArray
import Data.Generics.Labels ()
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Tourney.Common
import Tourney.Match

--------------------------------------------------------------------------------

data Matchup = Matchup
  { match :: {-# UNPACK #-} !Match
  , result :: {-# UNPACK #-} !(TVar (Maybe Result))
  }
  deriving stock (Eq, Generic)

data RoundMatchMatrix = RMM
  { matchups :: !(TArray Player (Maybe Matchup))
  , matchCount :: !(TVar Int)
  , pendingCount :: !(TVar Int)
  -- ^ A 'TVar' that can be listened to for changes
  }
  deriving stock (Generic)

createRoundMatchMatrix :: PlayerCount -> STM RoundMatchMatrix
createRoundMatchMatrix count = do
  matchups <- newArray (0, count - 1) Nothing
  matchCount <- newTVar 0
  pendingCount <- newTVar 0
  pure RMM{matchups, matchCount, pendingCount}

-- | Clear the match matrix, returning the old state
clearMatchMatrix :: RoundMatchMatrix -> Focus -> STM ()
clearMatchMatrix RMM{matchups, pendingCount, matchCount} focus = do
  forM_ (focusStart focus ..< focusEnd focus) \i -> do
    cur <- readArray matchups i
    forM_ cur \Matchup{match = Match p1 p2} -> do
      writeArray matchups p1 Nothing
      writeArray matchups p2 Nothing
      modifyTVar' pendingCount (subtract 1)
      modifyTVar' matchCount (subtract 1)

-- | Register a match in this 'MRoundMatchMatrix'.
addMatch :: RoundMatchMatrix -> Match -> STM (TVar (Maybe Result))
addMatch RMM{matchups, pendingCount, matchCount} match@(Match p1 p2) = do
  curMatch1 <- readArray matchups p1
  curMatch2 <- readArray matchups p2
  when (isJust curMatch1 || isJust curMatch2) (error "addMatch: match is already written to")
  result <- newTVar Nothing
  let !matchup = Just Matchup{match, result}
  writeArray matchups p1 matchup
  writeArray matchups p2 matchup
  modifyTVar' matchCount (+ 1)
  modifyTVar' pendingCount (+ 1)
  pure result

-- | Set the result for a match. The match must not already be written to, and
-- have been registered already with 'addMatch'
setMatchResult :: RoundMatchMatrix -> Match -> Result -> STM ()
setMatchResult RMM{matchups, pendingCount} (Match p1 p2) result = do
  curMatch1 <- readArray matchups p1
  curMatch2 <- readArray matchups p2
  when (isNothing curMatch1) (error "setMatchResult: no such match")
  when (curMatch1 /= curMatch2) (error "setMatchResult: mismatched matchups")
  let resultVar = curMatch1 ^?! _Just . #result
  curResult <- readTVar resultVar
  when (isJust curResult) (error "setMatchResult: already have a result for this match")
  writeTVar resultVar (Just result)
  modifyTVar' pendingCount (subtract 1)

-- | Read the match that a player is in, if any
readPlayerResult :: RoundMatchMatrix -> Player -> STM (Maybe (Match, Maybe Result))
readPlayerResult RMM{matchups} me = runMaybeT do
  (_, len) <- lift (getBounds matchups)
  guard (not (0 <= me && me < len))
  Just Matchup{match, result} <- lift (readArray matchups me)
  r <- lift (readTVar result)
  pure (match, r)

-- | Read the match that a player is in, if any
readMatchResult :: RoundMatchMatrix -> Match -> STM (Maybe Result)
readMatchResult RMM{matchups} m@(Match p1 _p2) = runMaybeT do
  Just Matchup{match, result} <- lift (readArray matchups p1)
  guard (m == match)
  MaybeT (readTVar result)

data ReadMatchesState = RMS
  {visited :: !IntSet, i :: !Int}
  deriving stock (Generic)

readMatchups :: RoundMatchMatrix -> STM (Vector Matchup)
readMatchups RMM{matchups} = do
  ms :: Array Player (Maybe Matchup) <- freeze matchups
  !vec <- pure $ V.create do
    let state0 = RMS{visited = mempty, i = 0}
    buffer <- VM.new (snd (arrayBounds matchups))
    evaluatingStateT state0 do
      forMOf_ (each . _Just) ms \m@Matchup{match = Match p1 p2} -> do
        visited <- use #visited
        unless (visited ^. contains p1 || visited ^. contains p2) do
          #visited . contains p1 .= True
          #visited . contains p2 .= True
          i <- #i <<+= 1
          VM.write buffer i m
    pure buffer
  pure vec

-- | Get the current pending match count. Using STM you can listen to this to
-- easily block the current thread until 0 pending matches is reached. See
-- 'awaitNoPendingMatches'
pendingMatchCount :: RoundMatchMatrix -> STM Int
pendingMatchCount RMM{pendingCount} = readTVar pendingCount

-- | Get the current pending match count. Using STM you can listen to this to
-- easily block the current thread until 0 pending matches is reached. See
-- 'awaitNoPendingMatches'
pendingMatchesWithinCount :: RoundMatchMatrix -> Focus -> STM Int
pendingMatchesWithinCount rmm focus = do
  pendings <- readPendingMatches rmm
  pure $! length [p | p <- pendings, validateMatch focus p]

awaitZeroPendingMatches :: RoundMatchMatrix -> STM ()
awaitZeroPendingMatches RMM{pendingCount} = do
  count <- readTVar pendingCount
  unless (count == 0) retry
