{-# OPTIONS_GHC -funbox-strict-fields #-}

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
  MRoundMatchMatrix,
  IORoundMatchMatrix,
  createRoundMatchMatrix,
  readMatches,
  pendingMatchCount,
  readMatchResult,
  addMatch,
  setMatchResult,
  freezeMatches,
) where

import Control.Lens
import Control.Monad.Primitive (MonadPrim)
import Control.Monad.ST.Strict
import Data.Generics.Labels ()
import Data.Primitive.PrimVar
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Tourney.Algebra.Unified (SortMethod (..), Sorter (..))
import Tourney.Common
import Tourney.Match

data RoundMatchMatrix = RMM
  { matchups :: !(U.Vector Player)
  , results :: !(V.Vector (Maybe Result))
  }

getMatchResult :: RoundMatchMatrix -> Match -> Maybe Result
getMatchResult RMM{results} (Match p1 _p2) = results ^? ix p1 . _Just

-- NOTE see below for the construction of 'RMM'; it should be correct by
-- construction, so we don't need to check p1 is not p2, or check that the p2
-- here is the same p2 as in the 'matchups' vector

--------------------------------------------------------------------------------
-- Mutable interface

type IORoundMatchMatrix = MRoundMatchMatrix RealWorld

data MRoundMatchMatrix s = MutRMM
  { matchups :: !(UM.MVector s (Player, Int))
  , sorters :: !(VM.MVector s Sorter)
  , results :: !(VM.MVector s (Maybe Result))
  , matchupCount :: !(PrimVar s Int)
  , resultsCount :: !(PrimVar s Int)
  }

readMatches :: MonadPrim s m => MRoundMatchMatrix s -> m (V.Vector (Match, Maybe Result))
readMatches MutRMM{matchups = matchups_, results = results_} = do
  matchups <- U.unsafeFreeze matchups_
  results <- V.unsafeFreeze results_
  -- these unsafeFreezes are fine because we're not doing any writing. Of
  -- course, this is not thread safe, but it wouldn't be if it were mutable
  -- anyway.
  let !count = U.length matchups
  accum <- VM.new count
  evaluatingStateT MS{visitedPlayers = mempty, count = 0} $ do
    U.iforM_ matchups \player1 player2 -> do
      visited <- use (#visitedPlayers . contains player2) -- Note that player2 >= player1 always
      when (not visited && player1 >= 0 && player2 >= 0 && player1 /= player2) do
        #visitedPlayers . contains player1 .= True
        #visitedPlayers . contains player2 .= True
        let match = Match player1 player2
        let result = results ^? ix player1 . _Just
        i <- #count <<+= 1
        VM.write accum i (match, result)
  V.unsafeFreeze (VM.take count accum)

-- internal state
data MS = MS
  { visitedPlayers :: !IntSet
  , count :: !Int
  }
  deriving stock (Generic)

createRoundMatchMatrix :: MonadPrim s m => PlayerCount -> m (MRoundMatchMatrix s)
createRoundMatchMatrix count = do
  matchups <- UM.replicate count (-1)
  matchupCount <- newPrimVar 0
  results <- VM.replicate count Nothing
  resultsCount <- newPrimVar 0
  pure MutRMM{matchups, matchupCount, results, resultsCount}

-- | Get the number of matches that are pending a result _within the
-- 'MRoundMatchMatrix'_. Only matches that are registered first with 'addMatch'
-- are counted.
pendingMatchCount :: MonadPrim s m => MRoundMatchMatrix s -> m Int
pendingMatchCount MutRMM{matchupCount, resultsCount} = do
  m <- readPrimVar matchupCount
  r <- readPrimVar resultsCount
  pure (max 0 (m - r))

-- | Read the match that a player is in, if any
readMatchResult :: MonadPrim s m => MRoundMatchMatrix s -> Player -> m (Maybe (Match, Maybe Result))
readMatchResult MutRMM{matchups, results} me = runMaybeT do
  Just other <- UM.readMaybe matchups me
  let p1 = min me other
  let p2 = max me other
  guard (p1 >= 0 && p2 >= 0 && p1 /= p2)
  r <- VM.readMaybe results p1
  pure (Match p1 p2, join r)

-- | Register a match in this 'MRoundMatchMatrix'.
addMatch :: MonadPrim s m => MRoundMatchMatrix s -> Match -> m ()
addMatch MutRMM{matchups, matchupCount, results} (Match p1 p2) = do
  curP2 <- UM.read matchups p1
  when (curP2 >= 0) (error "addMatch: match is already written to")
  modifyPrimVar matchupCount (+ 1)
  UM.write matchups p1 p2
  VM.write results p1 Nothing

-- | Set the result for a match. The match must not already be written to, and
-- have been registered already with 'addMatch'
setMatchResult :: MonadPrim s m => MRoundMatchMatrix s -> Match -> Result -> m ()
setMatchResult MutRMM{matchups, results, resultsCount} (Match p1 p2) result = do
  curP2 <- UM.read matchups p1
  when (curP2 /= p2) (error "setMatchResult: mismatched matchups!")
  curResult <- VM.read results p1
  when (isJust curResult) (error "setMatchResult: already have a result for this match")
  modifyPrimVar resultsCount (+ 1)
  VM.write results p1 (Just result)

freezeMatches :: MonadPrim s m => MRoundMatchMatrix s -> m RoundMatchMatrix
freezeMatches MutRMM{matchups, results} = RMM <$> U.freeze matchups <*> V.freeze results
