{-# LANGUAGE NoFieldSelectors #-}

module Tourney.VM.Interpret (
  -- * Setting up an interpreter
  IStateVar,
  createIState,
  interpGetPendingMatches,
  interpGetStandings,
  interpGetMatches,

  -- * Looking at updates
  StandingsUpdate (..),
  interpGetStandingsHistory,

  -- * Running it
  tryRunStep,
  runStepRetrying,
) where

import Control.Concurrent.STM (retry)
import Data.Vector qualified as V
import Tourney.Common
import Tourney.Match
import Tourney.Match.Matrix
import Tourney.SortingNetwork
import Tourney.VM.Code

-- | Interpreter state
newtype IStateVar = IStateVar
  { var :: TVar IState
  }
  deriving stock (Eq)

data IState = IState
  { roundDepth :: !Int
  , roundNo :: !RoundNo
  , matrix :: !MatchMatrix
  , standings :: !(V.Vector (Points, Player))
  , history :: !(MapByRound StandingsUpdate)
  }
  deriving stock (Generic)

data StandingsUpdate = StandingsUpdate
  { roundNo :: !RoundNo
  , roundDepth :: !Int
  , standings :: !(Vector (Points, Player))
  }
  deriving stock (Show, Eq, Ord, Generic)

createIState :: PlayerCount -> STM IStateVar
createIState count = do
  matrix <- createMatchMatrix
  let standings = V.generate count \i -> (0 :: Points, Player i)
  let state0 =
        IState
          { roundDepth = 0
          , roundNo = 0
          , matrix
          , standings
          , history = Empty & at (-1) ?~ StandingsUpdate (-1) 0 standings
          }
  var <- newTVar state0
  pure IStateVar{var}

interpGetPendingMatches :: IStateVar -> STM (Vector Match)
interpGetPendingMatches IStateVar{var} = do
  IState{roundNo, matrix} <- readTVar var
  getPendingMatches matrix roundNo

interpGetStandings :: IStateVar -> Focus -> STM Standings
interpGetStandings IStateVar{var} Focus{focusStart, focusLength} = do
  IState{standings} <- readTVar var
  pure (vectorToStandings (V.slice (asInt focusStart) focusLength (V.map snd standings)))

interpGetStandingsHistory :: IStateVar -> STM (MapByRound StandingsUpdate)
interpGetStandingsHistory IStateVar{var} = do
  IState{history} <- readTVar var
  pure history

interpGetMatches :: IStateVar -> STM (MapByRound (MapByMatches (Maybe Result)))
interpGetMatches IStateVar{var} = do
  readMatchMatrix . view #matrix =<< readTVar var

--------------------------------------------------------------------------------

-- | Run a step
tryRunStep :: IStateVar -> TourneyOp -> STM Bool
tryRunStep IStateVar{var} o = do
  s <- readTVar var
  (!continue, !s') <- stepOp o `runStateT` s
  when continue (writeTVar var s')
  pure continue

-- | Run a step, blocking until the step succeeds
runStepRetrying :: IStateVar -> TourneyOp -> STM ()
runStepRetrying IStateVar{var} o = do
  s <- readTVar var
  (!continue, !s') <- stepOp o `runStateT` s
  unless continue retry
  writeTVar var s'

--------------------------------------------------------------------------------

-- | Perform one step of the interpreter
stepOp :: TourneyOp -> StateT IState STM Bool
stepOp = \case
  MATCH m -> do
    matrix <- use #matrix
    roundNo <- use #roundNo
    _ <- lift (addMatch matrix roundNo m)
    pure True
  BEGIN_ROUND -> do
    #roundDepth += 1
    #roundNo += 1
    pure True
  PERFORM_SORTING focus method -> do
    IState{roundNo, roundDepth, standings, matrix} <- get
    haveAny <- lift (haveAnyPendingMatchesWithin matrix roundNo focus)
    -- If there are any pending matches _within this focus_, block
    if haveAny
      then pure False
      else do
        performSorter (Sorter focus method)
        #history . at roundNo ?= StandingsUpdate{roundNo, roundDepth, standings}
        pure True
  END_ROUND -> do
    matrix <- use #matrix
    roundNo <- use #roundNo
    count <- lift (pendingMatchCount matrix roundNo)
    -- If there are any pending matches before the round end, block
    if count > 0
      then pure False
      else do
        #roundDepth -= 1
        pure True

-- | Perform a sorter using the current 'RoundMatchMatrix'
performSorter :: Sorter -> StateT IState STM ()
performSorter sorter = do
  IState{matrix, standings, roundNo} <- get
  results <- lift (getMatchResults matrix roundNo)
  let !next = V.modify (runMatchesBy sorter results) standings
  #standings .= next
