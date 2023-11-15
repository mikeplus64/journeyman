{-# LANGUAGE NoFieldSelectors #-}

module Tourney.VM.Interpret (
  -- * Setting up an interpreter
  IStateVar,
  createIState,
  interpGetPendingMatches,
  interpGetStandings,

  -- * Running it
  tryRunStep,
  runStepRetrying,
) where

import Control.Concurrent.STM (retry)
import Data.Vector qualified as V
import Tourney.Algebra
import Tourney.Match.Matrix
import Tourney.SortingNetwork
import Tourney.VM.Code

-- | Interpreter state
newtype IStateVar = IStateVar
  { var :: TVar IState
  }
  deriving stock (Eq)

data StandingsUpdate = StandingsUpdate
  { roundNo :: !RoundNo
  , roundDepth :: !Int
  , standings :: !(Vector (Points, Player))
  }

createIState :: PlayerCount -> STM IStateVar
createIState count = do
  matrix <- createRoundMatchMatrix count
  let standings = V.generate count (0 :: Points,)
  let update0 = StandingsUpdate 0 0 standings
  let state0 =
        IState
          { roundDepth = 0
          , roundNo = 0
          , matrix
          , standings
          , history = update0 :| []
          }
  var <- newTVar state0
  pure IStateVar{var}

interpGetPendingMatches :: IStateVar -> STM (V.Vector Match)
interpGetPendingMatches IStateVar{var} = readPendingMatches . view #matrix =<< readTVar var

interpGetStandings :: IStateVar -> Focus -> STM Standings
interpGetStandings IStateVar{var} Focus{focusStart, focusLength} = do
  IState{history = StandingsUpdate{standings} :| _} <- readTVar var
  pure (V.convert (V.slice focusStart focusLength (V.map snd standings)))

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

data IState = IState
  { roundDepth :: !Int
  , roundNo :: !Int
  , matrix :: !RoundMatchMatrix
  , standings :: !(V.Vector (Points, Player))
  , history :: !(NonEmpty StandingsUpdate)
  }
  deriving stock (Generic)

--------------------------------------------------------------------------------

-- | Perform one step of the interpreter
stepOp :: TourneyOp -> StateT IState STM Bool
stepOp = \case
  MATCH m -> do
    matrix <- use #matrix
    _ <- lift (addMatch matrix m)
    pure True
  BEGIN_ROUND -> do
    #roundDepth += 1
    #roundNo += 1
    pure True
  PERFORM_SORTING focus method -> do
    IState{roundNo, roundDepth, standings, matrix} <- get
    count <- lift (pendingMatchesWithinCount matrix focus)
    -- If there are any pending matches _within this focus_, block
    if count == 0
      then do
        performSorter (Sorter focus method)
        #history %= addStandingsUpdate StandingsUpdate{roundNo, roundDepth, standings}
        True <$ lift (clearMatchMatrix matrix focus)
      else pure False
  END_ROUND -> do
    matrix <- use #matrix
    count <- lift (pendingMatchCount matrix)
    when (count == 0) (#roundDepth -= 1)
    -- If there are any pending matches before the round end, block
    pure (count == 0)

-- | Perform a sorter using the current 'RoundMatchMatrix'
performSorter :: Sorter -> StateT IState STM ()
performSorter sorter = do
  matrix <- use #matrix
  standings <- use #standings
  results <- lift (readMatchResults matrix)
  let !next = V.modify (runMatchesBy sorter results) standings
  #standings .= next

addStandingsUpdate :: StandingsUpdate -> NonEmpty StandingsUpdate -> NonEmpty StandingsUpdate
addStandingsUpdate next@StandingsUpdate{roundNo, roundDepth} (x :| xs)
  | sameCoords x = next :| xs
  | otherwise = next :| (x : xs)
  where
    sameCoords StandingsUpdate{roundNo = prevRoundNo, roundDepth = prevRoundDepth} =
      roundNo == prevRoundNo && roundDepth == prevRoundDepth
