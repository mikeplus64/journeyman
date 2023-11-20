{-# LANGUAGE NoFieldSelectors #-}

module Tourney.VM.Interpret (
  -- * Setting up an interpreter
  IStateVar (..),
  IState (..),
  createIState,

  -- * Looking at updates
  StandingsUpdate (..),

  -- * Running it
  StepContinue (..),
  tryRunStep,
) where

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
  matrix <- createMatchMatrix count
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

--------------------------------------------------------------------------------

-- | Run a step
tryRunStep :: IStateVar -> TourneyOp -> STM StepContinue
tryRunStep IStateVar{var} o = do
  s <- readTVar var
  (!c, !s') <- stepOp o `runStateT` s
  c <$ case c of
    Continue{} -> writeTVar var s'
    _ -> pure ()

--------------------------------------------------------------------------------

data StepInfo
  = DidSort !RoundNo !(Vector (Match, Maybe Result)) ![SortAction]
  | NoInfo
  deriving stock (Eq, Show)

data StepContinue
  = NeedResults'Sorting !RoundNo !(Vector Match)
  | NeedResults'EndRound !RoundNo !(Vector Match)
  | Continue !StepInfo
  deriving stock (Eq, Show)

pattern CONT :: StepContinue
pattern CONT = Continue NoInfo

-- | Perform one step of the interpreter
stepOp :: TourneyOp -> StateT IState STM StepContinue
stepOp = \case
  MATCH m -> do
    matrix <- use #matrix
    roundNo <- use #roundNo
    _ <- lift (addMatch matrix roundNo m)
    pure CONT
  BEGIN_ROUND -> do
    #roundDepth += 1
    pure CONT
  PERFORM_SORTING focus method -> do
    IState{roundNo, matrix} <- get
    haveAny <- lift (haveAnyPendingMatchesWithin matrix roundNo focus)
    -- If there are any pending matches _within this focus_, block
    if haveAny
      then do
        pendingMatches <- lift (getPendingMatches matrix roundNo)
        pure (NeedResults'Sorting roundNo pendingMatches)
      else do
        matches <- lift (getMatches matrix roundNo)
        results <- lift (getMatchResults matrix roundNo)
        actions <- performSorter (Sorter focus method) results
        pure (Continue (DidSort roundNo matches actions))
  END_ROUND -> do
    matrix <- use #matrix
    roundNo <- use #roundNo
    count <- lift (pendingMatchCount matrix roundNo)
    -- If there are any pending matches before the round end, block
    if count > 0
      then do
        pendingMatches <- lift (getPendingMatches matrix roundNo)
        pure (NeedResults'EndRound roundNo pendingMatches)
      else do
        #roundNo += 1
        #roundDepth -= 1
        pure CONT

-- | Perform a sorter using the current 'RoundMatchMatrix'
performSorter :: Sorter -> Vector MatchResult -> StateT IState STM [SortAction]
performSorter sorter results = do
  IState{standings, roundNo, roundDepth} <- get
  let (actions, next) = runST do
        standingsMut <- V.thaw standings
        log <- runMatchesBy sorter results standingsMut
        frz <- V.unsafeFreeze standingsMut
        pure (log, frz)
  #standings .= next
  #history . at roundNo ?= StandingsUpdate{roundNo, roundDepth, standings = next}
  pure actions

-- results <- lift (getMatchResults matrix roundNo)
-- let !next = V.modify (runMatchesBy sorter results) standings
-- #standings .= next
