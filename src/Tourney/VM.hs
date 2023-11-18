module Tourney.VM (
  VM,
  setup,
  loop,
  StepCodeEvent (..),
  StepContinue (..),
  getCodeSoFar,
  getMatches,
  getStandingsHistory,
  getPendingMatches,
  getRoundNo,
  peekCode,
  setMatchResult,
  getMatch,

  -- * Re-exports
  Code,
  TourneyOp (..),
  MapByRound (..),
  MapByMatches (..),
  Match (..),
  Result (..),
  MatchResult (..),
  StandingsUpdate (..),
) where

import Data.Vector qualified as V
import Tourney.Algebra.Unified
import Tourney.Common
import Tourney.Match
import Tourney.Match.Matrix (MapByMatches, MapByRound)
import Tourney.Match.Matrix qualified as MatchMatrix
import Tourney.Stream
import Tourney.VM.Code
import Tourney.VM.Compile
import Tourney.VM.Interpret
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB

-- | An encapsulation of both a compiler and an interpreter of tournaments.
data VM = VM
  { vmStream :: !(IORef (CodeStream WithVM))
  , vmIState :: !IStateVar
  , vmPlayerCount :: !PlayerCount
  }
  deriving stock (Generic)

instance MonadCodeStream WithVM WithVM where
  getCodeStream = WithVM (readIORef . vmStream)
  putCodeStream c = WithVM \venv -> writeIORef (vmStream venv) $! c
  runCodeStreamEffect = id

newtype WithVM a = WithVM {runVM :: VM -> IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader VM) via (ReaderT VM IO)

instance PrimMonad WithVM where
  type PrimState WithVM = RealWorld
  primitive m = liftIO (primitive m)

--------------------------------------------------------------------------------
-- Core tournament VM functionality

setup :: Tournament t -> PlayerCount -> IO VM
setup t count = do
  vmStream <- newIORef $! compile t (createStreamEnv count & withGetStandings Tourney.VM.getStandings)
  vmIState <- atomically (createIState count)
  pure VM{vmPlayerCount = count, vmStream, vmIState}

loop :: VM -> IO (Vector StepCodeEvent)
loop = runVM (VB.build <$> go mempty)
  where
    go !xs = do
      istate <- asks vmIState
      ev <- tryStepCodeStream (atomically . tryRunStep istate)
      case ev of
        Stepped Continue{} -> go (xs <> VB.singleton ev)
        _ -> pure (xs <> VB.singleton ev)

data StepCodeEvent
  = NoCode
  | Stepped StepContinue
  deriving stock (Eq, Show)

-- | Step the code-stream by an interpretting function. If the function returns
-- false, the previous code stream state is restored.
tryStepCodeStream :: MonadCodeStream m c => (TourneyOp -> m StepContinue) -> m StepCodeEvent
tryStepCodeStream stepFn = do
  prev <- getCodeStream
  pop <- popCodeStream
  mok <- forM pop \o -> do
    ok <- stepFn o
    case ok of
      Continue{} -> pure ()
      _ -> putCodeStream prev
    pure ok
  pure $! maybe NoCode Stepped mok

--------------------------------------------------------------------------------
-- Utility functions for seeing or manipulating state of the current tournament

getMatches :: VM -> IO (MapByRound (MapByMatches (Maybe Result)))
getMatches VM{vmIState = IStateVar var} =
  atomically (MatchMatrix.readMatchMatrix . view #matrix =<< readTVar var)

setMatchResult :: VM -> RoundNo -> Match -> Result -> IO Bool
setMatchResult VM{vmIState = IStateVar var} roundNo match result =
  atomically do
    IState{roundNo = curRoundNo, matrix} <- readTVar var
    if roundNo == curRoundNo
      then True <$ MatchMatrix.setMatchResult matrix roundNo match result
      else pure False

getMatch :: VM -> RoundNo -> Match -> IO (Maybe (Maybe Result))
getMatch VM{vmIState = IStateVar var} roundNo match =
  atomically do
    IState{roundNo = curRoundNo, matrix} <- readTVar var
    if roundNo == curRoundNo
      then MatchMatrix.getMatch matrix roundNo match
      else pure Nothing

getCodeSoFar :: VM -> IO Code
getCodeSoFar VM{vmStream} = codeSoFar <$> readIORef vmStream

peekCode :: VM -> IO Code
peekCode = runVM debugCodeStream

getStandingsHistory :: VM -> IO (MapByRound StandingsUpdate)
getStandingsHistory VM{vmIState = IStateVar var} =
  atomically (view #history <$> readTVar var)

getPendingMatches :: VM -> IO (Vector Match)
getPendingMatches VM{vmIState = IStateVar var} = atomically do
  IState{roundNo, matrix} <- readTVar var
  MatchMatrix.getPendingMatches matrix roundNo

getStandings :: Focus -> WithVM Standings
getStandings Focus{focusStart, focusLength} = WithVM \VM{vmIState = IStateVar var} -> do
  standings <- view #standings <$> readTVarIO var
  pure (vectorToStandings (V.slice (asInt focusStart) focusLength (V.map snd standings)))

getRoundNo :: VM -> IO RoundNo
getRoundNo VM{vmIState = IStateVar var} = view #roundNo <$> readTVarIO var
