module Tourney.VM (
  VM,
  setup,
  step,
  StepCodeEvent,
  loop,
  LoopEvent (..),
  getCodeSoFar,
  getMatches,
  getStandingsHistory,
  pendingMatches,
  peekCode,

  -- * Re-exports
  Code,
  MapByRound (..),
  MapByMatches (..),
  Match (..),
  Result (..),
  MatchResult (..),
  StandingsUpdate (..),
) where

import Tourney.Algebra.Unified
import Tourney.Common
import Tourney.Match
import Tourney.Match.Matrix
import Tourney.Stream
import Tourney.VM.Code
import Tourney.VM.Compile
import Tourney.VM.Interpret

data VM = VM
  { vmStream :: !(IORef (CodeStream WithVM))
  , vmIState :: !IStateVar
  , vmPlayerCount :: !PlayerCount
  }
  deriving stock (Generic)

setup :: Tournament t -> PlayerCount -> IO VM
setup t count = do
  vmStream <- newIORef $! compile t (createStreamEnv count & withGetStandings getStandings)
  vmIState <- atomically (createIState count)
  pure VM{vmPlayerCount = count, vmStream, vmIState}

data LoopEvent = VMDone | VMBlocked
  deriving stock (Show)

loop :: VM -> IO LoopEvent
loop = runVM go
  where
    go = do
      ev <- WithVM step
      case ev of
        SteppedOk -> go
        StepBlocked -> pure VMBlocked
        NoCode -> pure VMDone

step :: VM -> IO StepCodeEvent
step = runVM do
  istate <- asks vmIState
  tryStepCodeStream (atomically . tryRunStep istate)

getMatches :: VM -> IO (MapByRound (MapByMatches (Maybe Result)))
getMatches = runVM do
  istate <- asks vmIState
  atomically (interpGetMatches istate)

getCodeSoFar :: VM -> IO Code
getCodeSoFar VM{vmStream} = codeSoFar <$> readIORef vmStream

peekCode :: VM -> IO Code
peekCode = runVM debugCodeStream

getStandingsHistory :: VM -> IO (MapByRound StandingsUpdate)
getStandingsHistory VM{vmIState} = atomically (interpGetStandingsHistory vmIState)

pendingMatches :: VM -> STM (Vector Match)
pendingMatches VM{vmIState} = interpGetPendingMatches vmIState

--------------------------------------------------------------------------------

getStandings :: Focus -> WithVM Standings
getStandings focus = do
  istate <- asks vmIState
  atomically (interpGetStandings istate focus)

instance MonadCodeStream WithVM WithVM where
  getCodeStream = WithVM (readIORef . vmStream)
  putCodeStream c = WithVM \venv -> writeIORef (vmStream venv) $! c
  runCodeStreamEffect = id

newtype WithVM a = WithVM {runVM :: VM -> IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader VM) via (ReaderT VM IO)

instance PrimMonad WithVM where
  type PrimState WithVM = RealWorld
  primitive m = liftIO (primitive m)
