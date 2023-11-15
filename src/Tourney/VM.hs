module Tourney.VM (
  setup,
  loop,
  code,
  module Tourney.VM.Compile,
  module Tourney.VM.Code,
  module Tourney.VM.Interpret,
) where

import Tourney.Algebra.Builder (execSteps)
import Tourney.Algebra.Unified
import Tourney.Common
import Tourney.Format.SingleElimination
import Tourney.Match
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

pendingMatches :: VM -> IO (Vector Match)
pendingMatches = runVM do
  istate <- asks vmIState
  atomically (interpGetPendingMatches istate)

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

code :: VM -> IO Code
code VM{vmStream} = codeSoFar <$> readIORef vmStream

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

--------------------------------------------------------------------------------

test = setup (execSteps id singleElimination) 8

runSE = do
  vm <- setup (execSteps id singleElimination) 8
  loop vm
