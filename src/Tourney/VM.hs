module Tourney.VM (
  VM,

  -- * Core VM functionality
  setup,
  loop,

  -- ** Simulations

  -- | These functions are intended for use in simulating outcomes of a
  -- tournament, to e.g. gauge how well it performs in terms of its rank
  -- preservation.
  simulateWith,
  simulateByEloDistribution,

  -- *** Utilities for interpretting results of a simulation
  inversions,
  countInversions,

  -- ** Functions for interacting with a VM
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
import System.Random.Stateful qualified as Random
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

-- | Create a 'VM' from a tournament and a fixed player count.
setup :: Tournament t -> PlayerCount -> IO VM
setup t count = do
  vmStream <- newIORef $! compile t (createStreamEnv count & withGetStandings Tourney.VM.getStandings)
  vmIState <- atomically (createIState count)
  pure VM{vmPlayerCount = count, vmStream, vmIState}

-- | Evaluate a 'VM' to the maximal extent allowed. Inspecting the result of
-- this function will reveal why the tournament stopped when it did; for
-- instance, if there are still pending matches, the final value in the
-- 'StepCodeEvent' vector will be @ 'Stepped' ('NeedResults\'Sorting' _ matches)
-- @ or @ 'NeedResults\'EndRound' _ matches @
loop :: VM -> IO (Vector StepCodeEvent)
loop = runVM (VB.build <$> go mempty)
  where
    go !xs = do
      istate <- asks vmIState
      ev <- tryStepCodeStream (atomically . tryRunStep istate)
      case ev of
        Stepped Continue{} -> go (xs <> VB.singleton ev)
        _ -> pure (xs <> VB.singleton ev)

-- | An event generated by stepping the VM
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

getLastStandings :: VM -> IO (Vector (Points, Player))
getLastStandings VM{vmIState = IStateVar var} =
  atomically (view (#history . traverseMax . #standings) <$> readTVar var)

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

--------------------------------------------------------------------------------
-- Full Simulation of tournaments using a probabilistic match result function

simulateWith :: MonadIO m => VM -> (Vector (Points, Player) -> Match -> m Result) -> m ()
simulateWith vm getResult = go
  where
    go = do
      lastEv <- V.lastM =<< liftIO (loop vm)
      lastStandings <- liftIO (getLastStandings vm)
      case lastEv of
        NoCode -> pure ()
        Stepped (NeedResults'Sorting roundNo matches) -> do
          forM_ matches \match -> do
            result <- getResult lastStandings match
            liftIO (setMatchResult vm roundNo match result)
          go
        Stepped (NeedResults'EndRound roundNo matches) -> do
          forM_ matches \match -> do
            result <- getResult lastStandings match
            liftIO (setMatchResult vm roundNo match result)
          go
        Stepped Continue{} -> error "impossible"

-- Simulations using the Elo rating system
----------------------------------------

pattern ELO_K_FACTOR :: Float
pattern ELO_K_FACTOR = 24

eloExpectedOutcomes :: Float -> Float -> (Float, Float)
eloExpectedOutcomes elo1 elo2 = (exp1, 1.0 - exp1)
  where
    !exp1 = 1.0 / (1 + 10.0 ** ((elo2 - elo1) / 400.0))

--
-- exp1 = 0.5*p_draw + win*expwin1
-- exp2 = 0.5*p_draw + win*expwin2
--
-- but p_draw = 0.0
--
-- Since draws are not allowed, ignore exp2

-- | Randomly guess a result of a match, using the input player Elo ratings to
-- calculate their respective win probabilities.
guessEloResult
  :: (MonadIO m, Random.StatefulGen g m)
  => TVar Float
  -> TVar Float
  -> g
  -> m Result
guessEloResult elo1Ref elo2Ref rgen = do
  elo1 <- readTVarIO elo1Ref
  elo2 <- readTVarIO elo2Ref
  let (exp1, _exp2) = eloExpectedOutcomes elo1 elo2
  -- Journeyman games/tournaments
  outcome <- Random.uniformRM (0.0, 1.0) rgen
  let (s1, s2) = if outcome <= exp1 then (1, 0) else (0, 1) :: (Int, Int)
  atomically do
    modifyTVar' elo1Ref (+ ELO_K_FACTOR * (realToFrac s1 - exp1))
    modifyTVar' elo2Ref (+ ELO_K_FACTOR * (realToFrac s2 - exp1))
  pure (Result (fromIntegral s1) (fromIntegral s2))

-- | Run a VM to completion, using the input Elo distribution to compute the win
-- probabilities of each player per match. A final Elo distribution is returned
-- which reflects changes in Elo that occured throughout the tournament.
simulateByEloDistribution :: Vector Float -> VM -> IO (Vector Float)
simulateByEloDistribution initialElos vm = do
  -- Probably I should use MutVar here, or simply a mutable vector
  elos <- V.mapM newTVarIO initialElos
  let getElo :: Player -> TVar Float
      getElo p = elos ^?! ix (asInt p)
  gen <- Random.newIOGenM =<< Random.newStdGen
  simulateWith vm \lastStandings (Match a b) -> do
    let !p1 = lastStandings ^?! ix (asInt a) . _2
    let !p2 = lastStandings ^?! ix (asInt b) . _2
    let !elo1 = getElo p1
    let !elo2 = getElo p2
    guessEloResult elo1 elo2 gen
  V.generateM (V.length initialElos) (readTVarIO . getElo . Player)

-- | Enumerate all the inversions of a vector
inversions :: Ord a => Vector a -> Vector a
inversions actuals = do
  (i, x) <- V.indexed actuals
  V.filter (< x) (V.drop i actuals)

-- | Count how many inversions there are in a vector
countInversions :: Ord a => Vector a -> Int
countInversions = V.length . inversions
