{-# LANGUAGE RecursiveDo #-}

module Tourney.IOResultsProvider where

import Control.Concurrent.MVar (modifyMVar)
import Control.Lens
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Array.IO (IOArray, getBounds, newArray, readArray, writeArray)
import Data.Array.Unboxed
import Data.Dependency qualified as S
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Tourney.Algebra.Unified
import Tourney.Common
import Tourney.Match
import Tourney.Match.Matrix
import Tourney.SortingNetwork
import Tourney.Stream
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB
import Prelude hiding (round)

newtype IOResultsProvider a = IOResultsProvider
  { runIRP :: IORef IOResultsState -> IO a
  }
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadReader (IORef IOResultsState))
    via (ReaderT (IORef IOResultsState) IO)

instance PrimMonad IOResultsProvider where
  type PrimState IOResultsProvider = RealWorld
  primitive x = IOResultsProvider \_ -> primitive x

data IOResultsState = IOResultsState
  { playerCount :: !Int
  , step :: IO [TourneyStep]
  -- ^ The primary interface for communicating with a 'IOResultsProvider'
  -- tournament. Pending matches are stored in the @interface@ field as
  -- 'Nothing', and matches that have been completed are represented by a
  -- completed 'MatchResult'. To advance the tournament after updating this
  -- map, call the 'advance' field.
  , matrix :: !IORoundMatchMatrix
  , current :: !(V.Vector (Player, Points))
  -- ^ The working area for standings. This will only update at the end of each
  -- round.
  , progression :: !(IOArray (Player, RoundNo) (Slot, Points))
  -- ^ A trace of every player's position and points throughout the tournament
  , roundNo :: !RoundNo
  }
  deriving stock (Generic)

createInitialState :: Tournament TMany -> Either PlayerCount Standings -> IO (IORef IOResultsState)
createInitialState tournament s = do
  -- choose 8 for number of rounds; it'll expand later as-needed
  progression <- newArray ((0, 0), (count - 1, 8)) (0, 0)
  matrix <- createRoundMatchMatrix count
  stateRef <-
    newIORef
      IOResultsState
        { playerCount = count
        , step = pure undefined
        , matrix
        , current = V.generate count \i -> (standings U.! i, 0 :: Points)
        , roundNo = -1
        , progression
        }
  doStep <- createDoStep count tournament stateRef
  modifyIORef' stateRef (#step .~ doStep)
  initNextRound stateRef
  pure stateRef
  where
    !count = either id U.length s
    !standings = either createInitialStandings id s

initNextRound :: IORef IOResultsState -> IO ()
initNextRound ref = do
  IOResultsState{current, roundNo, progression = prog0} <- readIORef ref
  -- Populate the 'round'-th row of the progression array by the 'current' array
  V.iforM_ current \slot (player, points) -> do
    writeArray prog0 (player, roundNo) (slot, points)
  -- Resize the progression array to fit the next round
  (_, (players, roundCapacity)) <- getBounds prog0
  let !nextRound = roundNo + 1

  when (nextRound > roundCapacity) do
    let !newBounds = ((0, 0), (players - 1, nextRound))
    prog1 <- newArray newBounds (0 :: Slot, 0 :: Points)
    forM_ (range newBounds) \i -> do
      orig <- readArray prog0 i
      writeArray prog1 i orig
    modifyIORef' ref (#progression .~ prog1)

  -- Save the progression made in the last round
  when (roundNo >= 0) do
    original <- V.thaw current
    runMatchesBy _ _ _

  modifyIORef' ref (#roundNo .~ nextRound)

data TourneyStep
  = NeedMatches
  | GotRound
  | HadError CompileError

createDoStep :: PlayerCount -> Tournament TMany -> IORef IOResultsState -> IO (IO [TourneyStep])
createDoStep count tournament stateRef = do
  streamRef <- newMVar (runTourney @IOResultsProvider (createTourney cenv tournament))
  let
    step :: StateT (TourneyStream IOResultsProvider) IOResultsProvider [TourneyStep]
    step = do
      self <- ask
      IOResultsState{matrix} <- readIORef self
      pendings <- pendingMatchCount matrix
      if pendings > 0
        then pure [NeedMatches]
        else do
          now :: TourneyStream IOResultsProvider <- get
          nextRound <- lift (S.popForced now)
          case nextRound of
            Nothing -> pure []
            Just (roundStream, stream1) -> do
              put stream1
              nextMatrix <- createRoundMatchMatrix count
              errorsRef <- newIORef []
              lift $ S.for_ roundStream \case
                Left err -> modifyIORef errorsRef (err :)
                Right (_sorter, g) -> do
                  (matchErrs, gmatches) <- partitionEithers <$> S.toList g
                  unless (null matchErrs) (modifyIORef' errorsRef (matchErrs ++))
                  mapM_ (addMatch nextMatrix) gmatches

              liftIO (initNextRound self)
              modifyIORef' self (#matrix .~ nextMatrix)
              errors <- readIORef errorsRef
              pure (GotRound : concatMap (map HadError . toList) errors)

  -- round <- fmap (VB.build @V.Vector . fold) . lift $ S.toVector $ S.for roundStream \case
  --   Left err -> do
  --     modifyIORef' errorsRef (<> VB.singleton err)
  --     pure VB.empty
  --   Right (_sorter, g) -> do

  -- errors <- VB.build @V.Vector <$> readIORef errorsRef

  -- if null errors
  --   then pure [round]
  --   else _

  pure (modifyMVar streamRef (fmap swap . flip runIRP stateRef . runStateT step))
  where
    cenv = createCompileEnv count & withGetStandings getStandingsIO

--------------------------------------------------------------------------------

getStandingsIO :: Focus -> IOResultsProvider Standings
getStandingsIO focus = do
  IOResultsState{current} <- readIORef =<< ask
  pure (U.convert (V.slice (focusStart focus) (focusLength focus) (V.map fst current)))
