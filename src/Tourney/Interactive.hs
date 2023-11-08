{-# LANGUAGE RecordWildCards #-}

module Tourney.Interactive where

import Brick qualified
import Brick.Widgets.Border.Style
import Control.Lens
import Control.Monad.Primitive
import Control.Monad.ST.Strict
import Data.Array.ST qualified as A
import Data.Array.Unboxed qualified as A
import Data.Dependency (MonadRequest (..), StreamM (..))
import Data.Dependency qualified as S
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.MinMax
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Image
import Graphics.Vty.Input.Events
import Tourney.Algebra.Monad
import Tourney.Algebra.Step
import Tourney.Format.DoubleElimination
import Tourney.Format.RoundRobin
import Tourney.Format.SingleElimination
import Tourney.Match
import Tourney.SortingNetwork
import Tourney.Types

type RPState = NonEmpty (V.Vector (Points, Player))

newtype ResultsProviderIO a = RP {runRP :: StateT RPState IO a}
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState RPState) via (StateT RPState IO)

instance PrimMonad ResultsProviderIO where
  type PrimState ResultsProviderIO = RealWorld
  primitive f = RP (primitive f)

instance MonadRequest Standings ResultsProviderIO where
  request = gets (V.convert . V.map snd . head)

runSteps
  :: forall m
   . (PrimMonad m, MonadState RPState m, MonadRequest Standings m)
  => (Match -> m MatchResult)
  -> Steps m ()
  -> Standings
  -> m ()
runSteps runMatch steps initialStandings =
  S.for_ roundStream \rd -> do
    let matchStream = getMatches @m rd
    roundStartStandings <- gets head
    nextStandingsMut <- V.thaw roundStartStandings
    S.for_ matchStream \(sorter, matches) -> do
      runMatchesBy runMatch sorter matches nextStandingsMut
    nextStandings <- V.unsafeFreeze nextStandingsMut
    modify' (NE.cons nextStandings)
  where
    roundStream = compileRounds steps (U.length initialStandings)

runStepsIO
  :: (Match -> IO MatchResult)
  -> Steps ResultsProviderIO ()
  -> Standings
  -> IO RPState
runStepsIO runMatch steps initialStandings =
  runRP (runSteps (liftIO . runMatch) steps initialStandings) `execStateT` one initialState
  where
    !initialState = V.map (0 :: Points,) (V.convert initialStandings)

defaultStandings :: PlayerCount -> Standings
defaultStandings count = U.enumFromTo 0 (count - 1)

--------------------------------------------------------------------------------
-- A terminal UI for running tournaments

data Round = Round
  { matches :: Map Sorter (V.Vector Match)
  , results :: [(Sorter, MatchResult)]
  }

data AppEvent
  = AddResult MatchResult
  | AdvanceRound
  | Quit
  | Noop

data AppState = AppState
  { steps :: Steps ResultsProviderIO ()
  , rpState :: RPState
  , roundStream :: S.StreamM ResultsProviderIO CompiledStep ()
  , playerCount :: PlayerCount
  , initialStandings :: Standings
  , currentMatchStream :: S.StreamM ResultsProviderIO (Sorter, V.Vector Match) ()
  , currentMatches :: Maybe (V.Vector (Sorter, V.Vector Match))
  , pastRounds :: [V.Vector (Sorter, V.Vector Match)]
  -- ^ Round history, most recent first
  , matchHistory :: [[(Sorter, Match, MatchResult)]]
  -- ^ Match history, most recent first, by round
  , borderStyle :: BorderStyle
  }
  deriving stock (Generic)

setupRounds :: MonadState AppState m => PlayerCount -> m ()
setupRounds count = do
  steps <- use #steps
  #playerCount .= count
  #roundStream .= compileRounds steps count

liftRP :: (MonadState AppState m, MonadIO m) => ResultsProviderIO a -> m a
liftRP (RP m) = do
  rp <- use #rpState
  (a, !rp') <- liftIO (m `runStateT` rp)
  #rpState .= rp'
  pure a

advanceRound :: (MonadState AppState m, MonadIO m) => m ()
advanceRound = do
  s <- use #roundStream
  liftRP (S.pop s) >>= mapM_ \(compiledStep, s') -> do
    #roundStream .= s'
    #currentMatchStream .= getMatches compiledStep
  pure ()

advanceMatches :: (MonadState AppState m, MonadIO m) => m ()
advanceMatches = do
  use #roundStream

interactivelyRunSteps :: Steps ResultsProviderIO () -> IO AppState
interactivelyRunSteps steps =
  Brick.defaultMain
    app
    AppState
      { steps
      , roundStream = mempty
      , playerCount = 0
      , initialStandings = mempty
      , currentMatchStream = mempty
      , currentMatches = mempty
      , pastRounds = []
      , matchHistory = []
      , borderStyle = unicodeBold
      }
  where
    app :: Brick.App AppState AppEvent ()
    app =
      Brick.App
        { appDraw = \s -> [draw s]
        , appChooseCursor = Brick.showFirstCursor
        , appHandleEvent = handleEvent . reduceBrickEvent
        , appStartEvent = pure ()
        , appAttrMap = \_ -> Brick.attrMap defAttr []
        }

    reduceBrickEvent (Brick.VtyEvent (EvKey key mods)) = case (key, mods) of
      (KChar 'c', [MCtrl]) -> Quit
      (KChar 'q', []) -> Quit
      (KChar ' ', []) -> AdvanceRound
      _ -> Noop
    reduceBrickEvent _ = Noop

    handleEvent = \case
      Quit -> Brick.halt
      AdvanceRound -> pure ()
      _ -> pure ()

    draw :: AppState -> Brick.Widget ()
    draw state = undefined

drawRound :: Int -> PlayerCount -> V.Vector (Sorter, V.Vector Match) -> Brick.Widget ()
drawRound height count matches =
  Brick.raw image
  where
    image =
      vertCat
        [ pad 0 0 0 linesBetweenSlots (charFill defAttr bsHorizontal width 1)
        | _ <- [0 .. count - 1]
        ]

    width = 10
    BorderStyle{..} = unicodeBold -- provides bsCornerTL, bsHorizontal, bsVertical, etc.
    !linesBetweenSlots = height `div` count
    (minMatch, maxMatch) = case minMax (foldMap (foldMap ordPairToMinMax . snd) matches) of
      Just (l, h) -> (l, h)
      Nothing -> (0, 0)
