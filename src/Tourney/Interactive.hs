{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move guards forward" #-}

module Tourney.Interactive where

import Brick qualified
import Brick.Widgets.Border qualified as Brick
import Brick.Widgets.Border.Style
import Control.Concurrent.MVar (modifyMVar)
import Control.Lens
import Control.Monad.Primitive
import Control.Monad.ST.Strict
import Data.Array.ST qualified as A
import Data.Array.Unboxed qualified as A
import Data.Colour qualified as Colour
import Data.Colour.CIE qualified as Colour
import Data.Colour.CIE.Illuminant qualified as Colour
import Data.Colour.Names qualified as Colour
import Data.Colour.Palette.BrewerSet qualified as Colour
import Data.Colour.SRGB qualified as Colour
import Data.Dependency (MonadRequest (..), StreamM (..))
import Data.Dependency qualified as S
import Data.Foldable qualified as Fold
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Zipper qualified as ZNE
import Data.Map.Strict qualified as Map
import Data.MinMax
import Data.Tuple.Ordered
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as U
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as I
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events
import Tourney.Algebra
import Tourney.Format.DoubleElimination
import Tourney.Format.RoundRobin
import Tourney.Format.SingleElimination
import Tourney.SortingNetwork
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB

type RPState = NonEmpty (V.Vector (Points, Player))

newtype ResultsProviderIO a = RP {runRP :: StateT RPState IO a}
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState RPState) via (StateT RPState IO)

instance PrimMonad ResultsProviderIO where
  type PrimState ResultsProviderIO = RealWorld
  primitive f = RP (primitive f)

instance MonadRequest Standings ResultsProviderIO where
  request = gets (V.convert . V.map snd . head)

createRPState :: Standings -> RPState
createRPState initialStandings = one (V.map (0 :: Points,) (V.convert initialStandings))

defaultStandings :: PlayerCount -> Standings
defaultStandings count = U.enumFromTo 0 (count - 1)

--------------------------------------------------------------------------------
-- Describing the state of a tournament

data Rounds = Rounds
  { state :: !RoundsState
  , rounds :: !(V.Vector CompiledStep)
  , standingsInitial :: !Standings
  , playerCount :: !Int
  , steps :: Steps ResultsProviderIO ()
  }
  deriving stock (Generic)

data RoundsState = RoundsState
  { past :: ![PastRound]
  , current :: !ActiveRound
  , future :: ![FutureRound]
  }
  deriving stock (Generic)

data ActiveRound = ActiveRound
  { standingsBefore :: !(V.Vector (Player, Points))
  , standingsNow :: !(VM.IOVector (Player, Points))
  , matches :: !(V.Vector (Sorter, V.Vector Match))
  , results :: !(Map Match (Int, MatchResult))
  }
  deriving stock (Generic)

data PastRound = PastRound
  { standingsBefore :: !Standings
  , standingsAfter :: !Standings
  , matches :: V.Vector (Sorter, V.Vector Match)
  , results :: !(Map Match (Int, MatchResult))
  }
  deriving stock (Generic)

type FutureRound = CompiledStep

createRounds :: Steps ResultsProviderIO () -> Either Standings PlayerCount -> IO Rounds
createRounds steps esp = do
  stateRef <- newIORef initialResults
  let roundStream = compileRounds steps playerCount
  (steps, a) <- runRP (S.fold (:) [] roundStream) `runStateT` initialResults
  pure undefined
  where
    -- Rounds
    --   { state =
    --       RoundsState
    --         { past = []
    --         , current = undefined
    --         , future = undefined
    --         }
    --   , steps
    --   , playerCount
    --   , standingsInitial
    --   }

    !standingsInitial = either id (U.enumFromTo 0 . subtract 1) esp
    !initialResults = one (V.map (0,) (V.convert standingsInitial)) :: RPState
    !playerCount = either U.length id esp
    currentRound = undefined

-- advanceRound :: Brick.EventM AppElement AppState ()
-- advanceRound = do
--   s <- use #roundStream
--   let (rounds, _isComplete, next) = S.peekAll s
--   roundsWithMatches :: Maybe (NE.NonEmpty (CompiledStep, V.Vector (Sorter, V.Vector Match))) <-
--     forM (NE.nonEmpty rounds) $ mapM \compiledStep -> do
--       let matchStream = getMatches compiledStep
--       matches <- VB.build <$> S.fold (\x xs -> VB.singleton x <> xs) mempty matchStream
--       pure (compiledStep, matches)
--   #roundStream .= next
--   #rounds .= fmap ZNE.fromNonEmpty roundsWithMatches

-- --------------------------------------------------------------------------------
-- -- A terminal UI for running tournaments

-- newtype RoundState = RoundState
--   { matches :: V.Vector (Sorter, V.Vector Match, [MatchResult])
--   }

-- data AppEvent
--   = AddResult MatchResult
--   | AdvanceRound
--   | Quit
--   | Noop

-- data AppState = AppState
--   { steps :: Steps ResultsProviderIO ()
--   , rpState :: RPState
--   , roundStream :: S.StreamM ResultsProviderIO CompiledStep ()
--   , playerCount :: PlayerCount
--   , initialStandings :: Standings
--   , currentMatchStream :: S.StreamM ResultsProviderIO (Sorter, V.Vector Match) ()
--   , rounds :: Rounds
--   -- ^ Rounds. Past rounds are stored on the left side of the zipper, the
--   -- current is stored in the middle, and future rounds are stored on the right
--   -- side of the zipper. Care needs to be taken to update the future rounds
--   -- after any current round update, as they may depend on standings that could
--   -- have changed
--   , matchHistory :: [[(Sorter, Match, MatchResult)]]
--   -- ^ Match history, most recent first, by round
--   , borderStyle :: BorderStyle
--   }
--   deriving stock (Generic)

-- data AppElement = Main | ScrollBar Brick.ClickableScrollbarElement AppElement
--   deriving stock (Show, Ord, Eq)

-- liftRP :: (MonadState AppState m, MonadIO m) => ResultsProviderIO a -> m a
-- liftRP (RP m) = do
--   rp <- use #rpState
--   (a, !rp') <- liftIO (m `runStateT` rp)
--   #rpState .= rp'
--   pure a

-- interactivelyRunSteps :: Steps ResultsProviderIO () -> IO AppState
-- interactivelyRunSteps steps =
--   Brick.defaultMain
--     app
--     AppState
--       { steps
--       , rpState = one V.empty
--       , roundStream = mempty
--       , playerCount = 16
--       , initialStandings = mempty
--       , currentMatchStream = mempty
--       , rounds = Nothing
--       , matchHistory = mempty
--       , borderStyle = unicodeBold
--       }
--   where
--     app :: Brick.App AppState AppEvent AppElement
--     app =
--       Brick.App
--         { appDraw = \s -> [render s]
--         , appChooseCursor = Brick.showFirstCursor
--         , appHandleEvent = handleBrickEvent
--         , appStartEvent = setupApp
--         , appAttrMap = \_ -> Brick.attrMap I.defAttr []
--         }

--     handleBrickEvent = \case
--       Brick.VtyEvent (EvKey key mods) -> case (key, mods) of
--         (KChar 'c', [MCtrl]) -> Brick.halt
--         (KChar 'q', []) -> Brick.halt
--         (KChar ' ', []) -> advanceRound
--         (KLeft, []) -> Brick.hScrollBy (Brick.viewportScroll Main) (-6)
--         (KRight, []) -> Brick.hScrollBy (Brick.viewportScroll Main) 6
--         _ -> pure ()
--       Brick.MouseDown _ BScrollUp _ _ ->
--         Brick.hScrollBy (Brick.viewportScroll Main) (-6)
--       Brick.MouseDown _ BScrollDown _ _ ->
--         Brick.hScrollBy (Brick.viewportScroll Main) 6
--       Brick.MouseDown (ScrollBar el Main) BLeft _ _ -> do
--         mapM_ (Brick.hScrollBy (Brick.viewportScroll Main)) case el of
--           Brick.SBHandleBefore -> Just (-6)
--           Brick.SBHandleAfter -> Just 6
--           Brick.SBTroughBefore -> Just (-12)
--           Brick.SBTroughAfter -> Just 12
--           _ -> Nothing
--       _ -> pure ()

--     render :: AppState -> Brick.Widget AppElement
--     render s =
--       Brick.vBox
--         [ setGreedyV (canvas s)
--         , panel s
--         ]

--     panel :: AppState -> Brick.Widget AppElement
--     panel s =
--       Brick.withBorderStyle unicodeRounded
--         . Brick.padAll 1
--         . Brick.borderWithLabel (Brick.txt "journeyman")
--         $ Brick.txtWrap "asdfasfd"

--     canvas :: AppState -> Brick.Widget AppElement
--     canvas s =
--       Brick.withClickableHScrollBars ScrollBar
--         . Brick.withHScrollBars Brick.OnBottom
--         . Brick.viewport Main Brick.Both
--         . Brick.padAll 1
--         . Brick.hBox
--         . map (Brick.padAll 1 . drawRound (s ^. #playerCount))
--         $ toList (s ^. #rounds)
--           ^.. each . _2

--     setGreedyV :: Brick.Widget a -> Brick.Widget a
--     setGreedyV w = w{Brick.vSize = Brick.Greedy}

-- setupApp :: Brick.EventM AppElement AppState ()
-- setupApp = do
--   -- Enable mouse support
--   vty <- Brick.getVtyHandle
--   liftIO (Vty.setMode (Vty.outputIface vty) Vty.Mouse True)
--   -- Setup the rounds initially
--   steps <- use #steps
--   count <- use #playerCount
--   #roundStream .= compileRounds steps count

-- -- advanceRound :: Brick.EventM AppElement AppState ()
-- -- advanceRound = do
-- --   s <- use #roundStream
-- --   let (rounds, _isComplete, next) = S.peekAll s
-- --   roundsWithMatches :: Maybe (NE.NonEmpty (CompiledStep, V.Vector (Sorter, V.Vector Match))) <-
-- --     forM (NE.nonEmpty rounds) $ mapM \compiledStep -> do
-- --       let matchStream = getMatches compiledStep
-- --       matches <- VB.build <$> S.fold (\x xs -> VB.singleton x <> xs) mempty matchStream
-- --       pure (compiledStep, matches)
-- --   #roundStream .= next
-- --   #rounds .= fmap ZNE.fromNonEmpty roundsWithMatches

-- -- s' <- liftRP do
-- --   next <- S.peekAll s
-- --   case next of
-- --     Nothing -> pure Nothing
-- --     Just (compiledStep, s') -> do
-- --       pure (Just (VB.build @V.Vector matches, s'))

-- -- forM_ s' \(matches, s'') -> do
-- --   #roundStream .= s''
-- --   mcur <- use #activeRounds
-- --   forM_ mcur \cur -> #activeRounds %= (cur :)
-- --   #activeRounds %= (matches :)

drawRound :: PlayerCount -> V.Vector (Sorter, V.Vector Match) -> Brick.Widget AppElement
drawRound pc matches =
  Brick.raw $
    I.horizCat
      [ vertLine False
      , I.horizCat
          [ drawMatchColumn m
          | m <- V.toList (matches >>= snd)
          ]
      , horizLines
      , vertLine True
      ]
  where
    !count = maximumOf (each . _2 . each . larger) matches & maybe pc (+ 1) & max pc
    !width = length (show count :: String)
    vertLine right =
      I.vertCat
        . map (\f -> I.char (I.defAttr `I.withForeColor` dim) (f unicodeRounded))
        . concat
        $ [ [if right then bsCornerTR else bsCornerTL]
          , replicate (count - 2) bsVertical
          , [if right then bsCornerBR else bsCornerBL]
          ]
    horizLines = I.vertCat (replicate count (I.char (I.defAttr `I.withForeColor` dim) (bsHorizontal unicodeRounded)))
    drawMatchColumn (OrdPair_ low high) =
      I.horizCat
        [ horizLines
        , I.vertCat
            [ if
              | low < m && m < high -> I.string (matchColour low high m) (replicate width ' ')
              | m == low -> I.string (matchColour low high m) (pad (show m))
              | m == high -> I.string (matchColour low high m) (pad (show m))
              | otherwise -> I.string (I.defAttr `I.withForeColor` dim) (replicate width (bsHorizontal unicode))
            | m <- [0 .. count - 1]
            ]
        ]
    dim = I.linearColor @Word8 50 50 50
    pad x = replicate (width - length x) ' ' ++ x
    matchColour low high actual =
      I.defAttr
        `I.withBackColor` toVtyRGB midColour
        `I.withForeColor` toVtyRGB textColour
        `I.withStyle` I.bold
      where
        textColour, midColour, lowColour, highColour :: Colour.Colour Double
        !textColour
          | isBye = Colour.grey
          | Colour.luminance midColour > 0.3 = Colour.black
          | otherwise = Colour.white
        !midColour =
          Colour.blend (sqrt dist) lowColour highColour
            & Colour.darken (if isBye then 0.01 else 1.0)
        !lowColour = colours V.! low
        !highColour = colours V.! high
        !dist = fromIntegral (high - actual) / fromIntegral (high - low)
        !isBye = not (low >= 0 && high < pc)
    colours = V.fromList (take count (cycle (Colour.brewerSet Colour.Paired 12)))
    toVtyRGB c = case Colour.toSRGB24 c of
      Colour.RGB r g b -> I.linearColor r g b

--------------------------------------------------------------------------------
-- Utility

-- | Run a 'StateT' action to update an 'IORef'. Non-atomic update.
modifyIORefByState :: MonadIO m => IORef s -> StateT s m a -> m a
modifyIORefByState ref update = do
  s0 <- readIORef ref
  (a, !s1) <- runStateT update s0
  writeIORef ref s1
  pure a

-- | Run a 'StateT' action to update an 'IORef'. Atomic update.
modifyMVarByState :: MonadIO m => MVar s -> StateT s IO a -> IO a
modifyMVarByState ref update =
  modifyMVar ref \s0 -> do
    (a, !s1) <- runStateT update s0
    pure (s1, a)
