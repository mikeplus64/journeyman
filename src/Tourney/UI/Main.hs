{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Tourney.UI.Main where

import Brick hiding (Max, Result)
import Brick qualified
import Brick.Types (BrickEvent)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Colour qualified as Colour
import Data.Colour.CIE qualified as Colour
import Data.Colour.CIE.Illuminant qualified as Colour
import Data.Colour.Names qualified as Colour
import Data.Colour.Palette.BrewerSet qualified as Colour
import Data.Colour.SRGB qualified as Colour
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as I
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events

import Control.Concurrent.MVar (modifyMVar)
import Control.Lens
import Control.Monad.Primitive
import Control.Monad.ST.Strict
import Data.Align (alignWith)
import Data.Array.ST qualified as A
import Data.Array.Unboxed qualified as A
import Data.Dependency (MonadRequest (..), StreamM (..))
import Data.Dependency qualified as S
import Data.Foldable qualified as Fold
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Zipper qualified as ZNE
import Data.Map.Strict qualified as Map
import Data.MinMax
import Data.These
import Data.Tuple.Ordered
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as U
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB

import Tourney.Algebra (Depth (..), Tournament, execSteps)
import Tourney.Common
import Tourney.Format.DoubleElimination
import Tourney.Format.RoundRobin
import Tourney.Format.SingleElimination
import Tourney.Match
import Tourney.Match.Matrix (MapByMatches (..), MapByRound (..))
import Tourney.SortingNetwork
import Tourney.Stream qualified as TS
import Tourney.VM (VM)
import Tourney.VM qualified as VM

trySE = interactiveTournament (execSteps id singleElimination)

data AppState = AppState
  { tournament :: Tournament TMany
  , codeSoFar :: VM.Code
  , matches :: VM.MapByRound (VM.MapByMatches (Maybe Result))
  , pureMatches :: VM.MapByRound (VM.MapByMatches (Maybe Result))
  , pureCode :: VM.Code
  , standingsHistory :: VM.MapByRound VM.StandingsUpdate
  , vm :: !VM
  , playerCount :: !PlayerCount
  , colours :: !(Array Player (Colour.Colour Double))
  }
  deriving stock (Generic)

data AppEvent
  = AddResult MatchResult
  | AdvanceRound
  | Quit
  | Noop
  deriving stock (Eq, Show, Ord)

interactiveTournament :: Tournament TMany -> IO AppState
interactiveTournament t = do
  let !playerCount = 8
  vm <- VM.setup t playerCount
  pureCode <- VM.peekCode vm
  defaultMain
    app
    AppState
      { tournament = t
      , matches = Empty
      , pureMatches = TS.pureMatchesByRound t playerCount
      , standingsHistory = Empty
      , codeSoFar = mempty
      , pureCode
      , playerCount
      , vm
      , colours = makeMatchColours playerCount
      }
  where
    app :: App AppState AppEvent AppElement
    app =
      App
        { appDraw = \s -> [drawMain `runReader` s]
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleBrickEvent
        , appStartEvent = setupApp
        , appAttrMap = \_ -> attrMap I.defAttr []
        }

handleBrickEvent :: BrickEvent AppElement AppEvent -> EventM AppElement AppState ()
handleBrickEvent = \case
  VtyEvent (EvKey key mods) -> case (key, mods) of
    (KChar 'c', [MCtrl]) -> halt
    (KChar 'q', []) -> halt
    (KChar ' ', []) -> advanceRound
    (KLeft, []) -> hScrollBy (viewportScroll Tournament) (-6)
    (KRight, []) -> hScrollBy (viewportScroll Tournament) 6
    _ -> pure ()
  MouseDown _ BScrollUp _ _ ->
    hScrollBy (viewportScroll Tournament) (-6)
  MouseDown _ BScrollDown _ _ ->
    hScrollBy (viewportScroll Tournament) 6
  MouseDown (ScrollBar el Tournament) BLeft _ _ -> mapM_ (hScrollBy (viewportScroll Tournament)) case el of
    SBHandleBefore -> Just (-6)
    SBHandleAfter -> Just 6
    SBTroughBefore -> Just (-12)
    SBTroughAfter -> Just 12
    _ -> Nothing
  _ -> pure ()

advanceRound :: EventM AppElement AppState ()
advanceRound = do
  vm <- use #vm
  _ <- liftIO (VM.loop vm)
  #matches <~ liftIO (VM.getMatches vm)
  #codeSoFar <~ liftIO (VM.getCodeSoFar vm)
  #standingsHistory <~ liftIO (VM.getStandingsHistory vm)

setupApp :: EventM AppElement AppState ()
setupApp = do
  -- Enable mouse support
  vty <- getVtyHandle
  liftIO (Vty.setMode (Vty.outputIface vty) Vty.Mouse True)

--------------------------------------------------------------------------------
-- UI types

data AppElement
  = Tournament
  | CodeStream
  | Panel
  | ScrollBar ClickableScrollbarElement AppElement
  deriving stock (Show, Eq, Ord)

type UIElement = Reader AppState (Widget AppElement)

drawMain :: UIElement
drawMain = do
  panel <- drawPanel
  tournament <- drawTournament
  pure (tournament <=> panel)

drawPanel :: UIElement
drawPanel = do
  pure $!
    txtWrap "fart"
      & padAll 1
      & borderWithLabel (txt " journeyman ")
      & withBorderStyle unicodeRounded

drawTournament :: UIElement
drawTournament = do
  roundsPure <- view #pureMatches
  rounds <- view #matches
  let displayRounds = alignWith alignRound (toList roundsPure) (toList rounds)
  standings <- fromMaybe mempty <$> preview (#standingsHistory . traverseMax . #standings)
  roundsRendered <- mapM (\(isReal, r) -> drawRound isReal r standings) displayRounds
  pure (hBox roundsRendered & padAll 1 & borderWithLabel (txt " tournament ") & withBorderStyle unicodeRounded)
  where
    alignRound (These _ r) = (True, r)
    alignRound (This p) = (False, p)
    alignRound (That r) = (True, r)

-- pure $!
--   mapM drawRound rounds
--     & hBox

-- \$ toList (s ^. #rounds)
--   ^.. each . _2

drawRound :: Bool -> MapByMatches (Maybe Result) -> Vector (Points, Player) -> UIElement
drawRound isReal matches standings = do
  colours <- view #colours
  pc <- view #playerCount
  let
    dim =
      if isReal
        then I.linearColor @Word8 100 100 100
        else I.linearColor @Word8 100 0 0

    -- compute count as a maximum over the actual players in order to compensate
    -- for possible goofs in calculating player numbers
    !count = asInt (getMax (ifoldMap (\(Match _ b) _ -> Max b) matches)) & max pc
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
    drawMatchColumn (Match low high) =
      let !playerLow = fromMaybe (coerce low) (standings ^? ix (asInt low) . _2)
          !playerHigh = fromMaybe (coerce high) (standings ^? ix (asInt high) . _2)
      in  I.horizCat
            [ horizLines
            , I.vertCat
                [ if
                  | low < m && m < high -> I.string attr (replicate width ' ')
                  | m == low -> I.string attr (pad (show m))
                  | m == high -> I.string attr (pad (show m))
                  | otherwise -> I.string (I.defAttr `I.withForeColor` dim) (replicate width (bsHorizontal unicode))
                | m <- 0 ..< Slot count
                , let !attr = matchAttr (low, playerLow) (high, playerHigh) m
                ]
            ]
    matchAttr = getMatchAttr pc colours
    pad x = replicate (width - length x) ' ' ++ x
  pure $
    raw $
      I.horizCat
        [ vertLine False
        , I.horizCat (map (drawMatchColumn . fst) (itoList matches))
        , horizLines
        , vertLine True
        ]

--------------------------------------------------------------------------------
-- Terminal attributes

makeMatchColours :: PlayerCount -> Array Player (Colour.Colour Double)
makeMatchColours count =
  listArray (0, Player (count - 1)) (take count (cycle knownColours))
  where
    knownColours = Colour.brewerSet Colour.Paired 12

toVtyRGB :: Colour.Colour Double -> Vty.Color
toVtyRGB c = case Colour.toSRGB24 c of
  Colour.RGB r g b -> I.linearColor r g b

getMatchAttr
  :: PlayerCount
  -> Array Player (Colour.Colour Double)
  -> (Slot, Player)
  -> (Slot, Player)
  -> Slot
  -> I.Attr
getMatchAttr pc colours (slow, low) (shigh, high) actual =
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
    !lowColour = colours ^?! ix low
    !highColour = colours ^?! ix high
    !dist = fromIntegral (shigh - actual) / fromIntegral (shigh - slow)
    !isBye = slow < 0 || slow > Slot pc
