module Tourney.UI.Main where

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
import Data.Vector (Vector)
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

--------------------------------------------------------------------------------
-- UI elements

data UIConfig = UIConfig
  { playerColours :: !(Array Player (Colour.Colour Double))
  , playerCount :: !PlayerCount
  }

data AppElement
  = Tournament
  | Panel

type UIElement = Reader UIConfig (Brick.Widget AppElement)

drawPanel :: UIElement
drawPanel = undefined

drawTournament :: Vector (Vector (Match, Maybe Result)) -> UIElement
drawTournament rounds = undefined

drawRound :: Vector (Match, Maybe Result) -> UIElement
drawRound matches = do
  colours <- asks playerColours
  pc <- asks playerCount
  let
    !count = maximumOf (each . _1 . likelyLoser) matches & maybe pc (+ 1) & max pc
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
      I.horizCat
        [ horizLines
        , I.vertCat
            [ if
              | low < m && m < high -> I.string (matchAttr low high m) (replicate width ' ')
              | m == low -> I.string (matchAttr low high m) (pad (show m))
              | m == high -> I.string (matchAttr low high m) (pad (show m))
              | otherwise -> I.string (I.defAttr `I.withForeColor` dim) (replicate width (bsHorizontal unicode))
            | m <- [0 .. count - 1]
            ]
        ]
    matchAttr = getMatchAttr colours
    pad x = replicate (width - length x) ' ' ++ x
  pure $
    Brick.raw $
      I.horizCat
        [ vertLine False
        , I.horizCat (map (drawMatchColumn . fst) (toList matches))
        , horizLines
        , vertLine True
        ]

--------------------------------------------------------------------------------
-- Terminal attributes

dim :: I.Color
dim = I.linearColor @Word8 50 50 50

makeMatchColours :: PlayerCount -> Vector (Colour.Colour Double)
makeMatchColours count = V.fromList (take count (cycle (Colour.brewerSet Colour.Paired 12)))

toVtyRGB :: Colour.Colour Double -> Vty.Color
toVtyRGB c = case Colour.toSRGB24 c of
  Colour.RGB r g b -> I.linearColor r g b

getMatchAttr :: Array Player (Colour.Colour Double) -> Player -> Player -> Player -> I.Attr
getMatchAttr colours low high actual =
  I.defAttr
    `I.withBackColor` toVtyRGB midColour
    `I.withForeColor` toVtyRGB textColour
    `I.withStyle` I.bold
  where
    (_, pc) = arrayBounds colours
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
    !dist = fromIntegral (high - actual) / fromIntegral (high - low)
    !isBye = not (low >= 0 && high < pc)
