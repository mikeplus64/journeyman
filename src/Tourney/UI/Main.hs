{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Tourney.UI.Main where

import Brick hiding (Max, Result, zoom)
import Brick qualified
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Align (alignWith)
import Data.Char (isNumber)
import Data.Colour qualified as Colour
import Data.Colour.CIE qualified as Colour
import Data.Colour.Names qualified as Colour
import Data.Colour.Palette.BrewerSet qualified as Colour
import Data.Colour.SRGB qualified as Colour
import Data.These
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as I
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events
import Text.Pretty.Simple qualified as Pretty
import Tourney.Algebra (Depth (..), Tournament, execSteps)
import Tourney.Common
import Tourney.Format.DoubleElimination
import Tourney.Format.RoundRobin
import Tourney.Format.SingleElimination
import Tourney.Match
import Tourney.Stream qualified as TS
import Tourney.UI.Selection qualified as Sel
import Tourney.VM (VM)
import Tourney.VM qualified as VM
import Tourney.VM.Code (TourneyOp (..))

knownTournaments :: [(Text, Tournament TMany)]
knownTournaments =
  [ ("Single Elimination", execSteps id singleElimination)
  , ("Double Elimination", execSteps id doubleElimination)
  , ("Round Robin", execSteps id roundRobin)
  , ("Round Robin (2 groups)", execSteps id (groupRoundRobin 2))
  , ("Round Robin (4 groups)", execSteps id (groupRoundRobin 4))
  ]

data AppState = AppState
  { tournamentIndex :: !Int
  , playerCount :: !PlayerCount
  , initialStandings :: !Standings
  , state :: !(Maybe TournamentState)
  }
  deriving stock (Generic)

data TournamentState = TournamentState
  { tournament :: Tournament TMany
  , selection :: !Sel.Selection
  , matches :: VM.MapByRound (VM.MapByMatches (Maybe Result))
  , roundNo :: !RoundNo
  , pendingMatches :: Vector Match
  , pureMatches :: VM.MapByRound (VM.MapByMatches (Maybe Result))
  , pureCode :: VM.Code
  , codeSoFar :: Vector (TourneyOp, VM.StepCodeEvent)
  , rawEvents :: Vector VM.StepCodeEvent
  , standingsHistory :: VM.MapByRound VM.StandingsUpdate
  , vm :: !VM
  , playerCount :: !PlayerCount
  , colours :: !(Array Player (Colour.Colour Double))
  , tournamentViewportScroll :: !(Int, Int)
  , matchForm :: Maybe (Form MatchForm AppEvent AppResourceName)
  }
  deriving stock (Generic)

data AppEvent deriving stock (Eq, Show, Ord)

main :: IO AppState
main =
  defaultMain
    app
    AppState
      { tournamentIndex = 0
      , playerCount = 8
      , initialStandings = createInitialStandings 8
      , state = Nothing
      }
  where
    app :: App AppState AppEvent AppResourceName
    app =
      App
        { appDraw = \s ->
            concat
              [ [drawMenu `runReader` s | isNothing (s ^. #state)]
              , [drawMain `runReader` ts | Just ts <- [s ^. #state]]
              ]
        , appChooseCursor = \s c ->
            case s ^? #state . _Just . #matchForm . _Just of
              Just mf -> focusRingCursor formFocus mf c
              Nothing -> showFirstCursor s c
        , appHandleEvent = \ev -> Brick.zoom (#state . _Just) (handleEvent ev)
        , appStartEvent = do
            -- Enable mouse support
            vty <- getVtyHandle
            liftIO (Vty.setMode (Vty.outputIface vty) Vty.Mouse True)
        , appAttrMap = \_ ->
            attrMap
              I.defAttr
              [ (codeRealAttr, style I.bold)
              , (codePureAttr, fg dimRed)
              , (helpAttr, I.defAttr)
              , (helpKeyAttr, fg I.red `I.withStyle` I.bold)
              , (stepCodeAttr, fg I.green `I.withStyle` I.bold)
              ]
        }

--------------------------------------------------------------------------------
-- Main menu

drawMenu :: Reader AppState (Widget n)
drawMenu = do
  pure emptyWidget

--------------------------------------------------------------------------------

createTournamentState :: Tournament TMany -> IO TournamentState
createTournamentState t = do
  let !playerCount = 8
  vm <- VM.setup t playerCount
  pureCode <- VM.peekCode vm
  let pureMatches = TS.pureMatchesByRound t playerCount
  pure
    TournamentState
      { tournament = t
      , selection = Sel.new pureMatches
      , matches = Empty
      , roundNo = 0
      , pureMatches
      , pendingMatches = mempty
      , standingsHistory = Empty
      , codeSoFar = mempty
      , rawEvents = mempty
      , pureCode
      , playerCount
      , vm
      , colours = makeMatchColours playerCount
      , tournamentViewportScroll = (0, 0)
      , matchForm = Nothing
      }

handleEvent :: BrickEvent AppResourceName AppEvent -> EventM AppResourceName TournamentState ()
handleEvent ev = do
  let ch m = lift m >> put True
  didHandleEvent <- executingStateT False $ case ev of
    VtyEvent (EvKey key mods) -> case (key, mods) of
      (KChar 'c', [MCtrl]) -> ch halt
      (KChar 'q', []) -> ch halt
      (KChar 'a', []) -> ch advanceRound
      (KChar 'h', []) -> ch $ #tournamentViewportScroll . _1 -= 1
      (KChar 'j', []) -> ch $ #tournamentViewportScroll . _2 += 1
      (KChar 'k', []) -> ch $ #tournamentViewportScroll . _2 -= 1
      (KChar 'l', []) -> ch $ #tournamentViewportScroll . _1 += 1
      (KEnter, []) -> ch do
        mf <- use #matchForm
        case mf of
          Nothing -> #matchForm <~ fmap (fmap matchForm) createMatchForm
          Just _ -> acceptMatchFormEntry
      (KLeft, []) -> ch $ #selection %= Sel.moveLeft
      (KRight, []) -> ch $ #selection %= Sel.moveRight
      _ -> pure ()
    MouseDown _ BScrollUp _ _ ->
      ch $
        hScrollBy (viewportScroll Tournament) (-6)
    MouseDown _ BScrollDown _ _ ->
      ch $
        hScrollBy (viewportScroll Tournament) 6
    MouseDown (ScrollBar el Tournament) BLeft _ _ ->
      ch $ mapM_ (hScrollBy (viewportScroll Tournament)) case el of
        SBHandleBefore -> Just (-6)
        SBHandleAfter -> Just 6
        SBTroughBefore -> Just (-12)
        SBTroughAfter -> Just 12
        _ -> Nothing
    _ -> pure ()

  unless didHandleEvent (Brick.zoom (#matchForm . _Just) (handleFormEvent ev))

advanceRound :: EventM AppResourceName TournamentState ()
advanceRound = do
  vm <- use #vm
  events <- liftIO (VM.loop vm)
  code <- liftIO (VM.getCodeSoFar vm)
  #rawEvents <>= events
  #codeSoFar %= \soFar -> V.zip code (V.map snd soFar <> events)
  #standingsHistory <~ liftIO (VM.getStandingsHistory vm)
  #pendingMatches <~ liftIO (VM.getPendingMatches vm)
  #roundNo <~ liftIO (VM.getRoundNo vm)
  matches <- #matches <<~ liftIO (VM.getMatches vm)
  #selection %= Sel.merge matches

acceptMatchFormEntry :: EventM AppResourceName TournamentState ()
acceptMatchFormEntry = void $ runMaybeT do
  vm <- use #vm
  (curRd, curMatch) <- hoistMaybe =<< uses #selection Sel.current
  form <- hoistMaybe =<< use #matchForm
  let MatchForm{score1, score2} = form ^. to formState
  s1 <- parsePoints score1
  s2 <- parsePoints score2
  ok <- liftIO (VM.setMatchResult vm curRd curMatch (Result s1 s2))
  when ok do
    #matchForm .= Nothing
    #matches <~ liftIO (VM.getMatches vm)
    #selection %= Sel.moveRight
  where
    parsePoints = hoistMaybe . (readMaybe . toString :: Text -> Maybe Points)

createMatchForm :: EventM AppResourceName TournamentState (Maybe MatchForm)
createMatchForm = runMaybeT do
  (curRd, curMatch) <- hoistMaybe =<< uses #selection Sel.current
  result <- hoistMaybe =<< use (#matches . at curRd . non' _Empty . at curMatch)
  case result of
    Just (Result a b) -> pure (MatchForm (show a) (show b))
    Nothing -> pure (MatchForm "" "")

--------------------------------------------------------------------------------
-- UI types

data AppResourceName
  = Tournament
  | CodeStream
  | Panel
  | ScrollBar ClickableScrollbarElement AppResourceName
  | Score1
  | Score2
  deriving stock (Show, Eq, Ord)

type UIElement = Reader TournamentState (Widget AppResourceName)

drawMain :: Reader TournamentState [Widget AppResourceName]
drawMain = do
  tournament <- drawTournament
  code <- drawCode
  pendings <- drawPendings
  help <- drawHelp
  match <- drawSelectedMatch
  events <- drawEvents
  let main =
        hBox
          [ vBox [tournament, pendings]
          , vBorder
          , vBox
              [ code
              , hBorder
              , events
              , hBorder
              , help
              ]
          ]
  pure (maybeToList match ++ [main])

niceBorder :: Widget a -> Widget a
niceBorder = joinBorders . withBorderStyle unicodeRounded . border

drawCode :: UIElement
drawCode = do
  code <- view #codeSoFar
  pureCode <- view #pureCode
  renderedCode <- V.sequence (alignWith (pure . drawCodeLine) code pureCode)
  pure $!
    vBox (toList renderedCode)
      & padLeftRight 1
      & viewport CodeStream Vertical
      & withVScrollBars OnRight
      & hLimit 60
  where
    drawRan code ev =
      hBox
        [ txt (show code) & withAttr codeRealAttr
        , txt " ==> "
        , txt (show ev) & withAttr stepCodeAttr
        ]
    drawCodeLine = \case
      These (code, ev) _ -> drawRan code ev
      This (code, ev) -> drawRan code ev
      That b -> (txt (show b) & withAttr codePureAttr)

drawEvents :: UIElement
drawEvents = do
  events <- view #rawEvents
  let !len = V.length events
  let !lastEvents = V.drop (max 0 (len - 20)) events
  pure $ vBox [txt (show ev) | ev <- toList lastEvents]

drawPendings :: UIElement
drawPendings = do
  matches <- view #pendingMatches
  matrix <- view #matches
  roundNo <- view #roundNo
  let drawPendingMatch m =
        let result = join (matrix ^. at roundNo . non' _Empty . at m)
        in  case result of
              Just r -> txt (show m) <+> txt " := " <+> txt (show r)
              Nothing -> txt (show m)
  pure $ hBorderWithLabel (txt " Matches ") <=> vBox (map drawPendingMatch (toList matches))

data MatchForm = MatchForm
  { score1 :: !Text
  , score2 :: !Text
  }
  deriving stock (Generic)

drawSelectedMatch :: Reader TournamentState (Maybe (Widget AppResourceName))
drawSelectedMatch = runMaybeT do
  (RoundNo rd, Match (Slot a) (Slot b)) <- hoistMaybe =<< views #selection Sel.current
  form <- hoistMaybe =<< view #matchForm
  pure
    ( renderForm form
        & borderWithLabel (txt [fmt| Enter result for round {rd}, match {a}/{b} |])
        & withBorderStyle unicodeRounded
        & hLimit 40
        & vLimit 10
        & centerLayer
    )

score1Field, score2Field :: MatchForm -> FormFieldState MatchForm AppEvent AppResourceName
score1Field = editTextField #score1 Score1 (Just 1) >>> setFieldConcat \ws -> hBox (txt "Score 1: " : ws)
score2Field = editTextField #score2 Score2 (Just 1) >>> setFieldConcat \ws -> hBox (txt "Score 2: " : ws)

matchForm :: MatchForm -> Form MatchForm AppEvent AppResourceName
matchForm =
  newForm [score1Field, score2Field]
    >>> setFormConcat (vBox >>> vLimit 2 >>> padLeftRight 1)

-- borderWithLabel (txt [fmt| round - {rd} |]) (txt "asdf")

drawHelp :: UIElement
drawHelp =
  pure $
    vBox
      [ helpText "" "a" "dvance tournament"
      , helpText "" "<Enter>" " match"
      , helpText "" "<Left>" " select left"
      , helpText "" "<Right>" " select right"
      , helpText "" "hjkl" " scroll viewport"
      , helpText "" "q" "uit"
      ]
      & padLeftRight 1
  where
    helpText a k b = hBox (map applyHelpAttr [Left a, Right k, Left b])
    applyHelpAttr (Left a) = txt a & withAttr helpAttr
    applyHelpAttr (Right a) = txt a & withAttr helpKeyAttr

drawTournament :: UIElement
drawTournament = do
  count <- view #playerCount
  roundsPure <- view #pureMatches
  rounds <- view #matches
  let displayRounds = alignWith alignRound (itoList roundsPure) (itoList rounds)
  sel <- views #selection Sel.current
  (x, y) <- view #tournamentViewportScroll
  history <- view #standingsHistory
  roundsRendered <- forM displayRounds \(isReal, (roundNo, matches)) ->
    let prevRound = history ^. ix (roundNo - 1) . #standings
        selMatch = fmap snd sel
    in  drawRound selMatch isReal matches prevRound
  let
    standingsBefore = history ^. traverseMin . #standings
    standingsAfter = history ^. traverseMax . #standings
    displayStandings = I.vertCat . map (I.string I.defAttr . show . snd) . toList
    before = displayStandings standingsBefore
    after = displayStandings standingsAfter
  pure $
    (before I.<|> I.horizCat roundsRendered I.<|> after)
      & raw
      & center
      & translateBy (Location (x, y))
  where
    alignRound = \case
      These _ r -> (True, r)
      This p -> (False, p)
      That r -> (True, r)

    drawRound selectedMatch isReal matches standings = do
      colours <- view #colours
      pc <- view #playerCount
      let
        bga = if isReal then dim else dimRed
        -- compute count as a maximum over the actual players in order to
        -- compensate for possible goofs in calculating player numbers
        !count = asInt (getMax (ifoldMap (\(Match _ b) _ -> Max b) matches)) & max pc
        !width = max 2 (length ('S' : show count :: String))
        vertLine right =
          I.vertCat
            . map (\f -> I.char (I.defAttr `I.withForeColor` bga) (f unicodeRounded))
            . concat
            $ [ [if right then bsCornerTR else bsCornerTL]
              , replicate (count - 2) bsVertical
              , [if right then bsCornerBR else bsCornerBL]
              ]
        horizLines = I.vertCat (replicate count (I.char (I.defAttr `I.withForeColor` bga) (bsHorizontal unicodeRounded)))
        drawMatchColumn match@(Match low high) =
          let
            !playerLow = standings ^? ix (asInt low) . _2
            !playerHigh = standings ^? ix (asInt high) . _2
            !selected = Just match == selectedMatch
          in
            I.horizCat
              [ horizLines
              , I.vertCat $
                  [ if
                    | low < m && m < high -> I.string attr (replicate width ' ')
                    | m == low -> I.string attr (pad (maybe (show m) show playerLow))
                    | m == high -> I.string attr (pad (maybe (show m) show playerHigh))
                    | otherwise -> I.string (I.defAttr `I.withForeColor` bga) (replicate width (bsHorizontal unicode))
                  | m <- 0 ..< Slot count
                  , let !attr = matchAttr (low, playerLow) (high, playerHigh) m
                  ]
                    ++ if selected
                      then map (I.string I.defAttr) ("\x1fb6f" : replicate 3 "\x2502")
                      else []
              ]
        matchAttr = getMatchAttr pc colours
        pad x = replicate (width - length x) ' ' ++ x
      let img =
            I.horizCat
              [ vertLine False
              , I.horizCat (map (drawMatchColumn . fst) (itoList matches))
              , horizLines
              , vertLine True
              ]
      pure img

--------------------------------------------------------------------------------
-- Terminal attributes

codeRealAttr, codePureAttr, helpAttr, helpKeyAttr, stepCodeAttr :: AttrName
codeRealAttr = attrName "code_real"
codePureAttr = attrName "code_pure"
stepCodeAttr = attrName "step_code"
helpAttr = attrName "help"
helpKeyAttr = attrName "help_key"

dim, dimRed :: I.Color
dim = I.linearColor @Word8 100 100 100
dimRed = I.linearColor @Word8 50 50 50

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
  -> (Slot, Maybe Player)
  -> (Slot, Maybe Player)
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
    !lowColour = colours ^?! ix (Player (maybe (asInt slow) asInt low))
    !highColour = colours ^?! ix (Player (maybe (asInt shigh) asInt high))
    !dist = fromIntegral (shigh - actual) / fromIntegral (shigh - slow)
    !isBye = slow < 0 || slow > Slot pc
