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
import Brick.Widgets.Dialog
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Brick.Widgets.Table (renderTable, surroundingBorder, table)
import Data.Align (alignWith)
import Data.Colour qualified as Colour
import Data.Colour.CIE qualified as Colour
import Data.Colour.Names qualified as Colour
import Data.Colour.Palette.BrewerSet qualified as Colour
import Data.Colour.SRGB qualified as Colour
import Data.Text qualified as T
import Data.These
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Graphics.Vty.Attributes qualified as I
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events
import Tourney.Algebra (Depth (..), Tournament, execSteps)
import Tourney.Common
import Tourney.Format.DoubleElimination
import Tourney.Format.ICantBelieveItCanSort
import Tourney.Format.InsertionSort
import Tourney.Format.OptimalSortingNetwork
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
  , ("Optimal Sorting Network", execSteps id optimalSortingNetwork)
  , ("Insertion Sort (Naiive)", execSteps id insertionSortNaiive)
  , ("Insertion Sort", execSteps id insertionSort)
  , ("ICan'tBelieveItCanSort", execSteps id iCan'tBelieveItCanSort)
  ]

knownTournamentsLen :: Int
knownTournamentsLen = length knownTournaments

data MenuForm = MenuForm
  { tournament :: !Int
  , playerCount :: !Text
  }
  deriving stock (Generic)

data AppState = AppState
  { menu :: Form MenuForm AppEvent AppResourceName
  , dialog :: Dialog DialogChoice AppResourceName
  , state :: !(Maybe TournamentState)
  , errors :: [Text]
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

data MatchForm = MatchForm
  { score1 :: !Text
  , score2 :: !Text
  }
  deriving stock (Generic)

data AppResourceName
  = Tournament
  | CodeStream
  | EventStream
  | Panel
  | ScrollBar ClickableScrollbarElement AppResourceName
  | MenuTournamentItem !Int
  | MenuPlayerCountItem
  | MenuStandingsItem
  | MenuEnter
  | Score1
  | Score2
  deriving stock (Show, Eq, Ord)

type UIElement = Reader TournamentState (Widget AppResourceName)

data AppEvent deriving stock (Eq, Show, Ord)

main :: IO AppState
main =
  defaultMain
    app
    AppState
      { menu = menuForm MenuForm{playerCount = "8", tournament = 0}
      , dialog = menuDialog
      , state = Nothing
      , errors = []
      }
  where
    app :: App AppState AppEvent AppResourceName
    app =
      App
        { appDraw = \s ->
            let
              errorsLayers = drawErrors (s ^. #errors)
              menuLayer = [drawMenu `runReader` s | isNothing (s ^. #state)]
              mainLayer = foldMap (runReader drawMain) (s ^. #state)
            in
              errorsLayers ++ menuLayer ++ mainLayer
        , appChooseCursor = \s c ->
            case s ^. #state of
              Nothing -> focusRingCursor formFocus (s ^. #menu) c
              Just st -> case st ^. #matchForm of
                Just mf -> focusRingCursor formFocus mf c
                Nothing -> showFirstCursor s c
        , appHandleEvent = \ev -> do
            case ev of
              VtyEvent (EvKey (KChar 'q') _) -> halt
              VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt
              _ -> do
                s <- use #state
                case s of
                  Nothing -> do
                    case ev of
                      VtyEvent vev -> Brick.zoom #dialog (handleDialogEvent vev)
                      _ -> pure ()

                    case ev of
                      VtyEvent (EvKey KEnter _) -> beginTournament
                      _ -> Brick.zoom #menu (handleFormEvent ev)
                  Just _ -> Brick.zoom (#state . _Just) (handleTournamentEvent ev)
        , appStartEvent = do
            -- Enable mouse support
            vty <- getVtyHandle
            liftIO (Vty.setMode (Vty.outputIface vty) Vty.Mouse True)
        , appAttrMap = \_ ->
            attrMap
              I.defAttr
              [ (codeRealAttr, style I.bold)
              , (codePureAttr, fg dimRed)
              , (winPlayerAttr, fg I.green `I.withStyle` I.bold)
              , (lostPlayerAttr, fg I.red `I.withStyle` I.bold)
              , (helpAttr, I.defAttr)
              , (helpKeyAttr, fg I.red `I.withStyle` I.bold)
              , (stepCodeAttr, fg I.green `I.withStyle` I.bold)
              , (editAttr, I.white `Brick.on` I.black)
              , (editFocusedAttr, I.black `Brick.on` I.yellow)
              , (invalidFormInputAttr, I.white `Brick.on` I.red)
              , (focusedFormInputAttr, I.black `Brick.on` I.yellow)
              , (dialogAttr, I.white `Brick.on` I.blue)
              , (buttonAttr, I.black `Brick.on` I.white)
              , (buttonSelectedAttr, bg I.yellow)
              , (rowOddAttr, I.white `Brick.on` I.brightBlack)
              , (rowEvenAttr, I.black `Brick.on` I.white)
              ]
        }

--------------------------------------------------------------------------------
-- Main menu

data DialogChoice = DialogEnter

menuDialog :: Dialog DialogChoice AppResourceName
menuDialog =
  dialog
    (Just (txt " journeyman ui "))
    (Just (MenuEnter, [("Start", MenuEnter, DialogEnter)]))
    35

menuForm :: MenuForm -> Form MenuForm AppEvent AppResourceName
menuForm =
  newForm
    [ (txt "Player count: " <+>)
        @@= editTextField #playerCount MenuPlayerCountItem (Just 1)
    , ((txt "Tournament type: " <=>) >>> padTop (Pad 1))
        @@= radioField
          #tournament
          [ (i, MenuTournamentItem i, knownTournaments ^?! ix i . _1)
          | i <- [0 .. knownTournamentsLen - 1]
          ]
    ]
    >>> setFormConcat (vBox >>> padAll 1)

drawMenu :: Reader AppState (Widget AppResourceName)
drawMenu = do
  form <- view #menu
  dial <- view #dialog
  pure (renderDialog dial (renderForm form))

beginTournament :: EventM AppResourceName AppState ()
beginTournament = do
  mcount <- uses (#menu . to formState . #playerCount) (readEither @PlayerCount . toString)
  tournIx <- use (#menu . to formState . #tournament)
  let tournament = knownTournaments ^?! ix tournIx . _2
  case mcount of
    Left err -> #errors %= (err :)
    Right count -> do
      #state <~ liftIO (Just <$> createTournamentState count tournament)

--------------------------------------------------------------------------------
-- Errors layer

drawErrors :: [Text] -> [Widget AppResourceName]
drawErrors errs =
  [ txtWrap err & borderWithLabel (txt " error ") & withBorderStyle unicodeRounded
  | err <- errs
  ]

--------------------------------------------------------------------------------

handleTournamentEvent :: BrickEvent AppResourceName AppEvent -> EventM AppResourceName TournamentState ()
handleTournamentEvent ev = do
  didHandleEvent <- newIORef True
  case ev of
    VtyEvent (EvKey key mods) -> case (key, mods) of
      (KChar 'a', []) -> advanceRound
      (KChar 'h', []) -> #tournamentViewportScroll . _1 += 2
      (KChar 'j', []) -> #tournamentViewportScroll . _2 += 2
      (KChar 'k', []) -> #tournamentViewportScroll . _2 -= 2
      (KChar 'l', []) -> #tournamentViewportScroll . _1 -= 2
      (KChar '1', []) -> quickAssignResult (Result 1 0)
      (KChar '2', []) -> quickAssignResult (Result 0 1)
      (KEnter, []) -> do
        mf <- use #matchForm
        case mf of
          Nothing -> #matchForm <~ fmap (fmap matchForm) createMatchForm
          Just _ -> acceptMatchFormEntry
      (KLeft, []) -> #selection %= Sel.moveLeft
      (KRight, []) -> #selection %= Sel.moveRight
      (KEsc, []) -> #matchForm .= Nothing
      _ -> writeIORef didHandleEvent False
    MouseDown Tournament BScrollUp _ _ ->
      hScrollBy (viewportScroll Tournament) (-6)
    MouseDown Tournament BScrollDown _ _ ->
      hScrollBy (viewportScroll Tournament) 6
    MouseDown CodeStream BScrollUp _ _ ->
      vScrollBy (viewportScroll CodeStream) (-6)
    MouseDown CodeStream BScrollDown _ _ ->
      vScrollBy (viewportScroll CodeStream) 6
    MouseDown EventStream BScrollUp _ _ ->
      vScrollBy (viewportScroll EventStream) (-6)
    MouseDown EventStream BScrollDown _ _ ->
      vScrollBy (viewportScroll EventStream) 6
    MouseDown (ScrollBar el Tournament) BLeft _ _ ->
      mapM_ (hScrollBy (viewportScroll Tournament)) case el of
        SBHandleBefore -> Just (-6)
        SBHandleAfter -> Just 6
        SBTroughBefore -> Just (-12)
        SBTroughAfter -> Just 12
        _ -> Nothing
    _ -> writeIORef didHandleEvent False
  ok <- readIORef didHandleEvent
  unless ok (Brick.zoom (#matchForm . _Just) (handleFormEvent ev))

createTournamentState :: PlayerCount -> Tournament TMany -> IO TournamentState
createTournamentState playerCount t = do
  vm <- VM.setup t playerCount
  pureCode <- VM.peekCode vm
  let !pureMatches = TS.pureMatchesByRound t playerCount
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
  #selection %= Sel.moveRight

quickAssignResult :: Result -> EventM AppResourceName TournamentState ()
quickAssignResult result = void $ runMaybeT do
  vm <- use #vm
  (curRd, curMatch) <- hoistMaybe =<< uses #selection Sel.current
  Just Nothing <- liftIO (VM.getMatch vm curRd curMatch)
  ok <- liftIO (VM.setMatchResult vm curRd curMatch result)
  when ok do
    #matchForm .= Nothing
    #matches <~ liftIO (VM.getMatches vm)
    #selection %= Sel.moveRight
    pure ()

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
    pure () -- not necessary, but my syntax highlighting breaks without it

parsePoints :: Text -> MaybeT (EventM AppResourceName s) Points
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

drawMain :: Reader TournamentState [Widget AppResourceName]
drawMain = do
  tournament <- drawTournament
  code <- drawCode
  pendings <- drawPendings
  help <- drawHelp
  match <- drawSelectedMatch
  events <- drawEvents
  pure
    ( maybeToList match
        ++ [ hBox
              [ vBox [tournament, pendings]
              , vBorder
              , vBox
                  [ hBorderWithLabel (txt " Compiled Tournament ")
                  , code
                  , hBorderWithLabel (txt " Events ")
                  , events
                  , hBorder
                  , help
                  ]
                  & hLimit 40
              ]
           ]
    )

niceBorder :: Widget a -> Widget a
niceBorder = joinBorders . withBorderStyle unicodeRounded . border

drawCode :: UIElement
drawCode = do
  code <- view #codeSoFar
  pureCode <- view #pureCode
  renderedCode <- V.sequence (alignWith (pure . drawCodeLine) code pureCode)
  pure
    ( vBox (toList renderedCode)
        & padLeftRight 1
        & viewport CodeStream Vertical
        & withVScrollBars OnRight
    )
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
  --  let !lastEvents = V.drop (max 0 (len - 15)) events
  pure $
    vBox
      [ case ev of
        VM.Stepped e ->
          (if isOdd then rowOddAttr else rowEvenAttr)
            `withAttr` txtWrap (show e)
        _ -> txt "Finished"
      | (isOdd, ev) <- zip (cycle [False, True]) (toList events)
      ]
      & viewport EventStream Vertical
      & withVScrollBars OnRight

drawPendings :: UIElement
drawPendings = do
  matches <- view #pendingMatches
  matrix <- view #matches
  roundNo <- view #roundNo
  let drawPendingMatch m@(Match a b) =
        let
          result = join (matrix ^. at roundNo . non' _Empty . at m)
          highlight (<>?) = case result of
            Just (Result ra rb)
              | ra <>? rb -> withAttr winPlayerAttr
              | otherwise -> withAttr lostPlayerAttr
            _ -> id
        in
          hBox [txt (show a) & highlight (>), txt " vs ", txt (show b) & highlight (<)]
            & padLeftRight 1
      forceLen2 [] = [emptyWidget, emptyWidget]
      forceLen2 [a] = [a, emptyWidget]
      forceLen2 [a, b] = [a, b]
      forceLen2 _ = []
      pendings = map drawPendingMatch (toList matches) & chunksOf 2 & map forceLen2 & transpose
      pendingTable = table pendings & surroundingBorder False
  pure $ hBorderWithLabel (txt " Pending Matches ") <=> (renderTable pendingTable & hCenter & padTopBottom 1)

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

matchForm :: MatchForm -> Form MatchForm AppEvent AppResourceName
matchForm =
  newForm
    [ editTextField #score1 Score1 (Just 1) >>> setFieldConcat \ws -> hBox (txt "Score 1: " : ws)
    , editTextField #score2 Score2 (Just 1) >>> setFieldConcat \ws -> hBox (txt "Score 2: " : ws)
    ]
    >>> setFormConcat (vBox >>> vLimit 2 >>> padLeftRight 1)

drawHelp :: UIElement
drawHelp = do
  sel <- views #selection Sel.current
  pure $
    vBox
      [ helpText "a|dvance tournament"
      , case sel of
          Just (_, Match a _) -> helpText [fmt||1|: set {a:s} as winner|]
          Nothing -> helpText "set |1| as winner"
      , case sel of
          Just (_, Match _ b) -> helpText [fmt||2|: set {b:s} as winner|]
          Nothing -> helpText "set |2| as winner"
      , helpText "<Enter>| set match result manually"
      , helpText "<Left>| select left"
      , helpText "<Right>| select right"
      , helpText "hjkl| scroll viewport"
      , helpText "q|uit"
      ]
      & padLeftRight 1

helpText :: Text -> Widget n
helpText (T.split (== '|') -> ts) = case ts of
  [a, k, b] -> hBox (map applyHelpAttr [Left a, Right k, Left b])
  [a, b] -> hBox (map applyHelpAttr [Right a, Left b])
  [a] -> applyHelpAttr (Right a)
  _ -> emptyWidget
  where
    applyHelpAttr (Left a) = txt a & withAttr helpAttr
    applyHelpAttr (Right a) = txt a & withAttr helpKeyAttr

drawTournament :: UIElement
drawTournament = do
  roundsPure <- view #pureMatches
  rounds <- view #matches
  let displayRounds = alignWith alignRound (itoList roundsPure) (itoList rounds)
  sel <- views #selection Sel.current
  (x, y) <- view #tournamentViewportScroll
  history <- view #standingsHistory
  roundsRendered <- forM displayRounds \(isReal, (roundNo, matches)) ->
    let prevRound = history ^. ix (roundNo - 1) . #standings
        selMatch = do
          (r, m) <- sel
          guard (r == roundNo)
          pure m
    in  drawRound roundNo selMatch isReal matches prevRound
  let
    standingsBefore = history ^. traverseMin . #standings
    standingsAfter = history ^. traverseMax . #standings
    displayStandings = I.vertCat . (I.string I.defAttr " " :) . map (I.string I.defAttr . show . snd) . toList
    before = displayStandings standingsBefore
    after = displayStandings standingsAfter
  pure $
    raw (before I.<|> I.horizCat roundsRendered I.<|> after)
      & center
      & translateBy (Location (x, y))
  where
    alignRound = \case
      These _ r -> (True, r)
      This p -> (False, p)
      That r -> (True, r)

    drawRound (RoundNo roundNo) selectedMatch isReal matches standings = do
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
      pure (I.string (I.defAttr `I.withForeColor` bga) ('R' : show roundNo) I.<-> img)

--------------------------------------------------------------------------------
-- Terminal attributes

codeRealAttr
  , codePureAttr
  , helpAttr
  , helpKeyAttr
  , stepCodeAttr
  , lostPlayerAttr
  , winPlayerAttr
  , rowOddAttr
  , rowEvenAttr
    :: AttrName
codeRealAttr = attrName "code_real"
codePureAttr = attrName "code_pure"
stepCodeAttr = attrName "step_code"
helpAttr = attrName "help"
helpKeyAttr = attrName "help_key"
lostPlayerAttr = attrName "lost"
winPlayerAttr = attrName "win"
rowOddAttr = attrName "oddrow"
rowEvenAttr = attrName "evenrow"

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
    textColour, midColour :: Colour.Colour Double
    !textColour
      | isBye = Colour.grey
      | Colour.luminance midColour > 0.3 = Colour.black
      | otherwise = Colour.white
    !midColour =
      ( case (lowColour, highColour) of
          (Just l, Just h) -> Colour.blend (sqrt dist) l h
          _ -> fromMaybe Colour.white (lowColour <|> highColour)
      )
        & Colour.darken (if isBye then 0.01 else 1.0)

    lowColour, highColour :: Maybe (Colour.Colour Double)
    !lowColour = colours ^? ix (Player (maybe (asInt slow) asInt low))
    !highColour = colours ^? ix (Player (maybe (asInt shigh) asInt high))
    !dist = fromIntegral (shigh - actual) / fromIntegral (shigh - slow)
    !isBye = slow < 0 || slow > Slot pc

--------------------------------------------------------------------------------
-- Internals

-- |
--
-- chunksOf function taken from the "split" package by Brent Yorgey 2008-2023
-- licensed BSD-3
--
-- Copyright (c) 2008 Brent Yorgey, Louis Wasserman
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of other contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

    build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
    build g = g (:) []
