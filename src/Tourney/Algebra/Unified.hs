{-# LANGUAGE PartialTypeSignatures #-}

module Tourney.Algebra.Unified where

import Control.Lens hiding (Empty)
import Data.Default
import Data.Dependency (StreamM (..))
import Data.Dependency qualified as S
import Data.Function.Known (type (~>))
import Data.Generics.Labels ()
import Data.These
import Data.Typeable (cast)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Text.Show qualified as Show
import Tourney.Match
import Tourney.Types
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB

data Focus = Focus {focusStart, focusLength :: Int}
  deriving stock (Show, Eq, Generic)

-- | A description of a tournament. The type-parameter gives an upper-bound on
-- how many rounds this tournament has. It also ensures that the "spine" of a
-- tournament is static, regardless of use of constructors such as
-- 'ByStandings', that enable creating tournaments that depend on match results
-- that are not yet known.
data Tournament :: Depth -> Type where
  One :: !Match -> Tournament 'TOne
  Empty :: Tournament t
  -- | Modify a tournament by some 'Mod'
  Modify :: !Mod -> Tournament t -> Tournament t
  -- | Overlay two tournaments, to describe running two sub-tournaments in
  -- parallel. The depth of the tournaments must be the same
  Overlay :: !(Tournament a) -> !(Tournament a) -> Tournament a
  -- | Sequence two tournaments one after the other. The resulting tournament
  -- has a depth that is the sum of the two depths of the input tournaments.
  Sequence :: (KnownDepth a, KnownDepth b) => !(Tournament a) -> !(Tournament b) -> Tournament TMany
  LiftTMany :: KnownDepth t => Tournament t -> Tournament TMany
  -- | Depend on the player count to produce an inner tournament
  ByPlayerCount :: !(PlayerCount -> Tournament t) -> Tournament t
  -- | Depend on the current focus of the tournament to produce an inner
  -- tournament
  ByFocus :: !(Focus -> Tournament t) -> Tournament t
  -- | Depend on the current standings, at the outset of the current round, to
  -- run the tournament.
  ByStandings :: !(Standings -> Tournament t) -> Tournament t

instance Monoid (Tournament t) where mempty = Empty
instance Semigroup (Tournament t) where (<>) = (+++)

instance Show (Tournament t) where
  showsPrec p = \case
    Sequence a b -> Show.showParen True (Show.showsPrec 9 a . Show.showString " *** " . Show.showsPrec 9 b)
    Overlay a b -> Show.showParen True (Show.showsPrec 9 a . Show.showString " +++ " . Show.showsPrec 9 b)
    One a -> Show.showsPrec p a
    Empty -> Show.showString "Empty"
    LiftTMany t -> Show.showParen True (Show.showString "Lt " . Show.showsPrec 9 t)
    _ -> Show.showString "_"

data SortMethod
  = WinnerTakesHigh
  | -- | Award points and use that to sort the results in the end
    PointsAward (MatchResult ~> MatchResult)
  deriving stock (Show, Eq)

instance Default SortMethod where
  def = WinnerTakesHigh

data Sorter = Sorter {sorterFocus :: !Focus, sorterMethod :: !SortMethod}
  deriving stock (Show, Eq, Generic)

data Mod
  = SetFocus !(Focus -> [Focus])
  | SetOffset !Int
  | SetSortMethod !SortMethod

-- | The depth of a tournament. Since we only care about distinguishing single
-- rounds from sequences of rounds, that is the only information stored here.
data Depth = TOne | TMany

class Typeable d => KnownDepth (d :: Depth) where
  depthVal :: proxy d -> Depth

instance KnownDepth 'TOne where
  depthVal _ = TOne

instance KnownDepth 'TMany where
  depthVal _ = TMany

--------------------------------------------------------------------------------
-- Basic syntax

-- Overlays of tournaments
---------------------------

-- | Overlay two tournaments. See 'Overlay'.
(+++) :: Tournament a -> Tournament a -> Tournament a
(+++) Empty a = a
(+++) a Empty = a
(+++) a b = Overlay a b

infixl 2 +++

-- | Overlay an arbitrary number of tournaments.
overlay :: Foldable f => f (Tournament a) -> Tournament a
overlay = foldr (+++) Empty

-- Sequences of tournaments
----------------------------

-- | Sequence two tournaments, running them one after the other. See 'Sequence'.
(***) :: forall a b. (KnownDepth a, KnownDepth b) => Tournament a -> Tournament b -> Tournament TMany
(***) = curry \case
  (Empty, Empty) -> Empty
  (Empty, cast -> Just b) -> b
  (cast -> Just a, Empty) -> a
  (LiftTMany a, LiftTMany b) -> a *** b
  (LiftTMany a, b) -> a *** b
  (a, LiftTMany b) -> a *** b
  (a, b) -> Sequence a b

infixl 1 ***

sequence :: (Foldable f, KnownDepth a) => f (Tournament a) -> Tournament TMany
sequence = foldr (***) Empty

-- Convenience
---------------

-- | Overloaded 'Tournament' constructor for constructing tournaments with just
-- one round.
class IsStep a where
  toStep :: a -> Tournament TOne

instance t ~ TOne => IsStep (Tournament t) where
  toStep = id

instance {-# OVERLAPS #-} IsStep Match where
  toStep = One

instance {-# OVERLAPS #-} (a ~ Int, b ~ Int) => IsStep (a, b) where
  toStep = One . uncurry Match

instance {-# OVERLAPPABLE #-} (Foldable f, IsStep a) => IsStep (f a) where
  toStep = foldr (\x xs -> toStep x +++ xs) Empty

--------------------------------------------------------------------------------
-- Compilation
--
-- Since tournaments depend on results that may not be available at the outset,

-- | A tournament is that has been compiled into a stream of rounds
newtype RoundStream m = RoundStream {unRoundStream :: (Focus -> m Standings) -> Focus -> StreamM m (Tournament TOne) ()}

-- | Compile a tournament into a stream of rounds.
--
-- Since we have paramaterised 'Tournament' by number of rounds, forcing it to
-- be @ 1 @ ensures that each individual tournament is only one round.
-- Meanwhile, since the structure of a stream does not allow for two rounds to
-- yield into the same round index, each individual round is unique.
--
-- In order to accomplish this, we just need to know upfront the 'PlayerCount',
-- and also a way to stream results in.
createRoundStream :: forall m a. Monad m => Tournament a -> RoundStream m
createRoundStream t0 = RoundStream \getStandings focus0 ->
  let
    go :: Tournament x -> Reader Focus (StreamM m (Tournament TOne) ())
    go = \case
      Empty -> pure mempty
      t@One{} -> pure (S.yield t)
      Modify m t -> S.map (Modify m) <$> go t
      Overlay a b -> do
        !a' <- go a
        !b' <- go b
        pure (S.alignWith alignOverlayed a' b')
        where
          alignOverlayed (These l r) = l +++ r
          alignOverlayed (This l) = l
          alignOverlayed (That r) = r
      Sequence a b -> liftA2 S.Cat (go a) (go b)
      LiftTMany t -> go t
      ByPlayerCount byCount -> do
        Focus{focusLength} <- ask
        go (byCount focusLength)
      ByFocus byFocus -> do
        focus <- ask
        go (byFocus focus)
      ByStandings byStandings -> do
        focus <- ask
        pure do
          standings <- S.lift (getStandings focus)
          go (byStandings standings) `runReader` focus
  in
    go t0 `runReader` focus0

-- | A round of a tournament that has been compiled into a stream of matches,
-- grouped by their sorting method
newtype MatchStream m = MatchStream
  { unMatchStream
      :: (Focus -> m Standings)
      -> Focus
      -> StreamM m (Sorter, StreamM m Match ()) ()
  }

-- | Create a stream of matches from a round of a tournament. (Note that
-- @Tournament 1@ indicates a single round)
createMatchStream :: forall m. Monad m => Tournament TOne -> MatchStream m
createMatchStream t0 = MatchStream \getStandings focus0 ->
  let
    go :: Tournament TOne -> ReaderT Sorter (StreamM m (Sorter, StreamM m Match ())) (StreamM m Match ())
    go = \case
      One m -> pure (S.yield m)
      Empty -> pure mempty
      Modify m t -> case m of
        SetFocus getFocii -> do
          Sorter focus _ <- ask
          let focii = getFocii focus
          results <- mapM (\f -> local (#sorterFocus .~ f) (go t)) focii
          pure (fold results)
        SetOffset off -> local (#sorterFocus . #focusStart +~ off) (go t)
        SetSortMethod method -> do
          local (#sorterMethod .~ method) do
            sorter <- ask
            inner <- go t
            lift do
              S.yield (sorter, inner)
              pure mempty
      Overlay a b -> do
        a' <- go a
        b' <- go b
        pure (a' >> b')
      ByPlayerCount byCount -> do
        Sorter Focus{focusLength} _ <- ask
        go (byCount focusLength)
      ByFocus byFocus -> do
        Sorter focus _ <- ask
        go (byFocus focus)
      ByStandings byStandings -> do
        Sorter focus _ <- ask
        standings <- lift (S.lift (getStandings focus))
        go (byStandings standings)
  in
    flip runReaderT (Sorter focus0 def) do
      s0 <- ask
      leftover <- go t0
      lift (S.yield (s0, leftover))

-- | A fully compiled 'Tournament' that has been transformed into a stream of
-- matches. The mnemonic for the name here is that the word "tourney" is a
-- smaller synonym of "tournament".
newtype Tourney m = Tourney {unTourney :: (Focus -> m Standings) -> Focus -> StreamM m (MatchStream m) ()}

-- | Compile a tournament into a 'Tourney'.
compile :: forall m a. Monad m => Tournament a -> Tourney m
compile t = Tourney \getStandings focus ->
  let RoundStream rounds = createRoundStream @m t
  in  S.map (createMatchStream @m) (rounds getStandings focus)

noStandings :: Monad m => Focus -> m Standings
noStandings Focus{focusStart, focusLength} = do
  () <- traceM "WARNING: noStandings called"
  pure (U.enumFromTo focusStart (focusStart + focusLength - 1))

runTourney
  :: Monad m
  => Tourney m
  -> (Focus -> m Standings)
  -> Focus
  -> StreamM m (StreamM m (Sorter, StreamM m Match ()) ()) ()
runTourney (Tourney f) getStandings focus0 =
  S.map (\(MatchStream ms) -> ms getStandings focus0) (f getStandings focus0)

runPureTourney :: Tournament a -> PlayerCount -> Vector (Vector (Sorter, Vector Match))
runPureTourney t count =
  let !rounds = S.pureVector (runTourney @Identity (compile t) noStandings focus0)
      !groups = V.map S.pureVector rounds
      !matches = V.map (each . _2 %~ S.pureVector) groups
  in  matches
  where
    focus0 = Focus{focusStart = 0, focusLength = count}

-- | Run a tournament purely, discarding sorters and completeness
runPureTourneyMatches :: Tournament a -> PlayerCount -> Vector (Vector Match)
runPureTourneyMatches t = V.map (V.concatMap snd) . runPureTourney t

--------------------------------------------------------------------------------
-- A small query language for querying the matches that have been built in a
-- tournament. Useful for builders or for inspecting a tournament without
-- running it

data Inspect (t :: Depth) a where
  ByRound :: Inspect TOne a -> Inspect TMany (Vector a)
  BySorter :: Inspect TOne (Vector (Sorter, Vector Match))
  Flat :: Inspect t (Vector Match)

data Inspection t m a = Inspection
  { standingsFn :: Focus -> m Standings
  , playerCount :: PlayerCount
  , query :: Inspect t a
  }

-- | Inspect a tournament
--
-- Useful for builders that somehow compose with another builder's matches
runInspection :: Monad m => Inspection t m a -> Tournament t -> m a
runInspection Inspection{standingsFn = getStandings, playerCount = count, query} t = case query of
  Flat ->
    VB.build <$> executingStateT (mempty :: VB.Builder _) do
      S.for_ (S.hoistT roundStream) \rs -> do
        S.for_ (S.hoistT (runStreamFn unMatchStream count getStandings rs)) \(_, matchGroup) -> do
          S.for_ (S.hoistT matchGroup) \match -> do
            modify' (<> VB.singleton match)
  BySorter ->
    VB.build <$> executingStateT (mempty :: VB.Builder _) do
      S.for_ (S.hoistT roundStream) \rs -> do
        S.for_ (S.hoistT (runStreamFn unMatchStream count getStandings rs)) \(sorter, matchGroup) -> do
          matches <- lift (S.toVector matchGroup)
          modify' (<> VB.singleton (sorter, matches))
  ByRound Flat ->
    VB.build <$> executingStateT (mempty :: VB.Builder _) do
      S.for_ (S.hoistT roundStream) \rs -> do
        S.for_ (S.hoistT (runStreamFn unMatchStream count getStandings rs)) \(_, matchGroup) -> do
          matches <- lift (S.toVector matchGroup)
          modify' (<> VB.singleton matches)
  ByRound BySorter ->
    VB.build <$> executingStateT (mempty :: VB.Builder _) do
      S.for_ (S.hoistT roundStream) \rs -> do
        here <-
          VB.build <$> executingStateT (mempty :: VB.Builder _) do
            S.for_ (S.hoistT (S.hoistT (runStreamFn unMatchStream count getStandings rs))) \(sorter, matchGroup) -> do
              matches <- lift (lift (S.toVector matchGroup))
              modify' (<> VB.singleton (sorter, matches))
        modify' (<> VB.singleton here)
  where
    Tourney tourney = compile t
    roundStream = tourney getStandings (Focus 0 count)

--------------------------------------------------------------------------------
-- Internals

-- | Run a stream function like that created by 'createMatchStream', with no
-- ability to see standings
runStreamFn
  :: (streamfn -> ((Focus -> m Standings) -> Focus -> x)) -> PlayerCount -> (Focus -> m Standings) -> streamfn -> x
runStreamFn unconstr count getStandings streamfn = unconstr streamfn getStandings (Focus 0 count)

-- | Run a stream function like that created by 'createMatchStream', with no
-- ability to see standings
runStreamFnPurely :: Monad m => PlayerCount -> ((Focus -> m Standings) -> Focus -> x) -> x
runStreamFnPurely count f = f noStandings focus0
  where
    focus0 = Focus 0 count
