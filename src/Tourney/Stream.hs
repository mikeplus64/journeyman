-- | This module defines ways to create streams of matches from tournaments.
--
-- Since tournaments depend on results that may not be available immediately,
-- the types for the streams here get quite complex. This is so that the streams
-- are able to expose all the matches that are /purely/ available, that is all
-- the matches and rounds that can be extracted from a tournament immediately
-- without having any results. For most tournaments, this is probably all
-- matches, but some tournament styles may not have this property, such as
-- Swiss-style.
--
-- At the same time, I try to validate that the tournament is valid namely that
-- no focus is set that is not within the outer/current focus, and that no match
-- uses slots that are not in the current focus.
--
-- = How this works
--
-- The core operation that enables the transformation of arbitrarily nested and
-- unbalanced combinations of 'Overlay' and 'Sequence' is /alignment/.
-- Effectively, every occurence of 'Overlay' triggers an alignment operation
-- such that both sides are balanced.
--
-- These diagrams may help elucidate how this is possible:
--
-- <<https://mikeplus64.github.io/journeyman/ub1.png>>
--
-- <<https://mikeplus64.github.io/journeyman/ub2.png>>
--
-- <<https://mikeplus64.github.io/journeyman/ub3.png>>
--
-- Here, an unbalanced combination of overlays and interleaves is turned into a
-- balanced sequence of overlays by successively re-balancing the tournament
-- tree.
module Tourney.Stream (
  -- * Compilation

  -- ** Creating a compiler environment
  StreamEnv,
  createStreamEnv,
  withGetStandings,
  noStandings,

  -- ** Main compilation functions
  Tourney (..),
  TourneyStream,
  CompileError (..),
  Compiled,
  createTourney,
  runTourney,
  RoundStream (..),
  createRoundStream,
  MatchStream (..),
  createMatchStream,

  -- * Inspection
  Inspection (..),
  Inspect (..),
  runInspection,
  pureMatchesByRound,
) where

import Control.Lens qualified as L
import Control.Monad.Except
import Data.Dependency (StreamM)
import Data.Dependency qualified as S
import Data.These
import Data.Vector qualified as V
import Tourney.Algebra.Unified
import Tourney.Common
import Tourney.Match
import Tourney.Match.Matrix (MapByMatches, MapByRound)
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB
import Prelude hiding (Empty)

data StreamEnv m = StreamEnv
  { getStandings :: !(Focus -> m Standings)
  , focus :: {-# UNPACK #-} !Focus
  }
  deriving stock (Generic)

data CompileError
  = InvalidMatch {focus :: !Focus, match :: !Match}
  | InvalidFocus {outer, inner :: !Focus}
  deriving stock (Eq, Ord, Show, Generic)

-- | A value that has gone through a compiler; either the compiled thing, or an
-- error. Convenience type
type Compiled = Either (NonEmpty CompileError)

-- | A tournament is that has been compiled into a stream of rounds
newtype RoundStream m = RoundStream
  { unRoundStream :: StreamEnv m -> StreamM m (Tournament TOne) ()
  }

err :: CompileError -> Compiled a
err = Left . (:| [])

-- | Compile a tournament into a stream of rounds.

-- | Compile a tournament into a stream of rounds.
--
-- Since we have paramaterised 'Tournament' by number of rounds, forcing it to
-- be @ TOne @ ensures that each individual tournament is only one round.
-- Meanwhile, since the structure of a stream does not allow for two rounds to
-- yield into the same round index, each individual round is unique.
--
-- In order to accomplish this, we just need to know upfront the 'PlayerCount',
-- and also a way to stream results in.
createRoundStream :: forall m a. Monad m => Tournament a -> RoundStream m
createRoundStream t0 = RoundStream \StreamEnv{getStandings, focus = focus0} ->
  let
    go :: Tournament x -> Reader Focus (StreamM m (Tournament TOne) ())
    go = \case
      Empty -> pure mempty
      One (Match a b) -> do
        Focus{focusStart = s} <- ask
        pure (S.yield (One (Match (s + a) (s + b))))
      Modify (SetOffset offs) t -> do
        next <- asks (#focusStart +~ Slot offs)
        local (const next) (go t)
      Modify (SetFocus getFocii) t -> do
        Focus{focusStart = cur} <- ask
        focii <- asks (map (#focusStart +~ cur) . getFocii)
        results <- mapM (\f -> local (const f) (go t)) focii
        pure (overlayMany results)
      -- "Unlift" the overlay operation to the inside of the tournament
      Overlay a b -> do
        !a' <- go a
        !b' <- go b
        pure (overlay2 a' b')
      Sequence a b -> do
        a' <- go a
        b' <- go b
        pure (a' <> b')
      LiftTOne t -> go t
      LiftTMod t -> go t
      ByPlayerCount byCount -> do
        Focus{focusLength} <- ask
        go (byCount focusLength)
      ByStandings byStandings -> do
        focus <- ask
        pure do
          standings <- S.lift (getStandings focus)
          go (byStandings standings) `runReader` focus
      Sort meth t -> do
        a <- go t
        -- Seems weird. It also is weird. Oh well.
        pure (a <> S.yield (Sort meth Empty))
  in
    go t0 `runReader` focus0

overlay2
  :: Monad m
  => S.StreamM m (Tournament t) ()
  -> S.StreamM m (Tournament t) ()
  -> S.StreamM m (Tournament t) ()
overlay2 = S.alignWith alignOverlayed
  where
    alignOverlayed (These l r) = l +++ r
    alignOverlayed (This l) = l
    alignOverlayed (That r) = r

overlayMany :: Monad m => [S.StreamM m (Tournament t) ()] -> S.StreamM m (Tournament t) ()
overlayMany = foldr overlay2 (pure ())

-- | A round of a tournament that has been compiled into a stream of matches,
-- grouped by their sorting method
newtype MatchStream m = MatchStream
  { unMatchStream :: StreamEnv m -> StreamM m (Compiled (Sorter, StreamM m (Compiled Match) ())) ()
  }

-- | Create a stream of matches from a round of a tournament. (Note that
-- @Tournament 1@ indicates a single round)
createMatchStream :: forall m. Monad m => Tournament TOne -> MatchStream m
createMatchStream t0 = MatchStream \StreamEnv{getStandings, focus = focus0} ->
  let
    go
      :: Tournament TOne
      -> ReaderT Sorter (StreamM m (Compiled (Sorter, StreamM m (Compiled Match) ()))) (StreamM m (Compiled Match) ())
    go = \case
      One m -> do
        Sorter focus _ <- ask
        pure
          . S.yield
          $! if validateMatch focus m
            then Right m
            else err InvalidMatch{focus, match = m}
      Empty -> pure mempty
      Sort method t -> do
        local (#sorterMethod .~ method) do
          sorter <- ask
          inner <- go t
          lift do
            S.yield (Right (sorter, inner))
            pure mempty
      Overlay a b -> do
        a' <- go a
        b' <- go b
        pure (a' >> b')
      ByPlayerCount byCount -> do
        Sorter Focus{focusLength} _ <- ask
        go (byCount focusLength)
      ByStandings byStandings -> do
        Sorter focus _ <- ask
        standings <- lift (S.lift (getStandings focus))
        go (byStandings standings)
  in
    flip runReaderT (Sorter focus0 def) do
      s0 <- ask
      leftover <- go t0
      unless (S.null leftover) (lift (S.yield (Right (s0, leftover))))

-- | A fully compiled 'Tournament' that has been transformed into a stream of
-- matches. The mnemonic for the name here is that the word "tourney" is a
-- smaller synonym of "tournament".
data Tourney m = Tourney
  { tourneyStream :: StreamM m (MatchStream m) ()
  , tourneyStreamEnv :: StreamEnv m
  }

-- | Compile a tournament into a 'Tourney'.
createTourney :: forall m a. Monad m => StreamEnv m -> Tournament a -> Tourney m
createTourney cenv t =
  Tourney
    { tourneyStream =
        let RoundStream rounds = createRoundStream @m t
        in  S.map (createMatchStream @m) (rounds cenv)
    , tourneyStreamEnv = cenv
    }

noStandings :: Monad m => Focus -> m Standings
noStandings Focus{focusLength} = pure (createInitialStandings focusLength)

createStreamEnv :: Monad m => PlayerCount -> StreamEnv m
createStreamEnv count = StreamEnv{focus = Focus 0 count, getStandings = noStandings}

withGetStandings :: (Focus -> m Standings) -> StreamEnv m -> StreamEnv m
withGetStandings fn c = c{getStandings = fn}

-- --------------------------------------------------------------------------------

-- | A stream of
-- 1. Rounds, which are streams of
-- 2. Match groups, which are sorters (a 'Focus' and 'SortMethod'), and streams of
-- 3. Matches
type TourneyStream m = StreamM m (StreamM m (Compiled (Sorter, StreamM m (Compiled Match) ())) ()) () -- Phew!

runTourney :: Monad m => Tourney m -> TourneyStream m
runTourney Tourney{tourneyStream, tourneyStreamEnv = cenv} =
  S.map (\(MatchStream ms) -> ms cenv) tourneyStream

--------------------------------------------------------------------------------
-- A small query language for querying the matches that have been built in a
-- tournament. Useful for builders or for inspecting a tournament without
-- running it

data Inspect (t :: Depth) a where
  ByRound :: Inspect TOne (Compiled a) -> Inspect TMany (Compiled (Vector a))
  BySorter :: Inspect TOne (Compiled (Vector (Sorter, Vector Match)))
  Flat :: Inspect t (Compiled (Vector Match))

data Inspection t m a = Inspection
  { standingsFn :: Focus -> m Standings
  , playerCount :: PlayerCount
  , query :: Inspect t a
  }

-- | Inspect a tournament
--
-- Useful for builders that somehow compose with another builder's matches
runInspection :: Monad m => Inspection t m a -> Tournament t -> m a
runInspection Inspection{standingsFn = getStandings, playerCount = count, query} t =
  -- Unfortunately I did not come up with an easy way to re-use code here.
  case query of
    Flat ->
      runExceptT $
        VB.build
          <$> executingStateT mempty do
            S.for_ (hoist2 roundStream) \rs -> do
              S.for_ (hoist2 (unMatchStream rs compiler0)) \case
                Left e -> throwError e
                Right (_, matchGroup) -> S.for_ (hoist2 matchGroup) \case
                  Left e -> throwError e
                  Right match -> modify' (<> VB.singleton match)
    -- or...
    BySorter ->
      runExceptT $
        VB.build
          <$> executingStateT mempty do
            S.for_ (hoist2 roundStream) \rs -> do
              S.for_ (hoist2 (unMatchStream rs compiler0)) \case
                Left e -> throwError e
                Right (sorter, matchGroup) -> do
                  matchesMaybe <- V.sequence <$> lift (lift (S.toVector matchGroup))
                  matches <- liftEither matchesMaybe
                  modify' (<> VB.singleton (sorter, matches))
    ByRound Flat ->
      runExceptT $
        VB.build
          <$> executingStateT mempty do
            S.for_ (hoist2 roundStream) \rs -> do
              S.for_ (hoist2 (unMatchStream rs compiler0)) \case
                Left e -> throwError e
                Right (_sorter, matchGroup) -> do
                  matchesRaw <- lift (lift (S.toVector matchGroup))
                  let matchesMaybe = V.sequence matchesRaw
                  matches <- liftEither matchesMaybe
                  modify' (<> VB.singleton matches)
    ByRound BySorter ->
      runExceptT $
        VB.build
          <$> executingStateT mempty do
            S.for_ (hoist2 roundStream) \rs -> do
              here <-
                VB.build <$> executingStateT mempty do
                  S.for_ (S.hoistT (hoist2 (unMatchStream rs compiler0))) \case
                    Left e -> throwError e
                    Right (sorter, matchGroup) -> do
                      matchesMaybe <- V.sequence <$> lift (lift (lift (S.toVector matchGroup)))
                      matches <- liftEither matchesMaybe
                      modify' (<> VB.singleton (sorter, matches))
              modify' (<> VB.singleton here)
  where
    compiler0 = StreamEnv{getStandings, focus = Focus 0 count}
    roundStream = tourneyStream (createTourney compiler0 t)
    hoist2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) => StreamM m a r -> StreamM (t1 (t2 m)) a r
    hoist2 = S.hoistT . S.hoistT

pureMatchesByRound
  :: Tournament TMany
  -> PlayerCount
  -> MapByRound (MapByMatches (Maybe Result))
pureMatchesByRound t count =
  runIdentity
    ( S.ifoldM
        ( \(RoundNo -> roundNo) (MatchStream ms) xs ->
            S.foldM
              ( \matchGroups xs' ->
                  case matchGroups of
                    Left _ -> pure xs'
                    Right (_, matches) ->
                      S.fold
                        ( \case
                            Right m -> at roundNo . non' _Empty . at m ?~ Nothing
                            Left _ -> id
                        )
                        xs'
                        matches
              )
              xs
              (ms senv)
        )
        L.Empty
        stream
    )
  where
    getStandings = noStandings @Identity
    senv = StreamEnv{getStandings, focus = Focus 0 count}
    stream = tourneyStream (createTourney senv t)

-- runIdentity (runInspection (Inspection noStandings count (ByRound Flat)) t)
--   & foldMapOf _Right
--     (fmap (foldMap (\m -> L.Empty & at m ?~ Nothing)) . vectorMapByRound)
