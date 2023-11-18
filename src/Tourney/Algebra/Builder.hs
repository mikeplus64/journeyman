module Tourney.Algebra.Builder (
  Builder,

  -- * Overloaded constructors

  -- * Steps
  AsSteps (..),
  Steps,
  round_,
  rounds_,
  asRound,

  -- * Rounds
  AsRound (..),
  Round,
  match,

  -- * Matches
  AsMatch (..),

  -- * Basic syntax
  list,
  overlaysBy,
  overlays,
  overlays_,
  getStandings,
  getPlayerCount,
  withFocus,
  withFocii,
  withOffset,
  divideInto,
  foldAround,
  foldAroundMidpoint,
  swaps,
  points,
  barrier,

  -- * Reduction back into a 'Tournament'
  inspect,
  execBuilder,
  execSteps,
  execRound,
  runSteps,
  runRound,
  getAccum,
) where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans.Accum
import Control.Monad.Writer
import Data.DList (DList)
import Data.DList qualified as DL
import Tourney.Algebra.Unified as U
import Tourney.Common
import Tourney.Match
import Tourney.Stream (Inspect (..), Inspection (..), noStandings, runInspection)
import Prelude hiding (round, sequence)

-- | Describe what strategy is appropriate to use for merging tournaments of a
-- certain depth. For depth 1, use overlaying; for depth>=1, use sequencing.
class Merge (t :: Depth) where
  merge :: [Tournament t] -> Tournament t

instance Merge TOne where
  merge = U.overlay

instance Merge ('TMod t) where
  merge = U.overlay

instance Merge TMany where
  merge = U.sequence

newtype Builder t r a = Builder (ContT r (AccumT (DList (Tournament t)) DList) a)
  deriving (Functor, Applicative, Monad, MonadFail) via (ContT r (AccumT (DList (Tournament t)) DList))

silently :: Merge i => Builder i a a -> Builder t r (Tournament i)
silently b = pure (execBuilder id b)

getAccum :: Merge t => Builder t r (Tournament t)
getAccum = Builder (lift (looks (merge . DL.toList)))

tellBuilder :: Tournament t -> Builder t r ()
tellBuilder t = Builder (lift (add (DL.singleton t)))

runBuilder :: Merge t => (a -> r) -> Builder t r a -> [(r, Tournament t)]
runBuilder f b = DL.toList (fmap (second (merge . DL.toList)) (runBuilderDL f b))

execBuilder :: Merge t => (a -> r) -> Builder t r a -> Tournament t
execBuilder f = merge . map (merge . DL.toList . snd) . DL.toList . runBuilderDL f

--------------------------------------------------------------------------------
-- Steps

-- | A monad to build tournaments over steps (See '(***)'). Operations in this
-- monad create rounds. See 'Round' for a builder monad to create rounds.
type Steps = Builder TMany

runSteps :: (a -> r) -> Steps r a -> ([r], Tournament TMany)
runSteps f = second U.sequence . unzip . runBuilder f

execSteps :: (a -> r) -> Steps r a -> Tournament TMany
execSteps f = U.sequence . map snd . runBuilder f

-- | Values that have simple 'Steps' representations. Think of this as an
-- overloaded 'Steps' constructor.
class AsSteps a r where
  steps :: a -> Steps r ()

instance a ~ () => AsSteps (Steps r a) r where
  steps = id

instance t ~ TMany => AsSteps (Tournament t) r where
  steps = tellBuilder

instance AsRound a () => AsSteps [a] r where
  steps = mapM_ (round_ . toRound @a)

instance AsRound a () => AsSteps (Vector a) r where
  steps = mapM_ (round_ . toRound @a)

instance (AsSteps a r, r ~ ()) => AsSteps (PlayerCount -> a) r where
  steps f = tellBuilder $ ByPlayerCount $ execSteps id . steps @a @r . f

instance (AsSteps a r, r ~ ()) => AsSteps (Standings -> a) r where
  steps f = tellBuilder $ ByStandings $ execSteps id . steps @a @r . f

-- Basic syntax for 'Steps'
----------------------------------------

-- | Any monad that can add rounds, such as 'Steps'
class Monad m => MonadRounds m where
  addRound :: Round () () -> m ()
  default addRound :: (m ~ t i, MonadTrans t, MonadRounds i) => Round () () -> m ()
  addRound r = lift (addRound r)

instance MonadRounds (Steps r) where addRound r = tellBuilder (LiftTOne (execRound id r))
instance MonadRounds m => MonadRounds (StateT s m)
instance MonadRounds m => MonadRounds (ReaderT s m)

-- | Add a round to steps
round_ :: forall a m. (MonadRounds m, AsRound a ()) => a -> m ()
round_ = addRound . toRound @a

-- | Add some rounds to steps
rounds_ :: AsRound r () => [r] -> Steps s ()
rounds_ = mapM_ round_

asRound :: Round r a -> Round r a
asRound = id

--------------------------------------------------------------------------------
-- Rounds

-- | A monad to build tournaments over matches. For convenience, this monad
-- behaves like a list monad, where the inner list is ultimately combined by
-- 'U.overlay'.
type Round = Builder TOne

runRound :: (a -> r) -> Round r a -> ([r], Tournament TOne)
runRound f = second U.overlay . unzip . runBuilder f

execRound :: (a -> r) -> Round r a -> Tournament TOne
execRound f = snd . runRound f

class AsRound a r where
  toRound :: a -> Round r ()

instance (x ~ (), r ~ r') => AsRound (Round r x) r' where
  toRound = id

instance t ~ TOne => AsRound (Tournament t) r where
  toRound = tellBuilder

instance AsRound Match r where
  toRound = match

instance (AsRound a r, r ~ ()) => AsRound [a] r where
  toRound = overlays_ . map (toRound @a) . toList

instance (AsRound a r, r ~ ()) => AsRound (Vector a) r where
  toRound = overlays_ . fmap (toRound @a) . toList

instance (AsRound a r, r ~ ()) => AsRound (PlayerCount -> a) r where
  toRound f = tellBuilder $ ByPlayerCount $ execRound id . toRound @a . f

instance (AsRound a r, r ~ ()) => AsRound (Standings -> a) r where
  toRound f = tellBuilder $ ByStandings $ execRound id . toRound @a . f

-- Basic syntax for 'Round'
----------------------------------------

class AsMatch m where toMatch :: m -> Match
instance (x ~ Slot, y ~ Slot) => AsMatch (x, y) where toMatch = uncurry Match
instance AsMatch Match where toMatch = id

-- | Add a match to the round
match :: AsMatch m => m -> Round r ()
match m = tellBuilder (One (toMatch m))

--------------------------------------------------------------------------------
-- Shared syntax for both

-- | Lift a list into a 'Round' for iteration. The underlying effect on the
-- 'Round' is to overlay the iterated elements
list :: Foldable f => f a -> Builder t r a
list a = Builder (lift (lift (foldr DL.cons DL.empty a)))

-- | Overlay a collection of builders
overlaysBy :: (Foldable f, Merge t) => (a -> x) -> f (Builder t x a) -> Builder t r [x]
overlaysBy f b = Builder $ ContT \ret -> do
  let r = map (runBuilder f) (toList b)
  add (DL.singleton (U.overlay (concatMap (map snd) r)))
  ret (concatMap (map fst) r)

-- | Overlay a collection of builders
overlays :: (Foldable f, Merge t) => f (Builder t a a) -> Builder t r [a]
overlays = overlaysBy id

-- | Overlay a collection of builders
overlays_ :: (Foldable f, Merge t) => f (Builder t a a) -> Builder t r ()
overlays_ = void . overlays

-- | Retrieve the current standings
--
-- Warning: this operation will have the effect of segmenting the tournament
-- into two sections, as a tournament runner cannot continue past this point
-- without first having the standings.
getStandings :: Merge t => Builder t () Standings
getStandings = Builder $ ContT \f -> do
  add (DL.singleton (ByStandings (mergeAccumT . f)))

-- | Retrieve the current player count
getPlayerCount :: Merge t => Builder t () PlayerCount
getPlayerCount = Builder $ ContT \f ->
  add (DL.singleton (ByPlayerCount (mergeAccumT . f)))

-- | Set the focus of the inner builder
withFocus :: Merge t => Slot -> Int -> Builder t a a -> Builder ('TMod t) r ()
withFocus focusStart focusLength =
  withFocii (const [Focus{focusStart, focusLength}])

-- | Set the focii of the inner builder
withFocii :: Merge t => (Focus -> [Focus]) -> Builder t a a -> Builder ('TMod t) r ()
withFocii makeFocii b =
  tellBuilder (Modify (SetFocus makeFocii) (execBuilder id b))

-- | Set the sort method of the inner builder
withSort :: Merge t => SortMethod -> Builder t a a -> Builder t r ()
withSort method b =
  tellBuilder (Sort method (execBuilder id b))

swaps :: Merge t => Builder t a a -> Builder t r ()
swaps = withSort WinnerTakesHigh

points :: Merge t => Builder t () () -> Builder t r ()
points = withSort PointsAward . barrier

barrier :: Merge t => Builder t () () -> Builder t r ()
barrier = tellBuilder . execBuilder id

-- | Set the offset of the inner builder
withOffset :: Merge t => Int -> Builder t a a -> Builder ('TMod t) r ()
withOffset offs b =
  tellBuilder (Modify (SetOffset offs) (execBuilder id b))

-- Combinators
---------------
-- These can be built by a user of the API but are defined here for convenience.

-- | Given a denominator @n@ and a builder, split the tournament into @n@
-- sub-tournaments and run the builder on each
divideInto :: Int -> Steps () () -> Steps () ()
divideInto denom builder =
  tellBuilder (LiftTMod (Modify (SetFocus mkFocii) inner))
  where
    inner = execBuilder id builder
    mkFocii Focus{focusStart, focusLength} = do
      let (m, _r) = divMod focusLength denom
      g <- [0 .. denom - 1]
      pure Focus{focusStart = focusStart + Slot (g * m), focusLength = m}

-- | Fold a list of players together around a midpoint of the list.
foldAround :: Int -> [Slot] -> [Match]
foldAround midpoint players = take midpoint (zipWith Match players (reverse players))

-- | Fold a list of players together around _the_ midpoint of the list. You
-- should ensure the input list has an even number of players.
foldAroundMidpoint :: [Slot] -> [Match]
foldAroundMidpoint players = take (length players `div` 2) (zipWith Match players (reverse players))

-- | Inspect the inner builder, for, e.g., matches
inspect :: (Merge t, Merge i) => Inspect i a -> Builder i r r -> Builder t () a
inspect query builder = do
  playerCount <- getPlayerCount
  inner <- silently builder
  runInspection Inspection{standingsFn = noStandings, playerCount, query} inner

--------------------------------------------------------------------------------
-- Internals

mergeAccumT :: Merge t => AccumT (DList (Tournament t)) DList x -> Tournament t
mergeAccumT = merge . DL.toList . join . flip execAccumT mempty

runBuilderDL :: (a -> r) -> Builder t r a -> DList (r, DList (Tournament t))
runBuilderDL f (Builder run) = runAccumT (runContT run (pure . f)) mempty
