module Tourney.Algebra.Builder (
  Builder,

  -- * Overloaded constructors
  AsSteps (..),
  AsRound (..),
  AsMatch (..),

  -- * Steps
  Steps,
  round,
  round_,
  rounds_,

  -- * Rounds
  Round,
  match,

  -- * Basic syntax
  list,
  overlaysBy,
  overlays,
  overlays_,
  standings,
  focus,
  playerCount,
  withFocus,
  withFocii,
  withSortMethod,
  withOffset,
  divideInto,
  collapseMatches,

  -- * Reduction back into a 'Tournament'
  execSteps,
  runSteps,
  execRound,
  runRound,
) where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans.Accum
import Control.Monad.Writer
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Tuple.Ordered
import Tourney.Algebra.Unified as U
import Tourney.Match
import Tourney.Types
import Prelude hiding (round, sequence)

newtype Builder t r a = Builder (ContT r (AccumT (DList (Tournament t)) DList) a)
  deriving (Functor, Applicative, Monad) via (ContT r (Control.Monad.Trans.Accum.AccumT (DList (Tournament t)) DList))

tellBuilder :: Tournament t -> Builder t r ()
tellBuilder t = Builder (lift (Control.Monad.Trans.Accum.add (DL.singleton t)))

runBuilder :: Merge t => (a -> r) -> Builder t r a -> [(r, Tournament t)]
runBuilder f b = DL.toList (fmap (second (merge . DL.toList)) (runBuilderDL f b))

--------------------------------------------------------------------------------
-- Steps

-- | A monad to build tournaments over steps (See '(***)'). Operations in this
-- monad create rounds. See 'Round' for a builder monad to create rounds.
type Steps = Builder TMany

runSteps :: (a -> r) -> Steps r a -> ([r], Tournament TMany)
runSteps f = second U.sequence . unzip . runBuilder f

execSteps :: (a -> r) -> Steps r a -> Tournament TMany
execSteps f = U.sequence . map snd . runBuilder f

tellSteps :: Tournament TMany -> Steps a ()
tellSteps t = Builder (lift (Control.Monad.Trans.Accum.add (DL.singleton t)))

class AsSteps a where
  toSteps :: a -> Steps () ()

instance r ~ () => AsSteps (Steps r r) where
  toSteps = id

instance AsRound a => AsSteps [a] where
  toSteps = mapM_ (round . toRound @a)

instance AsSteps a => AsSteps (PlayerCount -> a) where
  toSteps f = tellBuilder $ ByPlayerCount $ execSteps id . toSteps @a . f

instance AsSteps a => AsSteps (Standings -> a) where
  toSteps f = tellBuilder $ ByStandings $ execSteps id . toSteps @a . f

instance AsSteps a => AsSteps (Focus -> a) where
  toSteps f = tellBuilder $ ByFocus $ execSteps id . toSteps @a . f

-- Basic syntax for 'Steps'
----------------------------------------

-- | Add a round to steps, giving the resulting values
round :: Round a a -> Steps s [a]
round r = as <$ tellSteps (LiftTMany t)
  where
    (as, t) = runRound id r

-- | Add a round to steps
round_ :: Round a a -> Steps s ()
round_ = void . round

-- | Add some rounds to steps
rounds_ :: [Round x x] -> Steps s ()
rounds_ = mapM_ round_

class AsRound a where
  toRound :: a -> Round () ()

instance x ~ () => AsRound (Round x x) where
  toRound = id

instance AsRound Match where
  toRound = match

instance AsRound a => AsRound [a] where
  toRound = overlays_ . map (toRound @a) . toList

instance AsRound a => AsRound (PlayerCount -> a) where
  toRound f = tellBuilder $ ByPlayerCount $ execRound id . toRound @a . f

instance AsRound a => AsRound (Standings -> a) where
  toRound f = tellBuilder $ ByStandings $ execRound id . toRound @a . f

instance AsRound a => AsRound (Focus -> a) where
  toRound f = tellBuilder $ ByFocus $ execRound id . toRound @a . f

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

tellRound :: Tournament TOne -> Round a ()
tellRound t = Builder (lift (Control.Monad.Trans.Accum.add (DL.singleton t)))

-- Basic syntax for 'Round'
----------------------------------------

class AsMatch m where toMatch :: m -> Match
instance (x ~ Int, y ~ Int) => AsMatch (x, y) where toMatch = uncurry OrdPair_
instance AsMatch Match where toMatch = id

-- | Add a match
match :: AsMatch m => m -> Round r ()
match m = tellRound (One (toMatch m))

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
  Control.Monad.Trans.Accum.add (DL.singleton (U.overlay (concatMap (map snd) r)))
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
standings :: Builder t () Standings
standings = Builder $ ContT \f -> do
  Control.Monad.Trans.Accum.add $ DL.singleton (ByStandings (overlayAccumT . f))

-- | Retrieve the current focus
focus :: Builder t () Focus
focus = Builder $ ContT \f -> do
  Control.Monad.Trans.Accum.add $ DL.singleton (ByFocus (overlayAccumT . f))

-- | Retrieve the current player count
playerCount :: Builder t () PlayerCount
playerCount = Builder $ ContT \f -> do
  Control.Monad.Trans.Accum.add $ DL.singleton (ByPlayerCount (overlayAccumT . f))

-- | Set the focus of the inner builder
withFocus :: Merge t => Int -> Int -> Builder t a a -> Builder t r ()
withFocus focusStart focusLength = withFocii (const [Focus{focusStart, focusLength}])

-- | Set the focii of the inner builder
withFocii :: Merge t => (Focus -> [Focus]) -> Builder t a a -> Builder t r ()
withFocii makeFocii b = Builder do
  let (_as, ts) = unzip (runBuilder id b)
  lift (Control.Monad.Trans.Accum.add $ DL.singleton (Modify (SetFocus makeFocii) (merge ts)))

-- | Set the sort method of the inner builder
withSortMethod :: Merge t => SortMethod -> Builder t a a -> Builder t r ()
withSortMethod method b = Builder do
  let (_as, ts) = unzip (runBuilder id b)
  lift (Control.Monad.Trans.Accum.add $ DL.singleton (Modify (SetSortMethod method) (merge ts)))

-- | Set the offset of the inner builder
withOffset :: Merge t => Int -> Builder t a a -> Builder t r ()
withOffset offs b = Builder do
  let (_as, ts) = unzip (runBuilder id b)
  lift (Control.Monad.Trans.Accum.add $ DL.singleton (Modify (SetOffset offs) (merge ts)))

-- Combinators
---------------
-- These can be built by a user of the API but are defined here for convenience.

-- | Given a denominator @n@ and a builder, split the tournament into @n@
-- sub-tournaments and run the builder on each
divideInto :: Merge t => Int -> Builder t () () -> Builder t () ()
divideInto denom builder = do
  count <- playerCount
  let (m, r) = quotRem count denom
  let groupSize = m + r
  overlays_
    [ withFocus (groupNo * groupSize) groupSize builder
    | groupNo <- [0 .. denom - 1]
    ]

-- | Fold a list of players together around a midpoint of the list.
collapseMatches :: Int -> [Player] -> [Match]
collapseMatches midpoint players = take midpoint (zipWith OrdPair_ players (reverse players))

--------------------------------------------------------------------------------
-- Internals

overlayAccumT :: Control.Monad.Trans.Accum.AccumT (DList (Tournament t)) DList x -> Tournament t
overlayAccumT =
  U.overlay . DL.toList . join . flip Control.Monad.Trans.Accum.execAccumT mempty

runBuilderDL :: (a -> r) -> Builder t r a -> DList (r, DList (Tournament t))
runBuilderDL f (Builder run) = Control.Monad.Trans.Accum.runAccumT (runContT run (pure . f)) mempty

class Merge (t :: Depth) where merge :: [Tournament t] -> Tournament t
instance Merge TOne where merge = U.overlay
instance Merge TMany where merge = U.sequence
