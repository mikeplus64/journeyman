module Tourney.Algebra.Monad where

import Control.Lens hiding (Empty)
import Control.Monad.Reader
import Control.Monad.Writer (MonadWriter (..), WriterT (..), censor)
import Data.Align qualified as Align
import Data.Counting as C
import Data.Dependency (MonadRequest (..), StreamM (..))
import Data.Dependency qualified as S
import Data.Generics.Labels ()
import Data.These
import Data.Vector (Vector)
import Data.Vector qualified as V
import Tourney.Algebra.Step
import Tourney.Match
import Tourney.Types
import Prelude hiding (pass)

data StepsContext = StepsContext
  { outerSteps :: Seq Step
  , playerCount :: !PlayerCount
  , knownStandings :: !(Maybe Standings)
  }
  deriving stock (Generic)

newtype Steps m a = Steps {unSteps :: StepsContext -> m (a, Seq Step)}
  deriving stock (Functor, Generic)
  deriving
    (Applicative, Monad, MonadFix, MonadReader StepsContext, MonadWriter (Seq Step), MonadFail)
    via (ReaderT StepsContext (WriterT (Seq Step) m))

run :: Steps m a -> PlayerCount -> m (a, Seq Step)
run (Steps s) pc = s StepsContext{outerSteps = mempty, playerCount = pc, knownStandings = Nothing}

createStepsContext :: PlayerCount -> StepsContext
createStepsContext playerCount =
  StepsContext
    { playerCount
    , outerSteps = mempty
    , knownStandings = mempty
    }

type StepsResult a = (a, Seq Step)

instance Monad m => Semigroup (Steps m a) where
  (<>) = (>>)

instance (Monoid a, Monad m) => Monoid (Steps m a) where
  mempty = pure mempty

instance MonadTrans Steps where
  lift m = Steps \_ -> (,mempty) <$> m

rehearse :: forall m a. Monad m => Steps m a -> Steps m (a, Seq CompiledStep)
rehearse s = do
  count <- view #playerCount
  (a, steps :: Seq Step) <- silently (listen s)
  pure (a, fmap (`compile` count) steps)

rehearsePure :: forall m a. Monad m => Steps m a -> Steps m (a, [Vector Match])
rehearsePure s = do
  count <- view #playerCount
  (a, steps :: Seq Step) <- silently (listen s)
  let toMatches x = foldMap snd (compilePure x count)
  pure (a, map toMatches (toList steps))

silently :: MonadWriter w m => m a -> m a
silently = censor (const mempty)

addPlainStep :: Monad m => Step -> Steps m Step
addPlainStep s = s <$ tell (one s)

class Monad m => AddStep s m where
  addStep :: s -> Steps m Step

instance (Monad m, a ~ ()) => AddStep (StepBuilder a) m where
  addStep = buildStep

instance Monad m => AddStep Step m where
  addStep = addPlainStep

instance {-# OVERLAPS #-} Monad m => AddStep Match m where
  addStep = addPlainStep . toStep

instance {-# OVERLAPS #-} (Monad m, a ~ Int, b ~ Int) => AddStep (a, b) m where
  addStep = addPlainStep . toStep

instance {-# OVERLAPS #-} (Monad m, playerCount ~ PlayerCount, AddStep s m) => AddStep (playerCount -> s) m where
  addStep f = view #playerCount >>= addStep . f

instance {-# OVERLAPPABLE #-} (Monad m, Foldable f, IsStep a) => AddStep (f a) m where
  addStep = addPlainStep . toStep

getPlayerCount :: Monad m => Steps m PlayerCount
getPlayerCount = view #playerCount

-- | Write a step to the current 'Steps'
--
-- The 'AddStep' constraint enables a few overloaded definitions of this function:
--
-- @
-- step :: Step -> Steps m () -- ^ Add a "raw" step
-- step :: StepBuilder () -> Steps m () -- ^ Add a step builder
-- step :: [Step] -> Steps m () -- ^ Add a list of 'Step' to be overlayed together
-- step :: [Match] -> Steps m () -- ^ Add a list of matches to be overlayed together
-- step :: Match -> Steps m ()
-- step :: (Player, Player) -> Steps m ()
-- @
step_ :: AddStep s m => s -> Steps m ()
step_ = void . addStep

-- | Write a step to the current 'Steps', with passthrough of the computed 'Step'
step :: AddStep s m => s -> Steps m Step
step = addStep

alignWith
  :: Monad m
  => (Step -> Step -> Step)
  -> (a -> b -> c)
  -> Steps m a
  -> Steps m b
  -> Steps m c
alignWith combineSteps combineResults (Steps fa) (Steps fb) = Steps \ctx -> do
  (a, sa) <- fa ctx
  (b, sb) <- fb ctx
  -- We give each alignment function access to the current accumulation, but the
  -- final accumulation is ensured to be just the "new" suffix that 'fa' or 'fb'
  -- generated. Since AccumT is only monoidal in nature this should work.
  pure (combineResults a b, Align.alignWith combine sa sb)
  where
    combine (These l r) = combineSteps l r
    combine (This l) = combineSteps l mempty
    combine (That r) = combineSteps mempty r

interleaves :: Monad m => [Steps m a] -> Steps m [a]
interleaves = foldr (\l r -> uncurry (:) <$> alignWith overlay2 (,) l r) (pure mempty)

interleaves_ :: Monad m => [Steps m ()] -> Steps m ()
interleaves_ = foldr interleave_ (pure ())

interleave :: Monad m => Steps m a -> Steps m b -> Steps m (a, b)
interleave = alignWith overlay2 (,)

interleave_ :: Monad m => Steps m a -> Steps m b -> Steps m ()
interleave_ = alignWith overlay2 (\_ _ -> ())

infixl 0 |||
(|||) :: Monad m => Steps m a -> Steps m b -> Steps m ()
(|||) = interleave_

infixl 0 <|||
(<|||) :: Monad m => Steps m a -> Steps m b -> Steps m (a, b)
(<|||) = interleave

divideInto :: Monad m => Int -> Steps m () -> Steps m ()
divideInto denom steps = do
  !count <- view #playerCount
  let (m, r) = quotRem count denom
  let groupSize = m + r
  interleaves_
    [ focus (groupNo * groupSize) groupSize steps
    | groupNo <- [0 .. denom - 1]
    ]

-- | Focus the provided 'Steps' over an interval
focus
  :: Monad m
  => Int
  -- ^ The starting slot of the sorting network to focus
  -> Int
  -- ^ The size of the interval, in players.
  -> Steps m ()
  -> Steps m ()
focus _start size | size <= 0 = \_ -> pure ()
focus start size = censor (fmap (Offset start)) . local (#playerCount .~ size)

concatMapSteps :: Monad m => Steps m a -> (Int -> Step -> Steps (ReaderT (StepsResult a) m) b) -> Steps m (a, Seq b)
concatMapSteps (Steps s) f = Steps \pc -> do
  r@(a, sa) <- s pc
  let Steps next = imapM f sa
  (bs, sb) <- next pc `runReaderT` r
  pure ((a, bs), sb)

concatMapSteps_
  :: Monad m
  => Steps m ()
  -> (Int -> Step -> Steps (ReaderT (Seq Step) m) ())
  -> Steps m ()
concatMapSteps_ (Steps s) f = Steps \pc -> do
  (_, sa) <- s pc
  (_, sb) <- unSteps (imapM_ f sa) pc `runReaderT` sa
  pure ((), sb)

delay :: Monad m => Int -> Steps m ()
delay rounds = replicateM_ rounds (pure ())

instance Monad m => MonadState s (Steps (StateT s m)) where
  get = lift get
  put = lift . put
  state = lift . state

{-# INLINE statefully #-}
{-# INLINE statefully_ #-}
{-# INLINE reading #-}

statefully :: Monad m => s -> Steps (StateT s m) a -> Steps m (a, s)
statefully s0 (Steps f) = Steps \pc -> do
  ((result, acc), s1) <- runStateT (f pc) s0
  pure ((result, s1), acc)

statefully_ :: Monad m => s -> Steps (StateT s m) a -> Steps m a
statefully_ s0 s = fst <$> statefully s0 s

reading :: Monad m => r -> Steps (ReaderT r m) a -> Steps m a
reading r (Steps f) = Steps \pc -> runReaderT (f pc) r

--------------------------------------------------------------------------------

-- | Compiling steps into a stream of 'CompiledStep'.
--
-- To interpret the 'CompiledStep', see
compileRounds :: Monad m => Steps m a -> PlayerCount -> StreamM m CompiledStep ()
compileRounds description count = do
  (_, steps) <- S.lift (run description count)
  forM_ steps \here ->
    S.yield $! compile here count

--------------------------------------------------------------------------------

-- | A monad for building a 'Step'
newtype StepBuilder a = StepBuilder (StepsContext -> CountAcc (a, Step))
  deriving (Functor, Applicative, Monad) via (ReaderT StepsContext (WriterT Step CountAcc))

_StepBuilder :: Iso' (StepBuilder a) (ReaderT StepsContext (WriterT Step CountAcc) a)
_StepBuilder = coerced

instance MonadWriter Step StepBuilder where
  tell s = tell s ^. re _StepBuilder
  pass f = pass (f ^. _StepBuilder) ^. re _StepBuilder
  listen f = listen (f ^. _StepBuilder) ^. re _StepBuilder

instance Semigroup (StepBuilder a) where
  (<>) = (>>)

instance Monoid a => Monoid (StepBuilder a) where
  mempty = pure mempty

plus :: IsStep a => a -> StepBuilder ()
plus = tell . toStep

expand :: Foldable f => f a -> StepBuilder a
expand xss = StepBuilder \_ -> foldr (\x xs -> cons (x, mempty) xs) mempty xss

runStepBuilder :: Monoid a => StepBuilder a -> StepsContext -> (a, Step)
runStepBuilder (StepBuilder builder) ctx = (fold as, optimise1 (Overlay steps))
  where
    (as, steps) = V.unzip (countAccToVec (builder ctx))

execStepBuilder :: StepBuilder () -> StepsContext -> Step
execStepBuilder builder = snd . runStepBuilder builder

buildStep :: Monad m => StepBuilder () -> Steps m Step
buildStep builder = do
  (_, s) <- asks (runStepBuilder builder)
  addPlainStep s

--------------------------------------------------------------------------------

{-# INLINE fromTrans #-}
{-# INLINE toTrans #-}
{-# INLINE trans #-}
trans :: Iso' (Steps m a) (ReaderT StepsContext (WriterT (Seq Step) m) a)
trans = coerced

toTrans :: Steps m a -> ReaderT StepsContext (WriterT (Seq Step) m) a
toTrans = coerce

fromTrans :: ReaderT StepsContext (WriterT (Seq Step) m) a -> Steps m a
fromTrans = coerce
