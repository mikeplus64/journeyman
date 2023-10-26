module Tourney.Algebra.Monad where

import Control.Lens hiding (Empty)
import Data.Align qualified as Align
import Data.Counting
import Data.Dependency
import Data.These
import Data.Tuple.Ordered
import Tourney.Algebra.Step
import Tourney.Algebra.Types
import Tourney.Match

newtype Steps m a = Steps
  { unSteps :: PlayerCount -> m (StepsResult a)
  }
  deriving stock (Functor)

data StepsResult a = StepsResult
  { accum :: Counting [] Step
  , result :: a
  }
  deriving stock (Show, Functor)

instance Applicative StepsResult where
  pure = StepsResult mempty
  StepsResult sa fa <*> StepsResult sb a = StepsResult (sa <> sb) (fa a)

instance Applicative m => Applicative (Steps m) where
  pure a = Steps \_ -> pure (pure a)
  liftA2 fab (Steps fa) (Steps fb) = Steps \pc ->
    liftA2 (liftA2 fab) (fa pc) (fb pc)

instance Monad m => Monad (Steps m) where
  Steps ma >>= famb = Steps \pc -> do
    StepsResult r0 a <- ma pc
    StepsResult r1 b <- unSteps (famb a) pc
    pure (StepsResult (r0 <> r1) b)

instance Monad m => Semigroup (Steps m a) where
  (<>) = (>>)

instance (Monoid a, Monad m) => Monoid (Steps m a) where
  mempty = Steps \_ -> pure (StepsResult mempty mempty)

instance MonadTrans Steps where
  lift m = Steps \_ -> pure <$> m

instance Monad m => MonadReader r (Steps (ReaderT r m)) where
  local f (Steps m) = Steps \c -> ReaderT \r0 -> m c `runReaderT` f r0
  ask = lift ask

preview :: MonadRequest Standings m => Steps m a -> Steps m ([CompiledStep m], a)
preview (Steps f) = Steps \count -> do
  StepsResult {accum, result} <- f count
  pure
    StepsResult
      { accum = mempty
      , result = (map (`compile` count) (uncounting accum), result)
      }

instance Monad m => HasPlayerCount (Steps m) where
  getPlayerCount = Steps \count -> pure StepsResult {accum = mempty, result = count}

step :: (Applicative m, IsStep s) => s -> Steps m ()
step s = Steps \_count ->
  pure
    StepsResult
      { accum = one (toStep s)
      , result = ()
      }

alignWith
  :: Monad m
  => (Step -> Step -> Step)
  -> (a -> b -> c)
  -> Steps m a
  -> Steps m b
  -> Steps m c
alignWith combineSteps combineResults (Steps fa) (Steps fb) = Steps \pc -> do
  StepsResult sa a <- fa pc
  StepsResult sb b <- fb pc
  pure
    StepsResult
      { accum = Align.alignWith combine sa sb
      , result = combineResults a b
      }
  where
    combine (These l r) = combineSteps l r
    combine (This l) = combineSteps l mempty
    combine (That r) = combineSteps mempty r

interleaves :: Monad m => [Steps m a] -> Steps m [a]
interleaves = foldr (\l r -> uncurry (:) <$> alignWith overlay2 (,) l r) (pure mempty)

interleave_ :: Monad m => [Steps m ()] -> Steps m ()
interleave_ = foldr (\l r -> void (interleave2 l r)) (pure ())

interleave2 :: Monad m => Steps m a -> Steps m b -> Steps m (a, b)
interleave2 = alignWith overlay2 (,)

interleave2_ :: Monad m => Steps m () -> Steps m () -> Steps m ()
interleave2_ l r = void (interleave2 l r)

concatMapSteps :: Monad m => Steps m a -> (Int -> Step -> Steps (ReaderT (StepsResult a) m) b) -> Steps m (a, [b])
concatMapSteps (Steps s) f = Steps \pc -> do
  r@StepsResult {accum = sa, result = a} <- s pc
  let Steps next = imapM f (uncounting sa)
  StepsResult {accum = sb, result = bs} <- next pc `runReaderT` r
  pure StepsResult {accum = sb, result = (a, bs)}

concatMapSteps_ :: Monad m => Steps m () -> (Int -> Step -> Steps (ReaderT (Counting [] Step) m) ()) -> Steps m ()
concatMapSteps_ (Steps s) f = Steps \pc -> do
  StepsResult {accum = sa} <- s pc
  let Steps next = imapM_ f (uncounting sa)
  next pc `runReaderT` sa

delay :: Monad m => Int -> Steps m ()
delay rounds = replicateM_ rounds (pure ())

statefully :: Monad m => s -> Steps (StateT s m) a -> Steps m (a, s)
statefully s0 (Steps f) = Steps \pc -> do
  (StepsResult {accum, result}, s1) <- runStateT (f pc) s0
  pure StepsResult {accum, result = (result, s1)}
