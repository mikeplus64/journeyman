module Tourney.Algebra.Monad where

import Control.Lens hiding (Empty)
import Control.Monad.ST.Strict
import Data.Align qualified as Align
import Data.These
import Data.Tuple.Ordered
import Tourney.Algebra.Step
import Tourney.Algebra.Types
import Tourney.Match

type TResult a = (Seq Step, a)

newtype Steps a = Steps
  { unSteps
      :: PlayerCount
      -> IO (TResult a)
  }
  deriving stock (Functor)

getPlayerCount :: Steps PlayerCount
getPlayerCount = Steps \pc -> pure (pure pc)

instance Applicative Steps where
  pure a = Steps \_ -> pure (pure a)
  liftA2 fab (Steps fa) (Steps fb) = Steps \pc ->
    liftA2 (liftA2 fab) (fa pc) (fb pc)

instance Monad Steps where
  Steps ma >>= famb = Steps \pc -> do
    (r0, a) <- ma pc
    (r1, b) <- unSteps (famb a) pc
    pure (r0 <> r1, b)

instance Semigroup (Steps a) where
  (<>) = (>>)

instance Monoid a => Monoid (Steps a) where
  mempty = Steps \_ -> pure (mempty, mempty)

describe :: Steps a -> PlayerCount -> Seq Step
describe (Steps f) c = runST (fst <$> f c)

liftSteps :: Seq Step -> Steps ()
liftSteps s = Steps \_ -> pure (s, ())

class AsSteps s where
  step :: AsSteps s => s -> Steps [Match]
  step = stepMany . one

  stepMany :: AsSteps s => [s] -> Steps [Match]
  stepMany = mapM_ step

instance AsSteps () where
  step _ = step Empty

instance AsSteps a => AsSteps [a] where
  step = stepMany @a

instance AsSteps Step where
  step s = liftSteps (one s)
  stepMany s = liftSteps (one (overlay s))

instance AsSteps Match where
  step = step . Match
  stepMany = stepMany . map Match

instance (a ~ Int, b ~ Int) => AsSteps (a, b) where
  -- claim an instance over all pairs, but then constrain the valid elements of
  -- them to Player
  step = step . Match . uncurry LowHigh_
  stepMany = stepMany . map (Match . uncurry LowHigh_)

alignWith
  :: (Step -> Step -> Step)
  -> (a -> b -> c)
  -> Steps a
  -> Steps b
  -> Steps c
alignWith combineSteps combineResults (Steps fa) (Steps fb) = Steps \pc -> do
  (sa, a) <- fa pc
  (sb, b) <- fb pc
  pure (Align.alignWith combine sa sb, combineResults a b)
  where
    combine (These l r) = combineSteps l r
    combine (This l) = combineSteps l mempty
    combine (That r) = combineSteps mempty r

interleaves :: [Steps a] -> Steps [a]
interleaves = foldr (\l r -> uncurry (:) <$> alignWith overlay2 (,) l r) (pure mempty)

interleave_ :: [Steps ()] -> Steps ()
interleave_ = foldr (\l r -> void (interleave2 l r)) (pure ())

interleave2 :: Steps a -> Steps b -> Steps (a, b)
interleave2 = alignWith overlay2 (,)

interleave2_ :: Steps () -> Steps () -> Steps ()
interleave2_ l r = void (interleave2 l r)

concatMapSteps :: Steps a -> (Step -> Steps b) -> Steps (a, Seq b)
concatMapSteps (Steps s) f = Steps \pc -> do
  (sa, a) <- s pc
  let Steps next = mapM f sa
  next pc <&> _2 %~ (,) a

concatMapSteps_ :: Steps () -> (Step -> Steps ()) -> Steps ()
concatMapSteps_ (Steps s) f = Steps \pc -> do
  (sa, a) <- s pc
  let Steps next = mapM_ f sa
  next pc

delay :: Int -> Steps ()
delay rounds = replicateM_ rounds (pure ())
