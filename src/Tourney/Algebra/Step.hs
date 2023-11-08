{-# LANGUAGE NoFieldSelectors #-}

module Tourney.Algebra.Step where

import Control.Lens hiding (Empty)
import Data.Counting
import Data.DList qualified as DL
import Data.Default
import Data.Dependency (MonadRequest (..), StreamM (..))
import Data.Dependency qualified as S
import Data.Function.Known
import Data.Generics.Labels ()
import Data.Tuple.Ordered
import Data.Vector (Vector)
import Data.Vector qualified as V
import Tourney.Match
import Tourney.Types
import VectorBuilder.Builder qualified as VectorBuilder
import VectorBuilder.Vector qualified as VectorBuilder

data Focus = Focus {focusStart, focusLength :: Int}
  deriving stock (Show, Eq, Generic)

data Step
  = Modify StepMod Step
  | ByStandings (Standings -> Step)
  | ByFocus (Focus -> Step)
  | Overlay {-# UNPACK #-} !(Vector Step)
  | Offset Int Step
  | Match Match
  | Empty

data SortMethod
  = WinnerTakesHigh
  | -- | Award points and use that to sort the results in the end
    PointsAward (MatchResult -> MatchResult)

data Sorter = Sorter {-# UNPACK #-} !Focus !SortMethod

data StepMod
  = OverFocus (PlayerCount ~> [Focus])
  | WithSortMethod SortMethod

instance Monoid Step where mempty = Empty

-- | Uses 'overlay2'
instance Semigroup Step where (<>) = overlay2

-- | Overloaded 'Step' constructor.
class IsStep a where toStep :: a -> Step

instance IsStep Step where toStep = id
instance {-# OVERLAPS #-} IsStep Match where toStep = Match
instance {-# OVERLAPS #-} (a ~ Int, b ~ Int) => IsStep (a, b) where toStep = Match . uncurry OrdPair_
instance {-# OVERLAPPABLE #-} (Foldable f, IsStep a) => IsStep (f a) where toStep = optimise1 . foldMap toStep

dependantStep :: IsStep s => (Standings -> s) -> Step
dependantStep f = ByStandings (toStep . f)

-- | Combine many steps by allowing them to run simultaneously
overlay :: [Step] -> Step
overlay = toStep

overlayCountAcc :: CountAcc Step -> Step
overlayCountAcc (Counting len s) = optimise1 (Overlay (V.fromListN len (toList s)))

-- | Combine two steps by allowing them to run simultaneously
overlay2, (|+|) :: Step -> Step -> Step
overlay2 Empty a = a
overlay2 a Empty = a
overlay2 (Overlay a) (Overlay b) = Overlay (a <> b)
overlay2 (Overlay a) b = Overlay (a `V.snoc` b)
overlay2 a (Overlay b) = Overlay (a `V.cons` b)
overlay2 l r = Overlay (V.fromListN 2 [l, r])
(|+|) = overlay2

-- | Simplify the immediate inner structure of a 'Step'. This does not traverse
-- the entire 'Step', but only the outermost node
optimise1 :: Step -> Step
optimise1 (Overlay v0) = case V.length opt of
  0 -> Empty
  1 -> V.head opt
  _ -> Overlay opt
  where
    opt = V.filter (not . isEmpty) v0
    isEmpty Empty = True
    isEmpty (Overlay v) = V.all isEmpty v
    isEmpty (Offset _ s) = isEmpty s
    isEmpty (Modify _ s) = isEmpty s
    isEmpty _ = False
optimise1 s = s

overFocus :: (Focus -> Step) -> Step
overFocus = ByFocus

--------------------------------------------------------------------------------
-- Compiling a step

instance Default SortMethod where
  def = WinnerTakesHigh

data CompiledStep = CompiledStep
  { matches :: forall m. MonadRequest Standings m => StreamM m (Sorter, Vector Match) ()
  , source :: Step
  }

getMatches :: MonadRequest Standings m => CompiledStep -> StreamM m (Sorter, Vector Match) ()
getMatches CompiledStep{matches} = matches

data PureCompiledStep = PureCompiledStep
  { matches :: [(Sorter, Vector Match)]
  , source :: Step
  , isComplete :: !Bool
  }

-- | Compile a 'Step' into a list of matches that it produces, and a function
-- that, given current standings, generates a list of matches
compile :: Step -> PlayerCount -> CompiledStep
compile step count =
  let !focus0 = Focus{focusStart = 0, focusLength = count}
      !method0 = Sorter focus0 def
      !state0 = CompileState{sortMethod = method0, focus = focus0}
  in  CompiledStep
        { matches = (compact method0 . stream step) state0
        , source = step
        }
  where
    -- Simplify the steps to the matches it yields, or the sort method set
    stream :: MonadRequest Standings m => Step -> CompileState -> StreamM m (Either Sorter Match) ()
    stream = \case
      Empty ->
        pure mempty
      Offset o s ->
        S.map (_Right %~ offset o) <$> stream s
      Match m ->
        pure (S.yield (Right m))
      Overlay v ->
        fold <$> V.mapM stream v
      Modify (OverFocus getFocus) inner -> do
        focii <- run getFocus <$> view (#focus . #focusLength)
        results <- forM focii \f -> stream inner . set #focus f
        pure (fold results)
      Modify (WithSortMethod method) s -> \state0 -> do
        S.yield (Left (Sorter (state0 ^. #focus) method))
        stream s state0
      ByStandings f -> \state0 -> do
        standings <- S.await
        stream (f standings) state0
      ByFocus f -> \state0 -> do
        stream (f (state0 ^. #focus)) state0

    -- Then combine adjacent matches together and group them by sort method
    compact
      :: MonadRequest Standings m
      => Sorter
      -> StreamM m (Either Sorter Match) ()
      -> StreamM m (Sorter, Vector Match) ()
    compact method aw@(x :< xs) = case x of
      Left method1 -> compact method1 xs
      Right{} -> do
        let (matches, next) = accumMatches aw
        S.yield (method, VectorBuilder.build matches)
        compact method next
        where
          accumMatches (Got (Right m) ms) = (VectorBuilder.singleton m, ms)
          accumMatches (Cat l r) =
            let !(lacc, l') = accumMatches l
                !(racc, r') = accumMatches r
            in  (lacc <> racc, Cat l' r')
          accumMatches next = (mempty, next)
    compact _ _ = pure ()

data CompileState = CompileState
  { sortMethod :: {-# UNPACK #-} !Sorter
  , focus :: {-# UNPACK #-} !Focus
  }
  deriving stock (Generic)

-- | Compile the step into a list of matches. If there are any matches that
-- depend on standings to be yielded, they will not be present in the result.
compilePure :: Step -> PlayerCount -> [(Sorter, Vector Match)]
compilePure step =
  -- Fix the specific MonadRequest to ((->) Standings) since that can be
  -- executed purely
  DL.toList . S.peekFoldMap DL.singleton . getMatches @((->) Standings) <$> compile step

pureMatches :: CompiledStep -> ([(Sorter, Vector Match)], Bool)
pureMatches cs =
  (DL.toList matchBuilder, isComplete)
  where
    (matchBuilder, isComplete) = build (getMatches @((->) Standings) cs)
    build = \case
      Got x xs -> (DL.singleton x <> xs', completeXS)
        where
          (!xs', !completeXS) = build xs
      Cat l r -> (l' <> r', completeL && completeR)
        where
          (!l', !completeL) = build l
          (!r', !completeR) = build r
      Done _ -> (mempty, True)
      Wait _ -> (mempty, False)
