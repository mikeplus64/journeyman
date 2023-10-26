{-# LANGUAGE FieldSelectors #-}

module Tourney.Algebra.Step where

import Control.Monad.Free
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Dependency
import Data.Function.Known
import Data.Tuple.Ordered
import Data.Vector (Vector)
import Data.Vector qualified as V
import Tourney.Algebra.Types
import Tourney.Match

data Focus = Focus {focusStart, focusLength :: Int}
  deriving stock (Show, Eq)

data Step
  = Modify StepMod Step
  | ByStandings (Standings ~> Step)
  | ByCount (PlayerCount ~> Step)
  | Overlay {-# UNPACK #-} !(Vector Step)
  | Offset Int Step
  | Match Match
  | Empty
  deriving stock (Show, Eq)

data StepMod
  = OverFocus (PlayerCount ~> [Focus])
  | WithSortMethod (MatchResult ~> MatchPoints)
  | WithSwapMethod
  deriving stock (Show, Eq)

instance Monoid Step where mempty = Empty

-- | Uses 'overlay2'
instance Semigroup Step where (<>) = overlay2

-- | Overloaded 'Step' constructor.
class IsStep a where toStep :: a -> Step

instance IsStep Step where toStep = id
instance {-# OVERLAPS #-} IsStep Match where toStep = Match
instance {-# OVERLAPS #-} (a ~ Int, b ~ Int) => IsStep (a, b) where toStep = Match . uncurry LowHigh_
instance {-# OVERLAPPABLE #-} (Foldable f, IsStep a) => IsStep (f a) where toStep = optimise . foldMap toStep

-- | Combine many steps by allowing them to run simultaneously
overlay :: [Step] -> Step
overlay = toStep

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
optimise :: Step -> Step
optimise (Overlay v0) = case V.length filtered of
  0 -> Empty
  1 -> V.head filtered
  _ -> Overlay filtered
  where
    filtered = V.filter (not . isEmpty) v0
    isEmpty Empty = True
    isEmpty (Overlay v) = V.all isEmpty v
    isEmpty (Offset _ s) = isEmpty s
    isEmpty (Modify _ s) = isEmpty s
    isEmpty _ = False
optimise s = s

--------------------------------------------------------------------------------
-- "Known" functions for manipulating steps (by 'ByStandings' or 'ByCount')

newtype PatternDivideFocus = PatternDivideFocus Int
  deriving stock (Show, Eq)

instance KnownFunction PatternDivideFocus PlayerCount [Focus] where
  run (PatternDivideFocus n) count =
    [ Focus (i * len) len
    | i <- [0 .. n - 1]
    ]
      ++ [Focus (count - m) m | m > 0]
    where
      (len, m) = quotRem count n

-- | "Divide" a step; apply it over a focus that is (1/operand) the size of the
-- outer focus
divide
  :: Int
  -- ^ The denominator to use
  -> Step
  -> Step
divide = Modify . OverFocus . KnownFn . PatternDivideFocus

newtype PatternTileFocus = PatternTileFocus Int
  deriving stock (Show, Eq)

instance KnownFunction PatternTileFocus PlayerCount [Focus] where
  run (PatternTileFocus n) count =
    [ Focus (i * n) n
    | i <- [0 .. len - 1]
    ]
      ++ [Focus (count - m) m | m > 0]
    where
      (len, m) = quotRem count n

-- | "Tile" a step; apply it over a focus of the given size
tiled
  :: Int
  -- ^ The tile size to use
  -> Step
  -> Step
tiled = Modify . OverFocus . KnownFn . PatternTileFocus

--------------------------------------------------------------------------------
-- Compiling a step

data CompiledStep m = CompiledStep
  { matches :: AwaitingM m [Match] () -- Laziness here is intentional
  , source :: Step
  }

-- | Compile a 'Step' into a list of matches that it produces, and a function
-- that, given current standings, generates a list of matches
compile :: (MonadRequest Standings m, HasPlayerCount i) => Step -> i (CompiledStep m)
compile step = do
  count <- getPlayerCount
  let focus0 = Focus {focusStart = 0, focusLength = count}
  pure
    CompiledStep
      { matches = unAwaiting (DL.toList <$> Awaiting (awaiting (retract (simplifyF step focus0))))
      , source = step
      }
  where
    doOffset :: Int -> Free (DependantStream Standings) (DList Match) -> Free (DependantStream Standings) (DList Match)
    doOffset o = fmap (fmap (offset o))

    simplifyF :: Step -> Focus -> Free (DependantStream Standings) (DList Match)
    simplifyF s f = case s of
      Empty -> pure mempty
      Offset o s' -> doOffset o (simplifyF s' f)
      Match m -> pure (DL.singleton m)
      Overlay v -> fold <$> V.mapM (`simplifyF` f) v
      Modify (OverFocus getFocus) s' ->
        DL.concat
          <$> mapM
            (simplifyF s')
            (run getFocus (focusLength f))
      Modify _ s' -> simplifyF s' f
      ByCount getStep -> simplifyF (run getStep (focusLength f)) f
      ByStandings getStandings -> Free $ NeedS \standings ->
        pure (simplifyF (run getStandings standings) f)

-- | Compile the step into a list of matches. If there are any matches that
-- depend on standings to be yielded, they will not be present here in the final
-- list here
compilePure :: HasPlayerCount m => Step -> m [Match]
compilePure step = DL.toList . go . matches <$> compile @((->) Standings) step
  where
    go :: AwaitingM ((->) Standings) [Match] () -> DList Match
    go (Done _) = DL.empty
    go (Got x xs) = DL.fromList x <> go xs
    go (Cat l r) = go l <> go r
    go _ = mempty

class Monad m => HasPlayerCount m where
  getPlayerCount :: m PlayerCount

instance HasPlayerCount ((->) PlayerCount) where
  getPlayerCount = id

instance Monad m => HasPlayerCount (ReaderT PlayerCount m) where
  getPlayerCount = ask
