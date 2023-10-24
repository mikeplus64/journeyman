module Tourney.Algebra.Step where

import Control.Monad.Free
import Data.Function.Known
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
  | Overlay (Vector Step)
  | Match Match
  | Empty
  deriving stock (Show, Eq)

data StepMod
  = OverFocus (PlayerCount ~> [Focus])
  | WithSortMethod (MatchResult ~> MatchPoints)
  | WithSwapMethod
  deriving stock (Show, Eq)

instance Monoid Step where
  mempty = Empty

instance Semigroup Step where
  (<>) = overlay2

class IsStep a where
  toStep :: a -> Step

instance IsStep Step where
  toStep = id

instance IsStep Match where
  toStep = Match

instance IsStep a => IsStep [a] where
  toStep = overlay . map toStep

instance IsStep a => IsStep (Vector a) where
  toStep = Overlay . V.filter (\case Empty -> False; _ -> True) . V.map toStep

overlay :: [Step] -> Step
overlay = toStep . V.fromList

overlay2, (|+|) :: Step -> Step -> Step
overlay2 Empty a = a
overlay2 a Empty = a
overlay2 (Overlay a) (Overlay b) = Overlay (a <> b)
overlay2 (Overlay a) b = Overlay (a `V.snoc` b)
overlay2 a (Overlay b) = Overlay (a `V.cons` b)
overlay2 l r = Overlay (V.fromListN 2 [l, r])
(|+|) = overlay2

data PatternDivideFocus = PatternDivideFocus Int
  deriving stock (Show, Eq)

instance KnownFunction PatternDivideFocus PlayerCount [Focus] where
  run (PatternDivideFocus n) count =
    [ Focus (i * len) len
    | i <- [0 .. n - 1]
    ]
      ++ [Focus (count - m) m | m > 0]
    where
      (len, m) = quotRem count n

divide :: Int -> Step -> Step
divide = Modify . OverFocus . KnownFn . PatternDivideFocus

data PatternTileFocus = PatternTileFocus Int
  deriving stock (Show, Eq)

instance KnownFunction PatternTileFocus PlayerCount [Focus] where
  run (PatternTileFocus n) count =
    [ Focus (i * n) n
    | i <- [0 .. len - 1]
    ]
      ++ [Focus (count - m) m | m > 0]
    where
      (len, m) = quotRem count n

tiled :: Int -> Step -> Step
tiled = Modify . OverFocus . KnownFn . PatternTileFocus

--------------------------------------------------------------------------------
-- Simplifying

data Simplifier a
  = Static a
  | WithStandings (Standings -> a)
  deriving stock (Functor)

instance Applicative Simplifier where
  pure = Static
  liftA2 f (Static a) (Static b) = Static (f a b)
  liftA2 f a (WithStandings b) = WithStandings (liftA2 f (asWithStandings a) b)
  liftA2 f (WithStandings a) b = WithStandings (liftA2 f a (asWithStandings b))

instance Monoid a => Monoid (Simplifier a) where
  mempty = Static mempty

instance Semigroup a => Semigroup (Simplifier a) where
  (<>) = liftA2 (<>)

instance Monad Simplifier where
  Static a >>= f = f a
  WithStandings a >>= f = WithStandings \s -> case f (a s) of
    WithStandings g -> g s
    Static a' -> a'

asWithStandings :: Simplifier a -> Standings -> a
asWithStandings (Static a) = const a
asWithStandings (WithStandings a) = a

simplify :: Step -> PlayerCount -> Either [Match] (Standings -> [Match])
simplify step count = case retract (simplifyF step Focus {focusStart = 0, focusLength = count}) of
  Static a -> Left a
  WithStandings f -> Right f

simplifyF :: Step -> Focus -> Free Simplifier [Match]
simplifyF s f = case s of
  Empty -> pure []
  Match m -> pure [m]
  Overlay v -> fold <$> V.mapM (flip simplifyF f) v
  Modify (OverFocus getFocus) s' -> concat <$> mapM (simplifyF s') (run getFocus (focusLength f))
  Modify _ s' -> simplifyF s' f
  ByCount getStep -> simplifyF (run getStep (focusLength f)) f
  ByStandings getStandings -> Free $ WithStandings \standings ->
    simplifyF (run getStandings standings) f
