module Tourney.Algebra.Step where

import Control.Lens.Plated
import Data.Function.Bound
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Tourney.Algebra.Types

data Action = Swap | Score ((Int, Int) ~> U.Vector Int)
  deriving stock (Show)

data Focus = Focus {start, length :: Int}
  deriving stock (Show)

data Step
  = Pattern (PlayerCount ~> Focus) Step
  | ByScores (Vector Player ~> Step)
  | WithSortMethod Action Step
  | Overlay (Vector Step)
  | Match Match
  | Empty
  deriving stock (Show)

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

divide :: Int -> Step -> Step
divide n = Pattern (BoundFn (Dividing n))

newtype Dividing = Dividing Int
  deriving stock (Show, Eq, Ord)

instance IsBoundFn Dividing PlayerCount Focus where
  run (Dividing q) pc = Focus 0 (pc `quot` q)

tiled :: Int -> Step -> Step
tiled n = Pattern (ConstFn Focus {start = 0, length = n})

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
