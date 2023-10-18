{-# LANGUAGE TemplateHaskell #-}

module Tourney.Algebra where

import Control.Lens

import Control.Monad.Free

-- import Control.Monad.Trans.Free
import Control.Monad.Writer.Strict
import Data.Bits
import Data.Default
import Data.Fix
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Generics.Labels ()
import Data.Tuple.Ordered
import Data.Vector (Vector)
import Data.Vector qualified as V
import Prelude hiding (round)

type Player = Int

type Match = LowHigh Player

type PlayerCount = Int

type RoundNo = Int

data Action
  = Swap
  | Score ((Int, Int) -> [Int])

data Focus = Focus
  { start :: Int
  , length :: Int
  }

data Step
  = Pattern (PlayerCount -> Focus) Step
  | Overlay Step Step
  | Match Match
  | EmptyS

data Round
  = Dynamic (Vector Player -> Round)
  | Static Step

makeBaseFunctor ''Step

mapRound :: (Step -> Step) -> Round -> Round
mapRound f (Static a) = Static (f a)
mapRound f (Dynamic dyn) = Dynamic (fmap (mapRound f) dyn)

foldMapRound :: (Step -> Round) -> Round -> Round
foldMapRound f (Static a) = f a
foldMapRound f (Dynamic dyn) = Dynamic (foldMapRound f . dyn)

newtype Steps a = Steps ([Round], a)
  deriving newtype (Functor, Applicative, Monad)

-- ([Round], a)
-- == forall r. ([Round] -> a -> r) -> r
-- == forall r. (forall x. (Round -> x -> x) -> x -> a -> r) -> r

-- newtype Steps a = Steps
--   { runSteps :: [Seeding Step]
--   }
--   deriving (Functor, Applicative, Monad) via ([] `Compose` (,) Round)

-- newtype CompiledSteps = CompiledSteps (Fix (StepDelineation `Compose` (,) Step))

-- cons :: Step -> CompiledSteps -> CompiledSteps
-- cons x (CompiledSteps xs) = CompiledSteps (Fix (Compose (Static (x, xs))))

-- compiledStepsFromList :: [Step] -> CompiledSteps
-- compiledStepsFromList =
--   CompiledSteps . unfoldFix \case
--     x : xs -> Compose (Static (x, xs))
--     [] -> Compose Done

-- compile :: Steps () -> CompiledSteps
-- compile (Steps sf) = case sf of
--   Pure _ -> CompiledSteps (Fix (Compose Done))
--   Free (Compose sd) -> CompiledSteps . Fix . Compose $ case sd of
--     Dynamic dyn -> undefined
--     Static (Compose stats) -> undefined
-- Done -> Done

-- data Steps a
--   = Then (Steps a) (Fix StepF)
--   | Dynamic (Vector Player -> RoundNo -> Steps a)

-- compileStep :: Step a -> [Fix StepF]
-- compileStep (Step (FreeT fs)) = do
--   f <- fs
--   case f of
--     Free s -> case fmap (compileStep . Step) s of
--       PatternF pat a -> Fix . PatternF pat <$> a
--       DynamicF f -> _
--       ZipF a b -> _
--       MatchF m -> _
--       EmptyF -> _
--     Pure _ -> []

-- instance Semigroup (Step a) where
--   Step a <> Step b = Step (a <> b)

-- instance Monoid (Step a) where
--   mempty = Step mempty

-- pattern FF1 :: f (FreeT f [] a) -> FreeT f [] a
-- pattern FF1 a = FreeT [Free a]

-- pattern Pattern1 :: (PlayerCount -> Focus) -> Step a -> Step a
-- pattern Pattern1 f s <- Step (FF1 (PatternF f (coerce -> s)))
--   where
--     Pattern1 f s = Step (FF1 (PatternF f (coerce s)))

-- pattern Dynamic1 :: (RoundNo -> Focus -> Vector Player -> Step a) -> Step a
-- pattern Dynamic1 f <- Step (FF1 (DynamicF (coerce -> f)))
--   where
--     Dynamic1 f = Step (FF1 (DynamicF (coerce f)))

-- -- pattern Overlay1 :: Vector (Step a) -> Step a
-- -- pattern Overlay1 v <- Step (FF1 (OverlayF (coerce -> v)))
-- --   where
-- --     Overlay1 v = Step (FF1 (OverlayF (coerce v)))

-- -- pattern OverlayL1 :: [Step a] -> Step a
-- -- pattern OverlayL1 v <- Step (FF1 (OverlayF (coerce . V.toList -> v)))
-- --   where
-- --     OverlayL1 v = Step (FF1 (OverlayF (coerce (V.fromList v))))

-- pattern Match1 :: LowHigh Player -> Step a
-- pattern Match1 v = Step (FF1 (MatchF v))

-- pattern Empty1 :: Step a
-- pattern Empty1 = Step (FF1 EmptyF)

-- -- data TournamentBuilderState = TournamentBuilderState
-- --   { rounds :: [(Action, Step ())]
-- --   }
-- --   deriving stock (Generic)

-- newtype TournamentBuilder a
--   = TB [((Action, [Free StepF ()]), a)]
--   deriving (Functor, Applicative, Monad) via (Compose [] ((,) (Action, Step ())))

-- matches :: [Match] -> Step ()
-- matches = overlay >=> Match1

-- instance Semigroup (TournamentBuilder a) where
--   TB (StateT f) <> TB (StateT g) = TB $ StateT \s ->
--     let x = f s
--         y = g s
--      in x ++ y

-- class MonadList m where
--   list :: [a] -> m a

-- overlay :: [a] -> Step a
-- overlay = Step . lift

-- --------------------------------------------------------------------------------

-- -- singleElimination :: PlayerCount -> TournamentBuilder ()
-- -- singleElimination count =
-- --   firstRound <> subsequentRounds
-- --   where
-- --     firstRound = step (matches (slaughterSeeding depth))
-- --     subsequentRounds = do
-- --       round <- list [1 .. depth]
-- --       step do
-- --         i <- list (reverse [0 .. 2 ^ (round - 1)])
-- --         Match1 (LowHigh_ (2 * i) (2 * i + 1))
-- --     n = bitCeiling count
-- --     depth = bitLog2 n

-- slaughterSeeding :: Int -> [Match]
-- slaughterSeeding n | n <= 0 = []
-- slaughterSeeding 1 = [LowHigh_ 0 1]
-- slaughterSeeding d = [LowHigh_ p (2 ^ d - p - 1) | hl <- slaughterSeeding (d - 1), p <- toList hl]

-- bitCeiling :: Int -> Int
-- bitCeiling n =
--   if popCount n == 1
--     then n
--     else bit (bitLog2 n + 1)

-- bitLog2 :: Int -> Int
-- bitLog2 n = finiteBitSize n - countLeadingZeros n - 1
