{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Tourney.Algebra where

import Control.Lens
import Data.Tuple.Ordered
import Tourney.Algebra.Types

-- import BasePrelude (showsPrec)
-- import Control.Lens hiding (Const)
-- import Control.Monad.Reader
-- import Control.Monad.Writer.Strict
-- import Data.Bits
-- import Data.Counting
-- import Data.List ((!!))
-- import Data.These
-- import Data.Tuple.Ordered
-- import Data.Typeable (typeRep)
-- import Data.Vector (Vector)
-- import Data.Vector qualified as V
-- import Prelude hiding (Const, round)

-- type Player = Int

-- type Match = LowHigh Player

-- type PlayerCount = Int

-- type RoundNo = Int

-- data Action
--   = Swap
--   | Score
--   deriving stock (Eq, Ord, Show)

-- data FocusPattern = FocusPattern
--   { start :: Int
--   , length :: Int
--   }

-- type Rounds = Counting [Step]

-- newtype Steps a = Steps (PlayerCount -> Rounds -> (a, Rounds))
--   deriving
--     (Functor, Applicative, Monad, MonadReader PlayerCount)
--     via (ReaderT PlayerCount (State Rounds))

-- instance MonadState [Step] Steps where
--   get = Steps \_ r@(Counting _ s) -> (s, r)
--   put a = Steps \_ _ -> ((), counting a)

-- data a ~> b
--   = Fn (a -> b)
--   | Const b
--   deriving stock (Functor)

-- run :: (a ~> b) -> a -> b
-- run (Fn f) a = f a
-- run (Const b) _ = b

-- instance (Typeable a, Typeable b) => Show (a ~> b) where
--   showsPrec p _ = showsPrec p (typeRep (Proxy :: Proxy (a -> b)))

-- data Step
--   = Pattern (PlayerCount ~> FocusPattern) Step
--   | ByScores (Vector Player ~> Step)
--   | WithSortMethod Action Step
--   | Overlay (Vector Step)
--   | Negate Step
--   | Match Match
--   | E
--   deriving stock (Show)

-- newtype CompiledSteps = CompiledSteps (Vector (Vector Step))

-- narrow :: Int -> Step -> Step
-- narrow n = Pattern (Fn \pc -> FocusPattern {start = 0, length = pc `quot` n})

-- tiled :: Int -> Step -> Step
-- tiled n = Pattern (Const FocusPattern {start = 0, length = n})

-- interleave :: [Steps a] -> Steps [a]
-- interleave = foldr (\l r -> uncurry (:) <$> interleave2 l r) (pure mempty)

-- interleave_ :: [Steps ()] -> Steps ()
-- interleave_ = foldr (\l r -> void (interleave2 l r)) (pure ())

-- interleave2 :: Steps a -> Steps b -> Steps (a, b)
-- interleave2 (Steps fa) (Steps fb) = Steps \pc rounds ->
--   let (a, sa) = fa pc rounds
--       (b, sb) = fb pc rounds
--    in ((a, b), alignCounting combine sa sb)
--   where
--     combine (These l r) = overlay2 l r
--     combine (This l) = l
--     combine (That r) = r

-- interleave2_, (*|*) :: Steps () -> Steps () -> Steps ()
-- interleave2_ l r = void (interleave2 l r)
-- (*|*) = interleave2_

-- delay :: Int -> Steps a -> Steps a
-- delay rounds s = do
--   replicateM_ rounds (step ())
--   s

-- concatMapSteps :: (Step -> Steps b) -> Steps a -> Steps (a, [b])
-- concatMapSteps f (Steps s) = Steps \pc rounds ->
--   let (a, Counting _ sa) = s pc rounds
--       Steps next = mapM f sa
--    in next pc mempty & _1 %~ (,) a

-- modifySteps :: (Step -> Steps ()) -> Steps ()
-- modifySteps f = Steps \pc (Counting _ rounds) ->
--   let Steps next = mapM_ f rounds
--    in next pc mempty

-- overlay :: [Step] -> Step
-- overlay steps =
--   asOverlay do
--     s <- V.fromList steps
--     case s of
--       Overlay o -> o
--       E -> V.empty
--       a -> V.singleton a
--   where
--     asOverlay v = case V.length v of
--       0 -> E
--       1 -> V.head v
--       _ -> Overlay v

-- overlay2, (|+|) :: Step -> Step -> Step
-- overlay2 E a = a
-- overlay2 a E = a
-- overlay2 l r = Overlay (V.fromListN 2 [l, r])
-- (|+|) = overlay2

-- class AsSteps s where
--   step :: AsSteps s => s -> Steps ()
--   step = stepMany . one

--   stepMany :: AsSteps s => [s] -> Steps ()
--   stepMany = mapM_ step

-- instance AsSteps () where
--   step _ = step E

-- instance AsSteps a => AsSteps [a] where
--   step = stepMany @a

-- instance AsSteps Step where
--   step s = modify' (<> one s)
--   stepMany s = modify' (<> one (overlay s))

-- instance AsSteps Match where
--   step = step . Match
--   stepMany = stepMany . map Match

-- instance (a ~ Int, b ~ Int) => AsSteps (a, b) where
--   -- claim an instance over all pairs, but then constrain the valid elements of
--   -- them to Player
--   step = step . Match . uncurry LowHigh_
--   stepMany = stepMany . map (Match . uncurry LowHigh_)

-- singleElimination :: Steps ()
-- singleElimination = do
--   players <- ask
--   let depth = bitLog2 (nearestPow2Above players)
--   step (slaughterSeeding !! depth)
--   sequence_ do
--     round <- [1 .. depth]
--     pure (step (stride2 <$> fromPow2 round))

-- doubleElimination :: Steps ()
-- doubleElimination = do
--   depth <- asks (bitLog2 . nearestPow2Above)
--   singleElimination
--   modifySteps \singleElimStage -> do
--     step singleElimStage

-- slaughterSeeding :: [[Match]]
-- slaughterSeeding =
--   map snd (iterate next (2 :: Word32, [LowHigh_ 0 1]))
--   where
--     next :: (Word32, [Match]) -> (Word32, [Match])
--     next (!d, matches) = (d + 1,) do
--       p <- toList =<< matches
--       [LowHigh_ p (2 ^ d - p - 1)]

slaughterSeeding :: [[Match]]
slaughterSeeding = [LowHigh_ 0 1] : imap next slaughterSeeding2
  where
    next d a = [LowHigh_ p (2 ^ (d + 2) - p - 1) | p <- toList =<< a]

-- next (!d, matches) = (d + 1,) do
--   p <- toList =<< matches
--   [LowHigh_ p (2 ^ d - p - 1)]
