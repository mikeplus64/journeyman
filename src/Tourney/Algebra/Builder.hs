{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Tourney.Algebra.Builder where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Trans.Accum
import Control.Monad.Writer
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Dependency qualified as S
import Data.Tuple.Ordered
import Data.Vector qualified as V
import Tourney.Algebra.Unified as U
import Tourney.Arith
import Tourney.Match
import Tourney.Types
import Prelude hiding (round, sequence)

newtype Builder t r a = Builder (ContT r (AccumT (Tournament t) DList) a)
  deriving (Functor, Applicative, Monad) via (ContT r (AccumT (Tournament t) DList))

tellBuilder :: Tournament t -> Builder t r ()
tellBuilder t = Builder (lift (add t))

runBuilder :: (a -> r) -> Builder t r a -> [(r, Tournament t)]
runBuilder f b = DL.toList (runBuilderDL f b)

--------------------------------------------------------------------------------
-- Steps

-- | A monad to build tournaments over steps (See '(***)'). Operations in this
-- monad create rounds. See 'Round' for a builder monad to create rounds.
type Steps = Builder TMany

runSteps :: (a -> r) -> Steps r a -> ([r], Tournament TMany)
runSteps f = second U.sequence . unzip . runBuilder f

execSteps :: Steps a a -> Tournament TMany
execSteps = U.sequence . map snd . runBuilder id

tellSteps :: Tournament TMany -> Steps a ()
tellSteps t = Builder (lift (add t))

class AsSteps a x | a -> x where toSteps :: a -> Steps x x
instance x ~ y => AsSteps (Steps x y) x where toSteps = id
instance AsSteps a () => AsSteps [a] () where toSteps = mapM_ (toSteps @a)

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

class AsRound a x | a -> x where toRound :: a -> Round x x
instance x ~ y => AsRound (Round x y) x where toRound = id
instance AsRound Match () where toRound = match
instance AsRound a () => AsRound [a] () where
  toRound = overlays_ . map (toRound @a) . toList

--------------------------------------------------------------------------------
-- Rounds

-- | A monad to build tournaments over matches. For convenience, this monad
-- behaves like a list monad, where the inner list is ultimately combined by
-- 'U.overlay'.
type Round = Builder TOne

runRound :: (a -> r) -> Round r a -> ([r], Tournament TOne)
runRound f = second U.overlay . unzip . runBuilder f

tellRound :: Tournament TOne -> Round a ()
tellRound t = Builder (lift (add t))

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
overlaysBy :: Foldable f => (a -> x) -> f (Builder t x a) -> Builder t r [x]
overlaysBy f b = Builder $ ContT \ret -> do
  let r = map (runBuilder f) (toList b)
  add (U.overlay (concatMap (map snd) r))
  ret (concatMap (map fst) r)

-- | Overlay a collection of builders
overlays :: Foldable f => f (Builder t a a) -> Builder t r [a]
overlays = overlaysBy id

-- | Overlay a collection of builders
overlays_ :: Foldable f => f (Builder t a a) -> Builder t r ()
overlays_ = void . overlays

-- | Retrieve the current standings
--
-- Warning: this operation will have the effect of segmenting the tournament
-- into two sections, as a tournament runner cannot continue past this point
-- without first having the standings.
standings :: Builder t () Standings
standings = Builder $ ContT \f -> do
  add (ByStandings \s -> overlayAccumT (f s))

-- | Retrieve the current focus
focus :: Builder t () Focus
focus = Builder $ ContT \f -> do
  add (ByFocus \s -> overlayAccumT (f s))

-- | Retrieve the current player count
playerCount :: Builder t () PlayerCount
playerCount = Builder $ ContT \f -> do
  add (ByPlayerCount \s -> overlayAccumT (f s))

-- Combinators
---------------

-- | Set the focus of the inner builder
withFocus :: Merge t => Int -> Int -> Builder t a a -> Builder t r ()
withFocus focusStart focusLength = withFocii (const [Focus{focusStart, focusLength}])

-- | Set the focii of the inner builder
withFocii :: Merge t => (Focus -> [Focus]) -> Builder t a a -> Builder t r ()
withFocii makeFocii b = Builder do
  let (_as, ts) = unzip (runBuilder id b)
  lift (add (Modify (SetFocus makeFocii) (merge ts)))

-- | Set the sort method of the inner builder
withSortMethod :: Merge t => SortMethod -> Builder t a a -> Builder t r ()
withSortMethod method b = Builder do
  let (_as, ts) = unzip (runBuilder id b)
  lift (add (Modify (SetSortMethod method) (merge ts)))

-- | Set the offset of the inner builder
withOffset :: Merge t => Int -> Builder t a a -> Builder t r ()
withOffset offs b = Builder do
  let (_as, ts) = unzip (runBuilder id b)
  lift (add (Modify (SetOffset offs) (merge ts)))

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

--------------------------------------------------------------------------------
-- Example

roundRobin :: Int -> [[Match]]
roundRobin count =
  let (!midpoint, !r) = count `quotRem` 2
      !n = count + r
  in  [ collapseMatches midpoint (0 : ((n - i) ..< n) ++ (1 ..< (n - i)))
      | i <- [0 .. n - 2]
      ]

xxx f = do
  let t = compile @IO f
  let m = runTourney t noStandings (Focus 0 8)
  S.toVector m >>= V.mapM \rs -> do
    rounds <- S.toVector rs
    V.forM rounds \(sorter, matches) ->
      (sorter,) <$> S.toVector matches

-- Just (round0, _) <- S.forcePop (S.pop m)
-- Just ((sorter, matchGroup0), _) <- S.forcePop (S.pop round0)
-- Just (matches0, _) <- S.forcePop (S.pop matchGroup0)
-- _ matches0

-- | Fold a list of players together around a midpoint of the list.
collapseMatches :: Int -> [Player] -> [Match]
collapseMatches midpoint players = take midpoint (zipWith OrdPair_ players (reverse players))

-- -- | Divide a tournament into 'n' groups, and perform a round-robin within each
-- -- group.
-- groupRoundRobin :: Monad m => Int -> Steps () ()
-- groupRoundRobin numGroups =
--   divideInto numGroups roundRobin

--------------------------------------------------------------------------------
-- Internals

overlayAccumT :: AccumT (Tournament t) DList x -> Tournament t
overlayAccumT a =
  trace "overlayAccumT" $
    U.overlay . DL.toList . flip execAccumT mempty $
      a

runBuilderDL :: (a -> r) -> Builder t r a -> DList (r, Tournament t)
runBuilderDL f (Builder run) = runAccumT (runContT run (\x -> pure (f x))) mempty

class Merge (t :: Depth) where merge :: [Tournament t] -> Tournament t
instance Merge TOne where merge = U.overlay
instance Merge TMany where merge = U.sequence
