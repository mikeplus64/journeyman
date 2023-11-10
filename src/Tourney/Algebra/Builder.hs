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

newtype Builder t a = Builder (Writer (Tournament t) a)
  deriving (Functor, Applicative, Monad, MonadWriter (Tournament t)) via (Writer (Tournament t))

tellBuilder :: Tournament t -> Builder t ()
tellBuilder t = Builder (tell t)

runBuilder :: Builder t a -> (a, Tournament t)
runBuilder (Builder b) = runWriter b

--------------------------------------------------------------------------------
-- Steps

-- | A monad to build tournaments over steps (See '(***)'). Operations in this
-- monad create rounds. See 'Round' for a builder monad to create rounds.
type Steps = Builder TMany

runSteps :: Steps a -> (a, Tournament TMany)
runSteps = runBuilder

execSteps :: Steps a -> Tournament TMany
execSteps = snd . runBuilder

tellSteps :: Tournament TMany -> Steps ()
tellSteps t = Builder (tell t)

class AsSteps a x | a -> x where toSteps :: a -> Steps x
instance AsSteps (Steps x) x where toSteps = id
instance AsSteps a () => AsSteps [a] () where toSteps = mapM_ (toSteps @a)

-- Basic syntax for 'Steps'
----------------------------------------

-- | Add a round to steps, giving the resulting values
round :: Round a -> Steps a
round r = a <$ tellSteps (LiftTMany t)
  where
    (a, t) = runRound r

-- | Add a round to steps
round_ :: Round a -> Steps ()
round_ = void . round

-- | Add some rounds to steps
rounds_ :: [Round x] -> Steps ()
rounds_ = mapM_ round_

class AsRound a x | a -> x where toRound :: a -> Round x
instance AsRound (Round x) x where toRound = id
instance AsRound Match () where toRound = match
instance AsRound a () => AsRound [a] () where
  toRound = overlays_ . map (toRound @a) . toList

--------------------------------------------------------------------------------
-- Rounds

-- | A monad to build tournaments over matches. For convenience, this monad
-- behaves like a list monad, where the inner list is ultimately combined by
-- 'U.overlay'.
type Round = Builder TOne

runRound :: Round a -> (a, Tournament TOne)
runRound = runBuilder

tellRound :: Tournament TOne -> Round ()
tellRound t = Builder (tell t)

-- Basic syntax for 'Round'
----------------------------------------

class AsMatch m where toMatch :: m -> Match
instance (x ~ Int, y ~ Int) => AsMatch (x, y) where toMatch = uncurry OrdPair_
instance AsMatch Match where toMatch = id

-- | Add a match
match :: AsMatch m => m -> Round ()
match m = tellRound (One (toMatch m))

--------------------------------------------------------------------------------
-- Shared syntax for both

-- | Overlay a collection of builders
overlays :: Foldable f => f (Builder t a) -> Builder t [a]
overlays bs =
  Builder
    . writer
    . second U.overlay
    $ unzip (map runBuilder (toList bs))

-- | Overlay a collection of builders
overlays_ :: Foldable f => f (Builder t a) -> Builder t ()
overlays_ = void . overlays

-- -- | Retrieve the current standings
-- --
-- -- Warning: this operation will have the effect of segmenting the tournament
-- -- into two sections, as a tournament runner cannot continue past this point
-- -- without first having the standings.
-- standings :: Builder t () Standings
-- standings = Builder $ ContT \f -> do
--   add (ByStandings \s -> overlayAccumT (f s))

-- -- | Retrieve the current focus
-- focus :: Builder t () Focus
-- focus = Builder $ ContT \f -> do
--   add (ByFocus \s -> overlayAccumT (f s))

-- -- | Retrieve the current player count
-- playerCount :: Builder t () PlayerCount
-- playerCount = Builder $ ContT \f -> do
--   add (ByPlayerCount \s -> overlayAccumT (f s))

-- Combinators
---------------

-- -- | Set the focus of the inner builder
-- withFocus :: Merge t => Int -> Int -> Builder t a a -> Builder t ()
-- withFocus focusStart focusLength = withFocii (const [Focus{focusStart, focusLength}])

-- -- | Set the focii of the inner builder
-- withFocii :: Merge t => (Focus -> [Focus]) -> Builder t a a -> Builder t ()
-- withFocii makeFocii b = Builder do
--   let (_as, ts) = unzip (runBuilder id b)
--   lift (add (Modify (SetFocus makeFocii) (merge ts)))

-- -- | Set the sort method of the inner builder
-- withSortMethod :: Merge t => SortMethod -> Builder t a a -> Builder t ()
-- withSortMethod method b = Builder do
--   let (_as, ts) = unzip (runBuilder id b)
--   lift (add (Modify (SetSortMethod method) (merge ts)))

-- -- | Set the offset of the inner builder
-- withOffset :: Merge t => Int -> Builder t a a -> Builder t ()
-- withOffset offs b = Builder do
--   let (_as, ts) = unzip (runBuilder id b)
--   lift (add (Modify (SetOffset offs) (merge ts)))

-- | Given a denominator @n@ and a builder, split the tournament into @n@
-- sub-tournaments and run the builder on each
divideInto :: Merge t => Int -> Builder TMany () -> Builder TMany ()
divideInto denom builder = tellBuilder $ ByPlayerCount \count -> do
  let (m, r) = quotRem count denom
  let groupSize = m + r
  U.overlay
    [ Modify (SetFocus (\_ -> [Focus (groupNo * groupSize) groupSize])) (execSteps builder)
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

class Merge (t :: Depth) where merge :: [Tournament t] -> Tournament t
instance Merge TOne where merge = U.overlay
instance Merge TMany where merge = U.sequence
