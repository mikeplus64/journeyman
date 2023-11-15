{-# LANGUAGE PartialTypeSignatures #-}

module Tourney.Algebra.Unified (
  -- * Base eDSL
  Tournament (..),
  Mod (..),
  (+++),
  overlay,
  (***),
  sequence,

  -- * Types
  SortMethod (..),
  Sorter (..),
  Focus (..),
  Depth (..),
  KnownDepth (..),
) where

import Data.Generics.Labels ()
import Data.Typeable (cast)
import Text.Show qualified as Show
import Tourney.Common
import Tourney.Match
import Prelude hiding (Empty, sequence)

-- | A description of a tournament. The type-parameter gives an upper-bound on
-- how many rounds this tournament has. It also ensures that the "spine" of a
-- tournament is static, regardless of use of constructors such as
-- 'ByStandings', that enable creating tournaments that depend on match results
-- that are not yet known.
data Tournament :: Depth -> Type where
  One :: !Match -> Tournament 'TOne
  Empty :: Tournament t
  -- | Modify a tournament by some 'Mod'
  Modify :: !Mod -> Tournament t -> Tournament t
  -- | Overlay two tournaments, to describe running two sub-tournaments in
  -- parallel. The depth of the tournaments must be the same
  Overlay :: !(Tournament a) -> !(Tournament a) -> Tournament a
  -- | Sequence two tournaments one after the other. The resulting tournament
  -- has a depth that is the sum of the two depths of the input tournaments.
  Sequence :: (KnownDepth a, KnownDepth b) => !(Tournament a) -> !(Tournament b) -> Tournament TMany
  LiftTMany :: KnownDepth t => Tournament t -> Tournament TMany
  -- | Depend on the player count to produce an inner tournament
  ByPlayerCount :: !(PlayerCount -> Tournament t) -> Tournament t
  -- | Depend on the current focus of the tournament to produce an inner
  -- tournament
  ByFocus :: !(Focus -> Tournament t) -> Tournament t
  -- | Depend on the current standings, at the outset of the current round, to
  -- run the tournament.
  ByStandings :: !(Standings -> Tournament t) -> Tournament t

instance Monoid (Tournament t) where mempty = Empty
instance Semigroup (Tournament t) where (<>) = (+++)

instance Show (Tournament t) where
  showsPrec p = \case
    Sequence a b -> Show.showParen True (Show.showsPrec 9 a . Show.showString " *** " . Show.showsPrec 9 b)
    Overlay a b -> Show.showParen True (Show.showsPrec 9 a . Show.showString " +++ " . Show.showsPrec 9 b)
    One a -> Show.showsPrec p a
    Empty -> Show.showString "Empty"
    LiftTMany t -> Show.showParen True (Show.showString "Lt " . Show.showsPrec 9 t)
    _ -> Show.showString "_"

data Mod
  = SetFocus !(Focus -> [Focus])
  | SetOffset !Int
  | SetSortMethod !SortMethod

-- | The depth of a tournament. Since we only care about distinguishing single
-- rounds from sequences of rounds, that is the only information stored here.
data Depth = TOne | TMany

class Typeable d => KnownDepth (d :: Depth) where
  depthVal :: proxy d -> Depth

instance KnownDepth 'TOne where
  depthVal _ = TOne

instance KnownDepth 'TMany where
  depthVal _ = TMany

--------------------------------------------------------------------------------
-- Basic syntax

-- Overlays of tournaments
---------------------------

-- | Overlay two tournaments. See 'Overlay'.
(+++) :: Tournament a -> Tournament a -> Tournament a
(+++) Empty a = a
(+++) a Empty = a
(+++) a b = Overlay a b

infixl 2 +++

-- | Overlay an arbitrary number of tournaments.
overlay :: Foldable f => f (Tournament a) -> Tournament a
overlay = foldr (+++) Empty

-- Sequences of tournaments
----------------------------

-- | Sequence two tournaments, running them one after the other. See 'Sequence'.
(***) :: forall a b. (KnownDepth a, KnownDepth b) => Tournament a -> Tournament b -> Tournament TMany
(***) = curry \case
  (Empty, Empty) -> Empty
  (Empty, cast -> Just b) -> b
  (cast -> Just a, Empty) -> a
  (LiftTMany a, LiftTMany b) -> a *** b
  (LiftTMany a, b) -> a *** b
  (a, LiftTMany b) -> a *** b
  (a, b) -> Sequence a b

infixl 1 ***

sequence :: (Foldable f, KnownDepth a) => f (Tournament a) -> Tournament TMany
sequence = foldr (***) Empty
