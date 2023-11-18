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
  Modify :: !Mod -> Tournament t -> Tournament (TMod t)
  -- | Overlay two tournaments, to describe running two sub-tournaments in
  -- parallel. The depth of the tournaments must be the same
  Overlay :: !(Tournament a) -> !(Tournament a) -> Tournament a
  -- | Sequence two tournaments one after the other. The resulting tournament
  -- has a depth that is the sum of the two depths of the input tournaments.
  Sequence :: (KnownDepth a, KnownDepth b) => !(Tournament a) -> !(Tournament b) -> Tournament TMany
  LiftTOne :: Tournament TOne -> Tournament TMany
  LiftTMod :: Tournament (TMod t) -> Tournament TMany
  Sort :: !SortMethod -> Tournament t -> Tournament t
  -- | Depend on the player count to produce an inner tournament
  ByPlayerCount :: !(PlayerCount -> Tournament t) -> Tournament t
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
    LiftTOne t -> Show.showParen True (Show.showString "LiftTOne " . Show.showsPrec 9 t)
    LiftTMod t -> Show.showParen True (Show.showString "LiftTMod " . Show.showsPrec 9 t)
    ByPlayerCount f ->
      Show.showParen
        True
        (Show.showString "players=" . Show.showsPrec 0 (8 :: Word8) . Show.showString " => " . Show.showsPrec 9 (f 8))
    ByStandings f -> Show.showString "ByStandings"
    Modify m t -> Show.showParen True (Show.showString "Mod " . Show.showsPrec 9 t)
    Sort s t -> Show.showParen True (Show.showString "Sort " . Show.showsPrec 9 s . Show.showString " " . Show.showsPrec 9 t)

data Mod
  = SetFocus !(Focus -> [Focus])
  | SetOffset !Int

-- | The depth of a tournament. Since we only care about distinguishing single
-- rounds from sequences of rounds, that is the only information stored here.
data Depth = TOne | TMany | TMod Depth

class Typeable d => KnownDepth (d :: Depth) where
  depthVal :: proxy d -> Depth

instance KnownDepth d => KnownDepth ('TMod d) where
  depthVal _ = depthVal (Proxy :: Proxy d)

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
  (LiftTOne a, LiftTOne b) -> a *** b
  (LiftTOne a, b) -> a *** b
  (a, LiftTOne b) -> a *** b
  (a, b) -> Sequence a b

infixl 1 ***

sequence :: (Foldable f, KnownDepth a) => f (Tournament a) -> Tournament TMany
sequence = foldr (***) Empty

-- Merging "modified" tournaments
-----------------------------------

mergeMod :: Tournament ('TMod a) -> Tournament ('TMod a) -> Tournament ('TMod a)
mergeMod = (+++)
