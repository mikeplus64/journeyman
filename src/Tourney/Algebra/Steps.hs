module Tourney.Algebra.Steps where

import Tourney.Algebra.Step
import Tourney.Types

data Steps s
  = One s
  | Cat (Steps s) (Steps s)
  | Empty
  deriving stock (Functor, Foldable, Traversable)

data Dyn a
  = DynPlayerCount (PlayerCount -> a)
  | DynStandings (Standings -> a)
  | OverFocus (PlayerCount -> [Focus]) a
