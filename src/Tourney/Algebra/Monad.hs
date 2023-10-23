module Tourney.Algebra.Monad where

import Control.Monad.ST.Strict
import Data.Vector (Vector)
import Tourney.Algebra.Step
import Tourney.Algebra.Types

data TResult = TResult
  { rounds :: Vector Step
  }

newtype Tournament a = Tournament
  { describeTournament
      :: forall s
       . PlayerCount
      -> Vector Step
      -> ST s TResult
  }
