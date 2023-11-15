module Tourney.VM.Code where

import Data.Vector (Vector)
import Tourney.Algebra.Unified
import Tourney.Match
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB

data TourneyOp
  = MATCH !Match
  | -- | Begin a round
    BEGIN_ROUND
  | -- | End a round
    END_ROUND
  | -- | Cause the current sorting network to evaluate
    PERFORM_SORTING !Focus !SortMethod
  deriving stock (Show, Eq, Ord)

type Code = Vector TourneyOp

type CodeBuilder = VB.Builder TourneyOp

optimiseCodeBuilder :: CodeBuilder -> CodeBuilder
optimiseCodeBuilder = VB.vector @Vector . VB.build
