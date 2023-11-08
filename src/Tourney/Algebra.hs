module Tourney.Algebra (
  module Tourney.Algebra.Step,
  module Tourney.Algebra.Monad,
  module Tourney.Types,
  module Tourney.Match,
) where

import Control.Concurrent.Chan
import Control.Monad.ST.Strict
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Tourney.Algebra.Monad
import Tourney.Algebra.Step
import Tourney.Match
import Tourney.Types

data MatchID = MatchID
  { round, index :: !Int
  }
  deriving stock (Eq, Show, Ord)

-- data TournamentStep
--   = RoundDone !Int !Standings
--   | RequestMatch !MatchID !Match
--   | BeginStep Step

-- tournament
--   :: Monad m
--   => Steps ()
--   -> Standings
--   -> Stream (Of MatchResult) m ()
--   -> Stream (Of TournamentStep) m ()
-- tournament (Steps f) initialStandings results = do
--   let rounds = fst (runST (f (U.length initialStandings)))
--   S.yield (RoundDone 0 initialStandings)
--   mapM_ (performStep results) rounds

-- performStep
--   :: Monad m
--   => Stream (Of MatchResult) m ()
--   -> Step
--   -> Stream (Of TournamentStep) m ()
-- performStep results s = do
--   S.yield (BeginStep s)
