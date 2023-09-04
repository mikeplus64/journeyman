module Tourney.Monad where

import Control.Monad.Except (Except, ExceptT (..), MonadError (..))
import Control.Monad.Free
import Control.Monad.ST.Strict
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.Functor.Compose
import Data.Functor.Product
import Data.Set qualified as Set
import Data.Tuple.Ordered
import Data.Vector.Unboxed qualified as U
import Prelude hiding (Product)

newtype TourneyM a = TourneyM (Free TourneyStep a)
  deriving newtype (Functor, Applicative, Monad)

unTourneyM :: TourneyM a -> Free TourneyStep a
unTourneyM (TourneyM a) = a

data TourneyStep a where
  TourneyStep :: StepMode -> StepM s a -> TourneyStep a

instance Functor TourneyStep where
  fmap f (TourneyStep m s) = TourneyStep m (fmap _ s)

instance MonadFree TourneyStep TourneyM where
  wrap = TourneyM . Free . fmap (\(TourneyM a) -> a)

newtype StepM s a = StepM
  { runStep :: s -> Set (LowHigh Int) -> (Either StepError a, Set (LowHigh Int))
  }
  deriving stock (Functor)
  deriving
    ( Applicative
    , Monad
    , MonadState (Set (LowHigh Int))
    , MonadReader s
    , MonadError StepError
    )
    via (ReaderT s (ExceptT StepError (StateT (Set (LowHigh Int)) Identity)))

data StepMode = Score | Swaps

data StepError = InvalidMatch Int Int

type Seeding = U.Vector Int

step :: StepMode -> StepM s (TourneyM a) -> TourneyM a
step mode = TourneyM . Free . TourneyStep mode . fmap unTourneyM

match :: Int -> Int -> StepM s ()
match a b = case LowHigh a b of
  Just lh -> modify' (Set.insert lh)
  Nothing -> throwError (InvalidMatch a b)

playerCount :: StepM Seeding Int
playerCount = asks U.length

available :: StepM s (Set (LowHigh Int))
available = do
  count <- playerCount
  matches <- get
  pure $ Set.fromList do
    l <- [0 .. count - 1]
    h <- [l + 1 .. count - 1]
    let lh = LowHigh_ l h
    guard (lh `Set.notMember` matches)
    pure lh

done :: StepM s (TourneyM ())
done = pure (pure ())

test :: TourneyM ()
test = do
  step Swaps do
    avail <- available
    match 1 2
    done
