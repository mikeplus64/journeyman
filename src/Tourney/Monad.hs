module Tourney.Monad where

import Control.Lens
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free
import Data.Generics.Labels ()
import Data.Set qualified as Set
import Data.Tuple.Ordered
import Data.Vector.Unboxed qualified as U
import Tourney.SortingNetwork
import Prelude hiding (Product)

type TourneyM s a = Free (TourneyStep s) a

pattern TourneyStepF :: MatchType -> StepM s (Free (TourneyStep s) a) -> TourneyM s a
pattern TourneyStepF m t = Free (TourneyStep m t)

pattern TourneyDoneF :: TourneyM s ()
pattern TourneyDoneF = Pure ()

{-# COMPLETE TourneyDoneF, TourneyStepF #-}

unTourneyM :: TourneyM s a -> Free (TourneyStep s) a
unTourneyM a = a

data TourneyStep s a where
  TourneyStep :: MatchType -> StepM s a -> TourneyStep s a

instance Functor (TourneyStep s) where
  fmap f (TourneyStep m s) = TourneyStep m (fmap f s)

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

data StepError = InvalidMatch Int Int
  deriving stock (Show, Read, Eq, Ord)

type Seeding = U.Vector Int

data Dep a where
  PlayerCount :: Dep Int
  RoundNumber :: Dep Int
  Seeding :: Dep Seeding

type DepType (x :: Dep a) = a

class x ~ DepType d => HasDep (d :: Dep x) s where
  getDep :: proxy d -> s -> x

step :: MatchType -> StepM s (TourneyM s a) -> TourneyM s a
step mode = Free . TourneyStep mode

dep :: forall d s. HasDep d s => StepM s (DepType d)
dep = asks (getDep (Proxy :: Proxy d))

match :: LowHigh Int -> StepM s ()
match lh = modify' (Set.insert lh)

done :: StepM s (TourneyM s ())
done = pure (pure ())

delay :: Int -> TourneyM s () -> TourneyM s ()
delay n t | n > 0 = step Swaps (pure (delay (n - 1) t))
delay _ t = t

interleave :: TourneyM s () -> TourneyM s () -> TourneyM s ()
interleave (TourneyStepF m0 top) (TourneyStepF m1 bot) =
  TourneyStepF m0 (top <&> \t -> TourneyStepF m1 (bot <&> interleave t))
interleave (Pure _) t = t
interleave _ t = t

leftover
  :: HasDep 'PlayerCount s
  => StepM s (Set (LowHigh Int))
leftover = do
  count <- dep @'PlayerCount
  matches <- get
  pure $ Set.fromList do
    l <- [0 .. count - 1]
    h <- [l + 1 .. count - 1]
    let lh = LowHigh_ l h
    guard (lh `Set.notMember` matches)
    pure lh

--------------------------------------------------------------------------------

halves :: [a] -> ([a], Maybe a, [a])
halves xs =
  ( take h xs
  , guard (q > 0) >> xs ^? ix h
  , drop (h + q) xs
  )
  where
    (h, q) = length xs `divMod` 2

easyGames :: Int -> (Maybe Int, [LowHigh Int])
easyGames n =
  (m, zipWith LowHigh_ l (reverse h))
  where
    (l, m, h) = halves [0 .. n - 1]

matchBestWorst :: HasDep 'PlayerCount s => StepM s Bool
matchBestWorst = do
  count <- dep @'PlayerCount
  if count <= 0
    then pure False
    else do
      modify' (Set.insert (LowHigh_ 0 (count - 1)))
      pure True

--------------------------------------------------------------------------------

data StaticInfo = StaticInfo {playerCount :: Int}
  deriving stock (Show, Read, Eq, Ord, Generic)

instance HasDep 'PlayerCount StaticInfo where
  getDep _ = view #playerCount

createStaticRounds
  :: s
  -> TourneyM s ()
  -> [Round]
createStaticRounds s t = case unTourneyM t of
  Free (TourneyStep type_ (StepM runStep)) ->
    Round {type_, matches}
      : createStaticRounds s t1
    where
      (matches, t1) = case runStep s mempty of
        (Left _err, ms) -> (ms, Pure ())
        (Right inner, ms) -> (ms, inner)
  Pure () -> []
