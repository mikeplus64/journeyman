-- | Dependant streams of values
--
-- That is, a structure that can be thought of as producing many values, but
-- optionally (but explicitly!) depending on some other.
--
-- This lets use the same syntax for creating structures that do or do not
-- depend on some outside data, while still being able to inspect those
-- structures that don't ultimately need the dependency.
--
-- It is trivially isomorphic to stream types used by the "streaming" library
-- under Reader-like monads. Those reader-like monads are generalised here to
-- support the general case of readers that immediately hold the data you want,
-- or readers that require an action to be ran to get the data. See
-- 'MonadRequest'
module Data.Dependency (
  -- * Base type
  DependantStream (..),
  fromStream,
  toStream,

  -- * Awaitable streams
  Awaiting (..),
  AwaitingM (..),
  awaiting,

  -- * Request monads
  MonadRequest (..),
  MonadReaderRequest,
) where

import Control.Monad (ap)
import Data.DList qualified as DL
import Streaming
import Streaming.Internal (Stream (..))
import Streaming.Prelude qualified as S

-- | A monad for reader-like monads where you can request a particular bit of
-- data. For readers where the data is immediately available, it is the same as
-- 'Control.Monad.Reader.ask'. For readers where the data is embedded within
-- something else, like needing to perform ''IO', it acts more like the 'await'
-- keyword of many popular programming languages.
class Monad m => MonadRequest d m where
  request :: m d

-- | See 'MonadReaderRequest'
instance MonadReaderRequest d' d m => MonadRequest d (ReaderT d' m) where request = requestReader

-- Plain functions are trivial 'MonadRequest's
instance d ~ d' => MonadRequest d ((->) d') where request = id

-- Provide two Reader instances:
--
-- 1. ReaderT d m a ~ plain readers
--
-- 2. ReaderT (m d) m a ~ "action" readers, requiring something to be ran before
-- getting the 'd' value.
--
-- Overlapping instances pragma should ensure that there is not conflict
-- between the two.
class Monad m => MonadReaderRequest d' d m where requestReader :: ReaderT d' m d
instance {-# OVERLAPPING #-} (Monad m, d ~ d') => MonadReaderRequest (m d') d m where requestReader = ReaderT id
instance {-# OVERLAPPABLE #-} (Monad m, d ~ d') => MonadReaderRequest d' d m where requestReader = ReaderT pure

--------------------------------------------------------------------------------

data DependantStream d a
  = OkS a
  | NeedS (d -> DependantStream d a)
  | SplitS (DL.DList (DependantStream d a))
  deriving stock (Functor)

instance Applicative (DependantStream d) where
  pure = OkS
  liftA2 = liftM2

instance Monad (DependantStream d) where
  OkS a >>= f = SplitS (fmap f (DL.singleton a))
  NeedS fa >>= f = NeedS (f <=< fa)
  SplitS xs >>= f = SplitS (fmap (>>= f) xs)

instance Monoid (DependantStream a b) where
  mempty = SplitS mempty

instance Semigroup (DependantStream a b) where
  a@OkS {} <> b@OkS {} = SplitS (a `DL.cons` b `DL.cons` DL.empty)
  a@OkS {} <> SplitS b = SplitS (a `DL.cons` b)
  NeedS a <> NeedS b = NeedS (a <> b)
  a@NeedS {} <> SplitS b = SplitS (a `DL.cons` b)
  SplitS a <> SplitS b = SplitS (a <> b)
  SplitS a <> b@OkS {} = SplitS (a `DL.snoc` b)
  SplitS a <> b@NeedS {} = SplitS (a `DL.snoc` b)
  a <> b = SplitS (DL.cons a (DL.cons b DL.empty))

--------------------------------------------------------------------------------

-- | Create an easily-inspected stream of values from a 'DependantStream'.
--
-- Suggested usage:
-- @
-- let
--   withStream (Got x xs) = do { _ x; withStream xs }
--   withStream (Wait m) = withStream m
--   withStream Done = pure ()
--
-- withStream (awaiting depStream)
-- @
awaiting :: forall d m a. MonadRequest d m => DependantStream d a -> AwaitingM m a ()
awaiting = asAwaitingM . toStream
  where
    -- We could implement this directly without the conversion to a 'Stream'
    -- first, but this would be a lot more complicated.
    asAwaitingM :: Stream (Of a) m () -> AwaitingM m a ()
    asAwaitingM s = case s of
      Step (x :> xs) -> Got x (asAwaitingM xs)
      Effect m -> Wait (fmap asAwaitingM m)
      Return _ -> Done ()

-- | A simplification of @ 'Stream' ('Of' a) m @ that can be inspected without
-- always needing to first go through the @ m @ effect.
--
-- Another point of comparison to 'Stream' is that it supports relatively cheap
-- concatentation, at a potential later cost to iteration over the stream.
data AwaitingM m a x
  = Got a !(AwaitingM m a x)
  | Wait !(m (AwaitingM m a x))
  | Cat !(AwaitingM m a x) !(AwaitingM m a x)
  | Done x
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Show a, Show x, forall i. Show i => Show (m i)) => Show (AwaitingM m a x)
deriving stock instance (Eq a, Eq x, forall i. Eq i => Eq (m i)) => Eq (AwaitingM m a x)

instance Functor m => Applicative (AwaitingM m a) where
  pure = Done
  liftA2 = liftM2
  (<*>) = ap
  a *> b = go a
    where
      go (Got x xs) = Got x (go xs)
      go (Wait m) = Wait (fmap go m)
      go (Cat x y) = Cat (go x) (go y)
      go (Done _) = b

instance Functor m => Monad (AwaitingM m a) where
  (>>) = (*>)
  a >>= f = go a
    where
      go (Got x xs) = Got x (go xs)
      go (Cat x y) = Cat (go x) (go y)
      go (Wait m) = Wait (fmap go m)
      go (Done x) = f x

instance Semigroup (AwaitingM m a x) where
  (<>) = Cat

instance Monoid x => Monoid (AwaitingM m a x) where
  mempty = Done mempty

-- | A version of 'AwaitingM' with 'Functor' and 'Foldable' instances over the
-- elements of the stream rather than the inner results of the monad
newtype Awaiting m a = Awaiting {unAwaiting :: AwaitingM m a ()}

deriving newtype instance (Show a, forall i. Show i => Show (m i)) => Show (Awaiting m a)
deriving newtype instance (Eq a, forall i. Eq i => Eq (m i)) => Eq (Awaiting m a)

instance Functor m => Functor (Awaiting m) where
  fmap f (Awaiting aw) = Awaiting case aw of
    Got x xs -> Got (f x) (unAwaiting (fmap f (Awaiting xs)))
    Cat x y -> Cat (unAwaiting (fmap f (Awaiting x))) (unAwaiting (fmap f (Awaiting y)))
    Wait m -> Wait (fmap (unAwaiting . fmap f . Awaiting) m)
    Done x -> Done x

-- | Fold over the immediately-available elements only
instance Foldable (Awaiting m) where
  foldMap f (Awaiting aw) = go aw
    where
      go (Got x xs) = f x <> go xs
      go (Cat x y) = go x <> go y
      go _ = mempty

-- See
-- https://hackage.haskell.org/package/streaming-0.2.4.0/docs/src/Streaming.Internal.html#line-331
-- for the 'Stream' equivalent of these instances; they are (necessarily) nearly
-- identical

--------------------------------------------------------------------------------
-- Isomorphism between dependant streams and actual streams from the "streaming"
-- library

-- | Convert a 'DependantStream' to a 'Stream'.
toStream :: MonadRequest d m => DependantStream d a -> Stream (Of a) m ()
toStream (OkS a) = S.yield a
toStream (SplitS xs) = foldMap toStream xs
toStream (NeedS f) = effect (toStream . f <$> request)

-- | Convert a 'Stream' into a 'DependantStream'. The inner monad of the stream
-- must be pure functions.
fromStream :: Stream (Of a) ((->) d) () -> DependantStream d a
fromStream (Step (x :> xs)) = OkS x <> fromStream xs
fromStream (Effect m) = NeedS (fromStream . m)
fromStream (Return _) = SplitS mempty
