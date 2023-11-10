{-# OPTIONS_GHC -Wno-deprecations #-}

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
  Stream (..),
  StreamM (..),
  peek,
  Pop (..),
  forcePop,
  pop,
  map,
  peekFoldMap,
  peekAll,
  peekAllS,
  yield,
  await,
  lift,
  for_,
  fold,
  hoist,
  alignWith,
  toVector,

  -- * Request monads
  MonadRequest (..),
  MonadReaderRequest,
  once,
  cached,
) where

import Control.Lens
import Control.Monad (ap, liftM2)
import Data.Foldable qualified as Foldable
import Data.These
import Data.Vector (Vector)
import Data.Vector qualified as V
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB
import Prelude hiding (fold, foldMap, foldMapM, for_, lift, map)

--------------------------------------------------------------------------------

-- | A stream type that can be inspected without always needing to first go
-- through the @ m @ effect. This is in contrast to the streams provided by the
-- "streaming" package that _always_ require going through the effect monad to
-- unwrap a stream. It also supports cheap concatentation of streams.
--
-- The property of being able to inspect the stream without going through its
-- effect monad is that this stream type is very useful for modelling
-- "dependant" streams, where some amount of data can be generated in staggered
-- stages, contingent on explicitly-made requests for more input.
data StreamM m a x
  = -- | A yielded value of the stream
    Got a !(StreamM m a x)
  | -- | Wait for a value
    Wait !(m (StreamM m a x))
  | -- | Concatenate streams
    Cat (StreamM m a x) (StreamM m a x)
  | -- | Monadic return
    Done x
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Show a, Show x, forall i. Show i => Show (m i)) => Show (StreamM m a x)
deriving stock instance (Eq a, Eq x, forall i. Eq i => Eq (m i)) => Eq (StreamM m a x)

instance Functor m => Applicative (StreamM m a) where
  pure = Done
  liftA2 = liftM2
  (<*>) = ap
  a *> b = go a
    where
      go (Got x xs) = Got x (go xs)
      go (Wait m) = Wait (fmap go m)
      go (Cat x y) = Cat (go x) (go y)
      go (Done _) = b

instance Functor m => Monad (StreamM m a) where
  (>>) = (*>)
  a >>= f = go a
    where
      go (Got x xs) = Got x (go xs)
      go (Cat x y) = Cat (go x) (go y)
      go (Wait m) = Wait (fmap go m)
      go (Done x) = f x

instance Semigroup (StreamM m a x) where
  (<>) = Cat

instance Monoid x => Monoid (StreamM m a x) where
  mempty = Done mempty

-- | The uncons operation _purely_ pops off the first element of the stream,
-- rebalancing the stream so that subsequent 'uncons' operations are cheap. This
-- cannot cause any inner effect to be ran.
instance Monoid x => Cons (StreamM m a x) (StreamM m a x) a a where
  _Cons = prism (uncurry Got) (\s -> maybe (Left s) Right (uncons_ s))
    where
      {-# INLINE uncons_ #-}
      uncons_ :: StreamM m a x -> Maybe (a, StreamM m a x)
      uncons_ = \case
        Got x xs -> Just (x, xs)
        Cat l r -> unconsTo l r
        _ -> Nothing

      -- See Acc.NeAcc.uncons for a similar implementation.
      unconsTo (Got x l') r = Just (x, Cat l' r)
      unconsTo (Cat l' r') r = unconsTo l' (Cat r' r)
      unconsTo _ _ = Nothing

instance x ~ () => One (StreamM m a x) where
  type OneItem (StreamM m a x) = a
  one a = Got a (Done ())

-- | Pop the first available element of the stream, rebalancing the stream if
-- possible so that subsequent 'pop' calls are cheap. When 'PopDone' is reached,
-- the stream has ended
pop :: Monad m => StreamM m a x -> Pop m a x
pop = \case
  Got x xs -> Pop x xs
  Cat l r -> go l r
  Wait m -> PopWait (pop <$> m)
  _ -> PopDone
  where
    go (Got x l') r = Pop x (Cat l' r)
    go (Cat l' r') r = go l' (Cat r' r)
    go (Wait m) r = PopWait ((`go` r) <$> m)
    go _ r = pop r

data Pop m a x
  = Pop a (StreamM m a x)
  | PopWait (m (Pop m a x))
  | PopDone

-- | Force a 'Pop', using the monad even if it is not strictly necessary.
forcePop :: Monad m => Pop m a x -> m (Maybe (a, StreamM m a x))
forcePop (Pop x xs) = pure (Just (x, xs))
forcePop (PopWait w) = forcePop =<< w
forcePop PopDone = pure Nothing

-- | Iterate over the stream, running effects as necessary.
for_ :: Monad m => StreamM m a x -> (a -> m ()) -> m ()
for_ s0 f = go s0
  where
    go s = do
      next <- forcePop (pop s)
      case next of
        Just (a, s') -> f a >> go s'
        Nothing -> pure ()

{-# INLINE fold #-}

-- | Fold over the elements of the stream, running effects as needed
fold :: Monad m => (a -> r -> r) -> r -> StreamM m a x -> m r
fold f z = go
  where
    go s = do
      next <- forcePop (pop s)
      case next of
        Just (a, s') -> f a <$> go s'
        Nothing -> pure z

toVector :: Monad m => StreamM m a x -> m (Vector a)
toVector s =
  VB.build <$> fold (\x xs -> VB.singleton x <> xs) mempty s

-- | Purely pop off the first available element of the stream, rebalancing the
-- stream so that subsequent 'uncons' operations are cheap. If the ordering of
-- elements and effects is important for this stream, prefer 'uncons' instead --
-- this function is happy to pop off a yielded stream element from the middle of
-- stream.
peek :: StreamM m a x -> Maybe (a, StreamM m a x)
peek = \case
  Got x xs -> Just (x, xs)
  Cat l r -> go l r
  _ -> Nothing
  where
    go (Got x l') r = Just (x, Cat l' r)
    go (Cat l' r') r = go l' (Cat r' r)
    go l (Got x r) = Just (x, Cat l r)
    go l (Cat l' r') = fmap (Cat l) <$> go l' r'
    go _ _ = Nothing

peekAllS :: MonadState (StreamM f a x) m => m (Bool, [a])
peekAllS = do
  s <- get
  let !(xs, done, s') = peekAll s
  (done, xs) <$ put s'

-- | Purely pop off all available elements of the stream. The ordering of
-- elements may not be the order that is expected if there are effects that
-- appear in the tree "before" available elements.
peekAll :: StreamM f a x -> ([a], Bool, StreamM f a x)
peekAll s =
  case peek s of
    Just (x, s') ->
      let !(xs, _isComplete, s'') = peekAll s'
      in  (x : xs, isDone s'', s'')
    Nothing -> ([], isDone s, s)
  where
    isDone (Done _) = True
    isDone (Got _ _) = False
    isDone (Wait _) = False
    isDone (Cat l r) = isDone l && isDone r

-- | Fold over only the immediately available elements
peekFoldMap :: Monoid b => (a -> b) -> StreamM m a x -> b
peekFoldMap f = go
  where
    go (Got x xs) = f x <> go xs
    go (Cat x y) = go x <> go y
    go _ = mempty

lift :: Functor m => m x -> StreamM m a x
lift mx = Wait (pure <$> mx)

map :: Functor m => (a -> b) -> StreamM m a x -> StreamM m b x
map f = \case
  Got x xs -> Got (f x) (map f xs)
  Cat x y -> Cat (map f x) (map f y)
  Wait m -> Wait (fmap (map f) m)
  Done x -> Done x

yield :: a -> StreamM m a ()
yield a = Got a (Done ())

await :: forall r a m. MonadRequest r m => StreamM m a r
await = Wait (pure <$> request @r)

-- | Natural transformation over the inner monad of a stream.
hoist :: Functor f => (forall x. f x -> g x) -> StreamM f a r -> StreamM g a r
hoist f = \case
  Got x xs -> Got x (hoist f xs)
  Cat l r -> Cat (hoist f l) (hoist f r)
  Wait m -> Wait (f (hoist f <$> m))
  Done x -> Done x

-- | Alignment of two streams. Avoids running effects until necessary.
alignWith :: forall m a b c. Monad m => (These a b -> c) -> StreamM m a () -> StreamM m b () -> StreamM m c ()
alignWith f = go
  where
    go :: StreamM m a () -> StreamM m b () -> StreamM m c ()
    go l r = case (pop l, pop r) of
      (Pop x xs, Pop y ys) -> do
        yield (f (These x y))
        go xs ys
      (pl, PopDone) -> thises pl
      (PopDone, pr) -> thats pr
      (pl, pr) -> do
        -- Fall back to running effects, since at least one of the left or right
        -- streams will anyway
        el <- lift (forcePop pl)
        er <- lift (forcePop pr)
        case (el, er) of
          (Just (x, xs), Just (y, ys)) -> do
            yield (f (These x y))
            go xs ys
          (Just (x, xs), Nothing) -> do
            yield (f (This x))
            thises (pop xs)
          (Nothing, Just (y, ys)) -> do
            yield (f (That y))
            thats (pop ys)
          (Nothing, Nothing) -> pure ()

    -- Only "left" elements remain
    thises :: Pop m a () -> StreamM m c ()
    thises = \case
      Pop x xs -> do
        yield (f (This x))
        thises (pop xs)
      PopWait w -> thises =<< lift w
      PopDone -> pure ()

    -- Only "right" elements remain
    thats :: Pop m b () -> StreamM m c ()
    thats = \case
      Pop y ys -> do
        yield (f (That y))
        thats (pop ys)
      PopWait w -> thats =<< lift w
      PopDone -> pure ()

-- | A version of 'StreamM' with 'Functor' and 'Foldable' instances over the
-- elements of the stream rather than the inner results of the monad
newtype Stream m a = Stream {unStream :: StreamM m a ()}

deriving newtype instance (Show a, forall i. Show i => Show (m i)) => Show (Stream m a)
deriving newtype instance (Eq a, forall i. Eq i => Eq (m i)) => Eq (Stream m a)

instance Cons (Stream m a) (Stream m a) a a where
  {-# INLINE _Cons #-}
  _Cons = coerced . am . coerced
    where
      am :: Prism (StreamM m a ()) (StreamM m a ()) (a, StreamM m a ()) (a, StreamM m a ())
      am = _Cons

instance Functor m => Functor (Stream m) where
  fmap f = coerce (map f)

-- | Fold over the immediately-available elements only
instance Foldable (Stream m) where
  foldMap f = coerce (peekFoldMap f)

-- See
-- https://hackage.haskell.org/package/streaming-0.2.4.0/docs/src/Streaming.Internal.html#line-331
-- for the 'Stream' equivalent of these instances; they are (necessarily) nearly
-- identical
--
-- Unlike the streams provided by the "streaming" package, the streams here are
-- allowed to uncons elements without first going through the base monad of the
-- stream. For instance, the Foldable instance will fold over all immediately
-- availble elements of the stream.

--------------------------------------------------------------------------------

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

-- | Perform an action only once. This may be useful for ensuring a dependency
-- only is fetched once.
once :: (MonadIO outer, MonadIO inner, NFData a) => inner a -> outer (inner a)
once computeValue = do
  lockedVar <- newMVar =<< newEmptyMVar
  let withVar f = do
        inner <- takeMVar lockedVar
        r <- f inner
        putMVar lockedVar inner
        pure r
  pure $ withVar \self -> do
    now <- tryReadMVar self
    case now of
      Just v -> pure v
      Nothing -> do
        !v <- evaluateNF =<< computeValue
        putMVar self v
        pure v

-- | Pure 'MonadState' version of 'once', that takes a lens to the cached value
-- from the state, and a function to compute a value.
cached :: (MonadState s m, NFData a) => Lens' s (Maybe a) -> m a -> m a
cached cacheLens computeValue =
  use cacheLens >>= \case
    Just v -> pure v
    Nothing -> do
      v <- computeValue
      case rnf v of
        () -> do
          cacheLens .= Just v
          pure v
