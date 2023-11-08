{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Defines vectors that are internally implemented as functions from an index
-- to a value, and a length. Numerous operations become extremely cheap with
-- this approach while still allowing for easy list-like expression of values.
--
-- This should usually be used as an intermediate data-structure as a builder of
-- "concrete" vectors.
module Data.Function.Vector (
  Vector,
  optimise,

  -- * Construction
  type U,
  type V,
  type S,
  fromVector,
  fromList,
  empty,
  singleton,
  append,
  enumFromTo,
  (..<),
  (>..),
  replicate,
  reverse,
  take,
  drop,

  -- * Updates
  map,
  set,
  modify,

  -- * Deconstruction
  toVector,
  toList,

  -- * Indexing
  (!),
  (!?),

  -- * Size
  null,
  length,
  depth,

  -- * Folds
  foldr,
  foldr',
  foldl,
  foldl',

  -- ** Indexed variants
  ifoldr,
  ifoldr',
  ifoldl,
  ifoldl',

  -- * Traversals
  mapM,
  mapM_,

  -- ** Indexed variants
  imapM,
  imapM_,
) where

import Control.Monad (liftM2)

-- import Data.IntervalMap.Generic.Interval
-- import Data.IntervalMap.Generic.Strict qualified as IM

import Control.Lens (Index, IxValue, Ixed (..), (^?!))
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IM
import Data.Monoid
import Data.Vector qualified as V
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Storable qualified as S
import Data.Vector.Unboxed qualified as U
import GHC.Base (
  Int#,
  tagToEnum#,
  (+#),
  (-#),
  (<#),
  (==#),
 )
import GHC.Int (Int (..))
import GHC.IsList qualified as GL
import Text.Show (showsPrec)
import Prelude hiding (
  drop,
  empty,
  enumFromTo,
  foldl',
  foldr,
  fromList,
  length,
  map,
  mapM,
  mapM_,
  modify,
  null,
  replicate,
  reverse,
  take,
  toList,
 )

-- | An index-based data structure. Conceptually it is just a @ Int -> a @. This
-- should have faster indexing, but slower everything-else.
--
-- It is possible to implement many instances for this type, such as 'Monad',
-- 'Functor', 'Eq', 'Ord', etc. -- but to a tee they would all use 'toVector' in
-- a way that would duplicate work if the user ever used the 'Vector' again. If you
-- need these instances, use 'toVector' first, and transform that back into an
-- 'Vector' with 'fromVector'.
data Vector a = Vector
  { index# :: Int# -> a
  , length# :: Int#
  , depth# :: Int#
  -- ^ maximum number of indirections
  }

instance Show a => Show (Vector a) where
  showsPrec n = showsPrec n . toVector @V.Vector

instance Semigroup (Vector a) where
  (<>) = append

instance Monoid (Vector a) where
  mempty = Data.Function.Vector.empty

instance Functor Vector where
  fmap = Data.Function.Vector.map @V

instance Applicative Vector where
  pure = singleton
  liftA2 = liftM2

instance Monad Vector where
  x >>= f = joinIntoVector (mapToVector @V (toVector @V . f) x)

instance GL.IsList (Vector a) where
  type Item (Vector a) = a
  fromList = fromVector . V.fromList
  fromListN n = fromVector . V.fromListN n
  toList = V.toList . toVector

instance Foldable Vector where
  foldr = Data.Function.Vector.foldr
  foldl = Data.Function.Vector.foldl
  foldr' = Data.Function.Vector.foldr'
  foldl' = Data.Function.Vector.foldl'
  length = Data.Function.Vector.length
  null = Data.Function.Vector.null
  toList = Data.Function.Vector.toList

type instance IxValue (Vector a) = a
type instance Index (Vector a) = Int
instance Ixed (Vector a) where
  ix i f v = set v i <$> f (v ! i)

{-# INLINE optimise #-}

-- | /O(N * D)/
optimise :: forall v a. G.Vector v a => Vector a -> Vector a
optimise = fromVector @v . toVector

{-# INLINE joinIntoVector #-}

-- | Join the source vectors into a consolidated one. The result will be backed
-- by an 'IntMap' into the correct starting indices for each of the original
-- vectors, and those original vectors themselves.
joinIntoVector :: (Ixed (v a), IxValue (v a) ~ a, Index (v a) ~ Int, Foldable v, Foldable t) => t (v a) -> Vector a
joinIntoVector outer =
  Vector
    { length# = totalLength#
    , depth# = 2#
    , index# = \i ->
        let !(I# offset, source) = case IM.lookupLE (I# i) indices of
              Just (start, vec) -> (start, vec)
              _ -> error "joinIntoVector: interval not found"
        in  source ^?! ix (I# (i -# offset))
    }
  where
    state0 = IxState{total = 0, indices = IM.empty}
    !(IxState{total = I# totalLength#, indices}) = executingState state0 $ forM_ outer \inner -> do
      IxState{total = start, indices = acc} <- get
      let !end = start + Foldable.length inner
      let !next = IM.insert start inner acc
      put IxState{total = end, indices = next}

data IxInterval = IxInterval !Int !Int
  deriving stock (Eq, Ord, Show)

data IxState a = IxState {total :: !Int, indices :: !(IM.IntMap a)}

--------------------------------------------------------------------------------
-- Construction

type U = U.Vector
type S = S.Vector
type V = V.Vector

-- | /O(1)/
fromVector :: G.Vector v a => v a -> Vector a
fromVector !vec =
  Vector
    { length# = case G.length vec of I# a -> a
    , depth# = 1#
    , index# = \i -> G.unsafeIndex vec (I# i)
    }

fromList :: [a] -> Vector a
fromList = fromVector . V.fromList

empty :: Vector a
empty =
  Vector
    { index# = outOfBoundsError# "empty" 0#
    , length# = 0#
    , depth# = 0#
    }

singleton :: a -> Vector a
singleton a =
  Vector
    { index# = \i -> case i of
        0# -> a
        _ -> outOfBoundsError# "singleton" 1# i
    , length# = 1#
    , depth# = 1#
    }

-- | /O(1)/
append :: Vector a -> Vector a -> Vector a
append a b =
  Vector
    { length# = length# a +# length# b
    , depth# = 1# +# max# (depth# a) (depth# b)
    , index# = \i -> case i <# length# a of
        1# -> index# a i
        _ -> index# b (i -# length# a)
    }

-- | /O(1)/ Create an [open,open] interval between the two provided values.
enumFromTo :: Int -> Int -> Vector Int
enumFromTo (I# a) (I# b) =
  Vector
    { length# = (b -# a) +# 1#
    , depth# = 1#
    , index# = \i -> I# (a +# i)
    }

-- | /O(1)/ Create a [open, closed) interval between the two provided values.
--
-- @
-- interval a b = if a > b then [a, a - 1 .. b + 1] else [a .. b - 1]
-- a ..< b = fromList (interval a b)
-- @
(..<) :: Int -> Int -> Vector Int
(..<) (I# a) (I# b) =
  Vector
    { length# = b -# a
    , depth# = 1#
    , index# = \i -> I# (a +# i)
    }

-- | /O(1)/ Create a [open, closed) interval between the two provided values.
--
-- @
-- interval a b = if a > b then [a, a - 1 .. b + 1] else [a .. b - 1]
-- a >.. b = fromList (interval a b)
-- @
(>..) :: Int -> Int -> Vector Int
(>..) (I# a) (I# b) =
  Vector
    { length# = b -# a
    , depth# = 1#
    , index# = \i -> I# (a -# i)
    }

infix 7 >..
infix 7 ..<

-- | /O(1)/
replicate :: Int -> a -> Vector a
replicate (I# l) a =
  Vector
    { length# = l
    , depth# = 1#
    , index# = \_ -> a
    }

take :: Int -> Vector a -> Vector a
take (I# l) Vector{length#, depth#, index#} = Vector{depth#, length# = min# l length#, index#}

drop :: Int -> Vector a -> Vector a
drop (I# l) Vector{length#, depth#, index#} =
  Vector
    { depth# = depth# +# 1#
    , length# = max# 0# (length# -# l)
    , index# = \i -> index# (i +# l)
    }

reverse :: Vector a -> Vector a
reverse Vector{length#, depth#, index#} =
  Vector
    { depth# = depth# +# 1#
    , length#
    , index# = \i -> index# (length# -# i)
    }

--------------------------------------------------------------------------------
-- Updates

{-# INLINE set #-}

-- | /O(N)/ Beware
set :: Vector a -> Int -> a -> Vector a
set Vector{index#, length#, depth#} (I# i) a =
  Vector
    { depth# = depth# +# 1#
    , length# = length#
    , index# = \j -> case j ==# i of
        1# -> a
        _ -> index# j
    }

{-# INLINE modify #-}

-- | /O(N)/ Beware
modify :: Vector a -> Int -> (a -> a) -> Vector a
modify Vector{index#, depth#, length#} (I# i) f =
  Vector
    { depth# = depth# +# 1#
    , length# = length#
    , index# = \j -> case j ==# i of
        1# -> f (index# j)
        _ -> index# j
    }

--------------------------------------------------------------------------------
-- Deconstruction

-- | /O(N * D)/
toVector :: forall v a. G.Vector v a => Vector a -> v a
toVector = mapToVector id

toList :: Vector a -> [a]
toList = foldr (:) []

--------------------------------------------------------------------------------
-- Size

length :: Vector a -> Int
length Vector{length#} = I# length#

depth :: Vector a -> Int
depth Vector{depth#} = I# depth#

null :: Vector a -> Bool
null Vector{length#} = tagToEnum# (length# ==# 0#)

--------------------------------------------------------------------------------
-- Internal utility

{-# INLINE slice# #-}
slice#
  :: Vector a
  -> (Int# -> a -> b -> b)
  -> b
  -> Int#
  -- ^ starting index (inclusive)
  -> Int#
  -- ^ end index (non-inclusive)
  -> b
slice# Vector{index#} f z0 start end = go start
  where
    go i = case i <# end of
      1# -> f i (index# i) (go (i +# 1#))
      _ -> z0

{-# INLINE mapToVector #-}
mapToVector :: forall v b a. G.Vector v b => (a -> b) -> Vector a -> v b
mapToVector f o@Vector{length#} =
  G.create
    ( do
        m <- GM.new (I# length#)
        slice#
          o
          (\i# a acc -> acc >> GM.unsafeWrite m (I# i#) (f a))
          (return ())
          0#
          length#
        return m
    )

--------------------------------------------------------------------------------
-- Indexing

{-# INLINE (!) #-}

-- | /O(D)/
(!) :: Vector a -> Int -> a
(!) Vector{index#, length#} (I# i#)
  | tagToEnum# (i# <# length#) = index# i#
  | otherwise = outOfBoundsError# "(!)" i# length#

{-# INLINE (!?) #-}

-- | /O(D)/
(!?) :: Vector a -> Int -> Maybe a
(!?) Vector{length#, index#} (I# i#)
  | tagToEnum# (i# <# length#) = Just (index# i#)
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Maps

{-# INLINE map #-}

-- | /O(1)/
map :: forall v a b. G.Vector v b => (a -> b) -> Vector a -> Vector b
map f Vector{length#, depth#, index#} =
  Vector
    { length#
    , depth# = 1# +# depth#
    , index# = \i -> f (index# i)
    }

--------------------------------------------------------------------------------
-- Folds

{-# INLINE foldr #-}
{-# INLINE ifoldr #-}
{-# INLINE foldl #-}
{-# INLINE ifoldl #-}
{-# INLINE foldl' #-}
{-# INLINE ifoldl' #-}

-- | Right-associative fold
foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr f z o@Vector{length#} = slice# o (\_ x xs -> f x xs) z 0# length#

-- | Right-associative strict fold
foldr' :: (a -> b -> b) -> b -> Vector a -> b
foldr' f z o@Vector{length#} = slice# o (\_ !x !xs -> f x xs) z 0# length#

-- | Right-associative strict fold, with index
ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr f z o@Vector{length#} = slice# o (\i !x xs -> f (I# i) x xs) z 0# length#

-- | Right-associative strict fold, with index
ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr' f z o@Vector{length#} = slice# o (\i !x !xs -> f (I# i) x xs) z 0# length#

-- | Left-associative fold
foldl :: (b -> a -> b) -> b -> Vector a -> b
foldl f z0 o@Vector{length#} = slice# o (\_ !x k z -> k (f z x)) id 0# length# z0

-- | Strict left-associative fold
foldl' :: (b -> a -> b) -> b -> Vector a -> b
foldl' f z0 o@Vector{length#} = slice# o (\_ !x k z -> k $! f z x) id 0# length# z0

-- | Left-associative fold, with index
ifoldl :: (Int -> b -> a -> b) -> b -> Vector a -> b
ifoldl f z0 o@Vector{length#} = slice# o (\i !x k z -> k (f (I# i) z x)) id 0# length# z0

-- | Strict left-associative fold, with index
ifoldl' :: (Int -> b -> a -> b) -> b -> Vector a -> b
ifoldl' f z0 o@Vector{length#} = slice# o (\i !x k z -> k $! f (I# i) z x) id 0# length# z0

--------------------------------------------------------------------------------
-- Traversals

{-# INLINE mapM #-}
{-# INLINE imapM #-}
{-# INLINE mapM_ #-}
{-# INLINE imapM_ #-}

-- | /O(N * D)/ optimising
mapM :: Monad m => (a -> m b) -> Vector a -> m (Vector b)
mapM f = fmap fromVector . V.mapM f . toVector

-- | /O(N * D)/ optimising
imapM :: Monad m => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM f = fmap fromVector . V.imapM f . toVector

-- | /O(N * D)/
mapM_ :: Monad m => (a -> m ()) -> Vector a -> m ()
mapM_ f Vector{index#, length#} = go 0#
  where
    go i = case i <# length# of
      1# -> f (index# i) >> go (i +# 1#)
      _ -> return ()

-- | /O(N * D)/
imapM_ :: Monad m => (Int -> a -> m ()) -> Vector a -> m ()
imapM_ f Vector{index#, length#} = go 0#
  where
    go i = case i <# length# of
      1# -> f (I# i) (index# i) >> go (i +# 1#)
      _ -> return ()

--------------------------------------------------------------------------------
-- Internals

outOfBoundsError :: Text -> (Int, Int) -> a
outOfBoundsError a s = error ("Data.Vector." <> a <> ": out of bounds " <> show s)

{-# NOINLINE outOfBoundsError# #-}
outOfBoundsError# :: Text -> Int# -> Int# -> a
outOfBoundsError# a len# i# = outOfBoundsError a (I# i#, I# len#)

{-# INLINE max# #-}
max# :: Int# -> Int# -> Int#
max# a b = if tagToEnum# (a <# b) then b else a

{-# INLINE min# #-}
min# :: Int# -> Int# -> Int#
min# a b = if tagToEnum# (a <# b) then a else b
