{-# LANGUAGE NoImplicitPrelude #-}

module Prelude (
  -- * Relude modules
  module Relude.Applicative,
  module Relude.Base,
  module Relude.Bool,
  module Relude.Container,
  module Relude.Debug,
  module Relude.DeepSeq,
  module Relude.Enum,
  module Relude.Exception,
  module Relude.File,
  module Relude.Foldable,
  module Relude.Function,
  module Relude.Functor,
  module Relude.Lifted,
  module Relude.List,
  module Relude.Monad,
  module Relude.Monoid,
  module Relude.Nub,
  module Relude.Numeric,
  module Relude.Print,
  module Relude.String,

  -- * Extra Modules
  module Control.Monad.ST.Strict,

  -- ** Semigroups
  Max (..),
  Min (..),

  -- ** Default
  module Data.Default,

  -- ** Lens
  module Control.Lens,
  position,

  -- ** String formatting
  module PyF,

  -- * Convenience types
  PrimMonad (..),
  MonadPrim,

  -- ** Vector
  Vector,
  MVector,
  UVector,
  UMVector,

  -- ** Array

  -- *** Immutable interface
  Array,
  UArray,
  arrayBounds,
  array,
  listArray,
  accum,
  accumArray,

  -- *** Mutable interface
  MArray,
  newArray,
  readArray,
  writeArray,
  getBounds,
  freezeArray,
  thawArray,
) where

import Control.Lens hiding (universe, (??))
import Control.Monad.Primitive (MonadPrim, PrimMonad (..))
import Control.Monad.ST.Strict
import Data.Array (Array, accum, accumArray, array, listArray)
import Data.Array.Base (IArray, bounds)
import Data.Array.MArray (MArray, freeze, getBounds, newArray, readArray, thaw, writeArray)
import Data.Array.Unboxed (UArray)
import Data.Default
import Data.Generics.Labels ()
import Data.Generics.Product.Positions (position)
import Data.Ix (Ix)
import Data.Semigroup (Max (..), Min (..))
import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)
import Data.Vector.Unboxed qualified as U (Vector)
import Data.Vector.Unboxed.Mutable qualified as UM (MVector)
import PyF (fmt, fmtTrim)
import Relude.Applicative
import Relude.Base
import Relude.Bool
import Relude.Container
import Relude.Debug
import Relude.DeepSeq
import Relude.Enum
import Relude.Exception
import Relude.File
import Relude.Foldable
import Relude.Function
import Relude.Functor
import Relude.Lifted
import Relude.List hiding (uncons)
import Relude.Monad
import Relude.Monoid
import Relude.Nub
import Relude.Numeric
import Relude.Print
import Relude.String

type UVector = U.Vector

type UMVector = UM.MVector

freezeArray :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
freezeArray = Data.Array.MArray.freeze

thawArray :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
thawArray = Data.Array.MArray.thaw

arrayBounds :: (Ix i, IArray a e) => a i e -> (i, i)
arrayBounds = Data.Array.Base.bounds
