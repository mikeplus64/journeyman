-- | Reified functions; functions that are "well-known", and can be tested for
-- equality
--
-- This enables functional equality
module Data.Function.Known where

import BasePrelude (Show (showsPrec))
import Data.Bits
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Typeable
import Prelude

class (Eq ident, Typeable ident, Typeable a, Typeable b) => KnownFunction ident a b | ident -> a b where
  run :: ident -> a -> b
  properties :: ident -> Set FnProperty
  properties _ = mempty

data FnProperty
  = Surjective
  | Injective
  deriving stock (Eq, Ord, Show)

bijective, injective, surjective :: Set FnProperty
bijective = Set.fromList [Surjective, Injective]
injective = Set.fromList [Injective]
surjective = Set.fromList [Surjective]

apply :: (a ~> b) -> a -> b
apply (KnownFn i) = run i

data a ~> b where
  KnownFn :: KnownFunction ident a b => ident -> a ~> b

instance Eq (a ~> b) where
  KnownFn a == KnownFn b = case cast b of
    Just b' -> a == b'
    Nothing -> False

instance (Typeable a, Typeable b, Eq b) => KnownFunction (a ~> b) a b where
  run (KnownFn f) = run f
  properties (KnownFn f) = properties f

instance (Typeable a, Typeable b, Show b) => Show (a ~> b) where
  showsPrec p (KnownFn f) = showsPrec p (typeOf f)

--------------------------------------------------------------------------------
-- Sum functions

data SumFn a b c = SumFn (a ~> c) (b ~> c)
  deriving stock (Eq, Show)

instance (Typeable a, Typeable b, Typeable c, Eq a, Eq b, Eq c) => KnownFunction (SumFn a b c) (Either a b) c where
  run (SumFn f _) (Left a) = run f a
  run (SumFn _ g) (Right b) = run g b

pattern SumFnK :: (Typeable a, Typeable b, Typeable c, Eq a, Eq b, Eq c) => (a ~> c) -> (b ~> c) -> (Either a b ~> c)
pattern SumFnK f g <- KnownFn (cast -> Just (SumFn f g))
  where
    SumFnK f g = KnownFn (SumFn f g)

--------------------------------------------------------------------------------
-- Const function

data ConstFn (a :: Type) (b :: Type) = ConstFn b
  deriving stock (Eq, Show)

instance (Typeable a, Typeable b, Eq b) => KnownFunction (ConstFn a b) a b where
  run (ConstFn b) _ = b

castConstFn
  :: forall (b :: Type) (a :: Type) q
   . (Typeable a, Typeable b, Typeable q)
  => q
  -> Maybe (ConstFn a b)
castConstFn = cast

pattern ConstFnK
  :: forall (a :: Type) (b :: Type)
   . (Typeable a, Typeable b, Eq b)
  => b
  -> (a ~> b)
pattern ConstFnK a <- KnownFn (castConstFn @b @a -> Just (ConstFn a))
  where
    ConstFnK a = KnownFn (ConstFn a)

--------------------------------------------------------------------------------
-- Composed functions

data ComposedFn a b where
  ComposedFn :: (Eq x, Typeable x) => (a ~> x) -> (x ~> b) -> ComposedFn a b

instance (Typeable a, Typeable b, Eq b) => Eq (ComposedFn a b) where
  ComposedFn f0 g0 == ComposedFn f1 g1 =
    case cast (f1, g1) of
      Just (f1', g1') -> f0 == f1' && g0 == g1'
      _ -> False

instance (Typeable a, Typeable b, Eq b) => KnownFunction (ComposedFn a b) a b where
  run (ComposedFn f g) = apply g . apply f
  properties (ComposedFn f g) =
    properties f `Set.intersection` properties g

castComposedFn
  :: forall a x b q
   . (Typeable q, Typeable a, Typeable x, Typeable b)
  => q
  -> Maybe (a ~> x, x ~> b)
castComposedFn q = do
  ComposedFn mf mg :: ComposedFn a b <- cast q
  cast (mf, mg)

pattern (:>>>)
  :: forall a x b
   . (Typeable a, Typeable b, Eq x, Eq b, Typeable x)
  => (a ~> x)
  -> (x ~> b)
  -> (a ~> b)
pattern (:>>>) f g <- KnownFn (castComposedFn -> Just (f, g))
  where
    (:>>>) f g = KnownFn (ComposedFn f g)

infixr 2 :>>>

--------------------------------------------------------------------------------
-- Mappings

data Mapping a b = Mapping (Map.Map a b) b
  deriving stock (Eq, Show)

instance (Typeable a, Typeable b, Ord a, Eq b) => KnownFunction (Mapping a b) a b where
  run (Mapping m def) a = def `fromMaybe` Map.lookup a m

pattern MappingK :: (Typeable a, Typeable b, Ord a, Eq b) => Map.Map a b -> b -> a ~> b
pattern MappingK m def <- KnownFn (cast -> Just (Mapping m def))
  where
    MappingK m def = KnownFn (Mapping m def)

--------------------------------------------------------------------------------
-- Numeric

newtype Plus a = Plus a
  deriving stock (Eq, Show)

instance (Typeable a, Ord a, Eq a, Num a) => KnownFunction (Plus a) a a where
  run (Plus a) b = a + b
  properties _ = bijective

pattern PlusK :: (Num a, Ord a, Eq a, Typeable a) => a -> (a ~> a)
pattern PlusK a <- KnownFn (cast -> Just (Plus a))
  where
    PlusK a = KnownFn (Plus a)

newtype Minus a = Minus a
  deriving stock (Eq, Show)

instance (Typeable a, Ord a, Eq a, Num a) => KnownFunction (Minus a) a a where
  run (Minus a) b = a - b
  properties _ = bijective

pattern MinusK :: (Num a, Ord a, Eq a, Typeable a) => a -> (a ~> a)
pattern MinusK a <- KnownFn (cast -> Just (Minus a))
  where
    MinusK a = KnownFn (Minus a)

newtype Mul a = Mul a
  deriving stock (Eq, Show)

instance (Typeable a, Ord a, Eq a, Num a) => KnownFunction (Mul a) a a where
  run (Mul a) b = a * b
  properties _ = injective

pattern MulK :: (Num a, Ord a, Eq a, Typeable a) => a -> (a ~> a)
pattern MulK a <- KnownFn (cast -> Just (Mul a))
  where
    MulK a = KnownFn (Mul a)

newtype Div a = Div a
  deriving stock (Eq, Show)

instance (Typeable a, Ord a, Eq a, Integral a) => KnownFunction (Div a) a a where
  run (Div q) a = a `div` q
  properties _ = injective

pattern DivK :: (Integral a, Ord a, Eq a, Typeable a) => a -> (a ~> a)
pattern DivK a <- KnownFn (cast -> Just (Div a))
  where
    DivK a = KnownFn (Div a)

newtype Mod a = Mod a
  deriving stock (Eq, Show)

instance (Typeable a, Ord a, Eq a, Integral a) => KnownFunction (Mod a) a a where
  run (Mod q) a = a `mod` q
  properties _ = injective

pattern ModK :: (Integral a, Ord a, Eq a, Typeable a) => a -> (a ~> a)
pattern ModK a <- KnownFn (cast -> Just (Mod a))
  where
    ModK a = KnownFn (Mod a)

--------------------------------------------------------------------------------

ex :: Int8 ~> Int8
ex =
  MulK 2
    :>>> PlusK 4
    :>>> DivK 2

--------------------------------------------------------------------------------
-- Exhaustively determining function properties

split16 :: Word16 -> (Word8, Word8)
split16 w =
  ( fromIntegral ((w `shiftR` 0) .&. 0xff)
  , fromIntegral ((w `shiftR` 8) .&. 0xff)
  )

implies :: Bool -> Bool -> Bool
implies False _ = True
implies True True = True
implies _ _ = False

nonnull :: [a] -> Bool
nonnull = not . null

isInjective :: (Bounded a, Enum a, Eq a, Eq b) => (a -> b) -> Bool
isInjective f = nonnull do
  x <- universe
  x' <- universe
  guard ((x == x') `implies` (f x == f x'))

isSurjective :: (Bounded a, Bounded b, Enum a, Enum b, Eq b) => (a -> b) -> Bool
isSurjective f = all nonnull do
  y <- universe
  pure do
    x <- universe
    guard (f x == y)

isBijective :: (Bounded a, Bounded b, Enum a, Enum b, Eq a, Eq b) => (a -> b) -> Bool
isBijective f = isInjective f && isSurjective f
