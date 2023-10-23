module Data.Function.Bound where

import BasePrelude (Show (..))
import Data.Typeable
import Prelude hiding (Const)

class (Eq ident, Typeable ident) => IsBoundFn ident a b | ident -> a b where
  run :: ident -> a -> b

data a ~> b where
  ConstFn :: b -> a ~> b
  BoundFn :: IsBoundFn ident a b => ident -> a ~> b

instance Eq b => Eq (a ~> b) where
  ConstFn a == ConstFn b = a == b
  BoundFn a == BoundFn b = case cast b of
    Just b' -> a == b'
    Nothing -> False
  _ == _ = False

apply :: (a ~> b) -> a -> b
apply (ConstFn a) _ = a
apply (BoundFn i) a = run i a

instance (Typeable a, Typeable b, Show b) => Show (a ~> b) where
  showsPrec p (ConstFn b) = showsPrec p b
  showsPrec p (BoundFn f) = showsPrec p (typeOf f)

newtype UnknownFn a b = UnknownFn (a -> b)

instance Eq (UnknownFn a b) where
  _ == _ = False

instance (Typeable a, Typeable b) => IsBoundFn (UnknownFn a b) a b where
  run (UnknownFn f) = f

data ComposedFn a b where
  ComposedFn :: (Eq x, Typeable x) => (a ~> x) -> (x ~> b) -> ComposedFn a b

instance (Typeable a, Typeable b, Eq b) => Eq (ComposedFn a b) where
  ComposedFn f0 g0 == ComposedFn f1 g1 =
    case cast (f1, g1) of
      Just (f1', g1') -> f0 == f1' && g0 == g1'
      _ -> False

instance (Typeable a, Typeable b, Eq b) => IsBoundFn (ComposedFn a b) a b where
  run (ComposedFn f g) = apply g . apply f
