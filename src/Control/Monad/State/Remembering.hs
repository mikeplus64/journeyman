module Control.Monad.State.Remembering where

newtype RememberingStateT s m a = RememberingStateT
  { runRememberingStateT ::
      forall r.
      s ->
      [s] ->
      (s -> a -> m r) ->
      m r
  }

instance Functor (RememberingStateT s m) where
  fmap f (RememberingStateT x) = RememberingStateT \s h c -> x s h (\s' a' -> _)
