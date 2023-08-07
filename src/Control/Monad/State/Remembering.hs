module Control.Monad.State.Remembering where

newtype RememberingStateT s m a = RememberingStateT
  { runRememberingStateT ::
      forall r.
      s ->
      [s] ->
      (s -> a -> m r) ->
      m r
  }
