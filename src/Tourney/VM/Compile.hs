{-# LANGUAGE NoFieldSelectors #-}

-- | Compile a tournament into a stream of operations that can have a
-- straightforward interpreter implementation. Compilation relies on having an
-- underlying monad that can manipulate a stream of code.
--
-- This may seem overkill but it greatly simplifies things in the
-- implementation. The initial implementation generated a stream of rounds from
-- a tournament directly, but the stream of rounds had to actually be a stream
-- of stream of rounds, which had to be a stream of stream of streams of
-- matches. This is ultimately because the 'Tournament' language allows the
-- 'ByStandings' constructor anywhere, and because it can split or interleave
-- tournaments at will. While I could just flatten all matches within a round,
-- that feels antithetical to the goal of maximising how much of a tournament
-- can be statically analysed.
module Tourney.VM.Compile (
  -- * Code streams
  CodeStream,
  compile,
  codeSoFar,
  createStreamEnv,
  withGetStandings,
  debugCodeStream,

  -- * Reading code off of a stream
  MonadCodeStream (..),
  popCodeStream,

  -- * Debug
  debugExecCompiler,
  compile_,
) where

import Control.Monad.Codensity
import Data.Dependency qualified as S
import Tourney.Algebra
import Tourney.Stream
import Tourney.VM.Code
import VectorBuilder.Builder qualified as VB
import VectorBuilder.Vector qualified as VB
import Prelude hiding (Empty)

compile :: Monad m => Tournament t -> StreamEnv m -> CodeStream m
compile t env =
  CodeStream
    { builder = mempty
    , next =
        compile_ t
          `execCompiler` Env
            { focus = env ^. #focus
            , sort = def
            , streamEnv = env
            }
    }

codeSoFar :: CodeStream m -> Code
codeSoFar CodeStream{builder} = VB.build builder
codeSoFar CodeDone{seen} = seen

data CodeStream m
  = CodeStream {builder :: !CodeBuilder, next :: !(S.StreamM m TourneyOp ())}
  | CodeDone {seen :: Code}
  deriving stock (Generic)

class (Monad m, MonadPrim RealWorld m, Monad c) => MonadCodeStream m c | m -> c where
  getCodeStream :: m (CodeStream c)
  putCodeStream :: CodeStream c -> m ()
  runCodeStreamEffect :: c a -> m a

-- | Get the next 'TourneyOp' from the code stream.
popCodeStream :: MonadCodeStream m c => m (Maybe TourneyOp)
popCodeStream = do
  getCodeStream >>= \case
    CodeDone _code -> pure Nothing
    CodeStream{builder, next} ->
      runCodeStreamEffect (S.popForced next) >>= \case
        Just (x, xs) -> do
          putCodeStream CodeStream{builder = builder <> VB.singleton x, next = xs}
          pure (Just x)
        Nothing -> do
          -- We have reached the end of the stream, so we can finalise it
          putCodeStream (CodeDone (VB.build builder))
          pure Nothing

debugCodeStream :: MonadCodeStream m c => m Code
debugCodeStream = do
  getCodeStream <&> \case
    CodeDone c -> c
    CodeStream b n -> VB.build (b <> VB.vector (S.pureVector n))

--------------------------------------------------------------------------------

newtype Compiler m a
  = Compiler
      (forall r. (a -> Env m -> S.StreamM m TourneyOp r) -> Env m -> S.StreamM m TourneyOp r)
  deriving
    ( Functor
    , Monad
    , Applicative
    , MonadReader (Env m)
    )
    via (Codensity (ReaderT (Env m) (S.StreamM m TourneyOp)))

data Env m = Env
  { focus :: !Focus
  , sort :: !SortMethod
  , streamEnv :: !(StreamEnv m)
  }
  deriving stock (Generic)

instance MonadTrans Compiler where
  lift m = Compiler \ret e -> S.lift m >>= (`ret` e)

{-# INLINE emit #-}
emit :: Monad m => TourneyOp -> Compiler m ()
emit o = Compiler \ret e -> S.yield o >> ret () e

execCompiler :: Monad m => Compiler m a -> Env m -> S.StreamM m TourneyOp ()
execCompiler (Compiler m) = m (\_ _ -> pure ())

debugExecCompiler :: Tournament t -> Code
debugExecCompiler t =
  S.pureVector
    ( execCompiler
        (compile_ t)
        Env
          { focus = Focus 0 8
          , sort = def
          , streamEnv = createStreamEnv @Identity 8
          }
    )

--------------------------------------------------------------------------------

compile_ :: Monad m => Tournament t -> Compiler m ()
compile_ t0 = do
  env <- view #streamEnv
  -- We need to go through a round stream rather than directly compile a
  -- tournament in order to basically make sure that all sequences are on the
  -- "outside" of all overlays. createRoundStream handles that transformation
  -- for us
  let s = unRoundStream (createRoundStream t0) env
  S.for_ (S.hoistT s) \r -> do
    emit BEGIN_ROUND
    compile1_ r
    emit END_ROUND

-- | Compile within a tournament round
compile1_ :: Monad m => Tournament TOne -> Compiler m ()
compile1_ = \case
  Empty -> pure ()
  One m -> emit (MATCH m)
  Sort method t -> do
    focus <- view #focus
    local (#sort .~ method) (compile1_ t)
    emit (PERFORM_SORTING focus method)
  Overlay a b -> do
    compile1_ a
    compile1_ b
  ByPlayerCount byCount -> do
    f <- view #focus
    compile1_ (byCount (f ^. #focusLength))
  ByStandings byStandings -> do
    focus <- view #focus
    getter <- view (#streamEnv . #getStandings)
    standings <- lift (getter focus)
    compile1_ (byStandings standings)
