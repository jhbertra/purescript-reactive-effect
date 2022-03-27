module Control.Reactive.Prim.Frame
  ( FrameM
  , runFrame
  , runLater
  , runOnNextFrame
  ) where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec, untilJust)
import Control.Reactive.Util (Subscriber)
import Data.Exists (Exists, mkExists, runExists)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error, throwException)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unlift (class MonadUnliftEffect)
import Effect.Unsafe (unsafePerformEffect)

data Job a = Job (FrameM a) (Subscriber a)

type Queue = Ref (List (Exists Job))

-- A computation that simulates simultaneous execution of multiple tasks by
-- queuing them and processing them in sequence.
newtype FrameM a = FrameM (ReaderT Queue Effect a)

derive instance Functor FrameM
derive newtype instance Apply FrameM
derive newtype instance Bind FrameM
derive newtype instance Applicative FrameM
derive newtype instance Monad FrameM
derive newtype instance MonadEffect FrameM
derive newtype instance MonadUnliftEffect FrameM
derive newtype instance MonadRec FrameM
derive newtype instance MonadThrow Error FrameM
derive newtype instance MonadError Error FrameM
derive newtype instance Lazy1 FrameM

runFrame :: forall a. FrameM a -> Effect a
runFrame (FrameM frame) = do
  queue <- Ref.new mempty
  result <- runReaderT frame queue
  untilJust $ drainQueue queue
  pure result
  where
  drainQueue queue = do
    jobs <- Ref.read queue
    Ref.write Nil queue
    case jobs of
      Nil -> pure $ Just unit
      _ -> Nothing <$ runJobs queue jobs
  runJobs _ Nil = pure unit
  runJobs queue (job : jobs) = do
    runJobs queue jobs
    runExists (runJob queue) job

  runJob :: forall b. Queue -> Job b -> Effect Unit
  runJob queue (Job (FrameM job) subscriber) =
    subscriber =<< runReaderT job queue

-- | Schedule an action to be run later in the current frame at some point.
-- |
-- | NOTE: care must be taken with the result, which is lazily evaluated and
-- | will throw an exception if it is accessed too soon. To safely access the
-- | result, the action that accesses it should also be run with `runLater`.
runLater :: forall a. Lazy a => FrameM a -> FrameM a
runLater action = FrameM do
  queue <- ask
  resultR <- liftEffect $ Ref.new Nothing
  liftEffect $ Ref.modify_
    (mkExists (Job action (flip Ref.write resultR <<< Just)) : _)
    queue
  pure $ defer \_ -> unsafePerformEffect do
    result <- Ref.read resultR
    case result of
      Nothing -> throwException $ error
        "***Control.Reactive.Prim.Frame.runLater: premature access to result"
      Just a -> pure a

runOnNextFrame :: FrameM Unit -> Effect Unit
runOnNextFrame = unsafeThrow "TODO not implemented yet"
