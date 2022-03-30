module Control.Reactive.Prim.Frame.Moment
  ( FrameMoment
  , FrameTime
  , runFrameMoment
  , runLater
  , runLater_
  , scheduleFrame
  , scheduleFrameEffect
  ) where

import Prelude

import Control.Lazy.Lazy1 (class Lazy1)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Fix (class MonadFix)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Rec.Class (class MonadRec, untilJust)
import Control.Reactive.Moment (class MonadAdjustMoment, class MonadMoment)
import Control.Reactive.Util (Subscribe, Subscriber)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Lazy (Lazy, defer)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error, throwException)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unlift (class MonadUnliftEffect)
import Effect.Unsafe (unsafePerformEffect)

-- | An action that can be scheduled on a frame's queue.
data Action a = Action (FrameMoment a) (Subscriber a)

-- | A queue of existentialized actions.
type Queue = Ref (List (Exists Action))

-- | The implementation uses DOMHighResTimestamps for time, which are just
-- | Numbers obtained either via the performance API or requestAnimationFrame.
type FrameTime = Number

type FrameMomentContext =
  { queue :: Queue
  , time :: FrameTime
  }

-- A computation that simulates simultaneous execution of multiple tasks by
-- queuing them and processing them in sequence.
newtype FrameMoment a = FrameMoment (ReaderT FrameMomentContext Effect a)

derive instance Functor FrameMoment
derive newtype instance Apply FrameMoment
derive newtype instance Bind FrameMoment
derive newtype instance Applicative FrameMoment
derive newtype instance Monad FrameMoment
derive newtype instance MonadEffect FrameMoment
derive newtype instance MonadUnliftEffect FrameMoment
derive newtype instance MonadRec FrameMoment
derive newtype instance MonadFix FrameMoment
derive newtype instance MonadThrow Error FrameMoment
derive newtype instance MonadError Error FrameMoment
derive newtype instance Lazy1 FrameMoment
instance MonadMoment FrameTime FrameMoment where
  moment = FrameMoment $ asks _.time

instance MonadAdjustMoment FrameTime FrameMoment where
  adjustMoment f (FrameMoment m) = FrameMoment $
    local (\c -> c { time = f c.time }) m

runFrameMoment :: forall a. FrameMoment a -> FrameTime -> Effect a
runFrameMoment (FrameMoment frame) time = do
  queue <- Ref.new mempty
  let context = { queue, time }
  result <- runReaderT frame context
  untilJust $ drainQueue context
  pure result
  where
  drainQueue context = do
    jobs <- Ref.read context.queue
    Ref.write Nil context.queue
    case jobs of
      Nil -> pure $ Just unit
      _ -> Nothing <$ runJobs context jobs
  runJobs _ Nil = pure unit
  runJobs queue (job : jobs) = do
    runJobs queue jobs
    runExists (runJob queue) job

  runJob :: forall b. FrameMomentContext -> Action b -> Effect Unit
  runJob context (Action (FrameMoment job) subscriber) =
    subscriber =<< runReaderT job context

-- | Schedule an action to be run later in the current frame at some point.
-- |
-- | NOTE: care must be taken with the result. Specifically, it is not safe to
-- | call `force` on it unless you do so in a subsequent `runLater` block. But
-- | care must still be taken here, do blocks are eagerly evaluated! The first
-- | expression, binder, or let-binding in a do block will be evaluated when it
-- | is create, not when it is evaluated. This is because of how it is
-- | desugared:
-- |
-- | ```purescript
-- | do
-- |   foo <- foo $ force someLazyValue
-- |   bar foo
-- |
-- | -- desugared - someLazyValue is forced!
-- | (foo $ force someLazyValue) >>= \foo -> bar foo
-- | ```
-- |
-- | A workaround for this is to start a do-block with `pure unit`, which
-- | moves everything after into a bind continuation.
-- |
-- | ```purescript
-- | do
-- |   pure unit
-- |   foo <- foo $ force someLazyValue
-- |   bar foo
-- |
-- | -- desugared - someLazyValue is not forced until the monadic action is
-- | -- run.
-- | pure unit >>= \_ -> (foo $ force someLazyValue) >>= \foo -> bar foo
-- | ```
-- |
-- | ```purescript
-- | -- unsafe:
-- | do
-- |   a <- runLater getA
-- |   -- ***Control.Reactive.Prim.Frame.runLater: premature access to result
-- |   -- runtime exception will be thrown.
-- |   b <- doSomethingWithA $ force a
-- |   ...
-- |
-- | -- also unsafe:
-- | do
-- |   a <- runLater getA
-- |   b <- runLater $ doSomethingWithA $ force a
-- |   ...
-- |
-- | -- also unsafe:
-- | do
-- |   a <- runLater getA
-- |   b <- runLater do
-- |     doSomethingWithA $ force a
-- |   ...
-- |
-- | -- safe:
-- | do
-- |   a <- runLater getA
-- |   b <- runLater do
-- |     pure unit
-- |     doSomethingWithA $ force a
-- |   ...
-- | ```
runLater :: forall a. FrameMoment a -> FrameMoment (Lazy a)
runLater action = FrameMoment do
  queue <- asks _.queue
  resultR <- liftEffect $ Ref.new Nothing
  liftEffect $ Ref.modify_
    (mkExists (Action action (flip Ref.write resultR <<< Just)) : _)
    queue
  pure $ defer \_ -> unsafePerformEffect do
    result <- Ref.read resultR
    case result of
      Nothing -> throwException $ error
        "***Control.Reactive.Prim.Frame.runLater: premature access to result"
      Just a -> pure a

-- | A variant of `runLater` that discards the result. Generally more safe to
-- | use since it avoids the potential issue of premature access to the result
-- | of the computation.
runLater_ :: forall a. FrameMoment a -> FrameMoment Unit
runLater_ = void <<< runLater

-- | Schedule a computation to run on the next frame. Returns an Aff that can
-- | be used to retreeive the result asynchronously. Killing the Aff will
-- | cancel the action, and it will not run on the next frame. If a frame is
-- | already occurring, it will be promptly scheduled on the current frame.
scheduleFrame :: FrameMoment ~> Aff
scheduleFrame action = makeAff \resolve -> do
  subscribe <- scheduleFrameEffect action
  effectCanceler <$> subscribe (resolve <<< Right)

-- | Schedule a computation to run on the next frame in the Effect monad.
-- | Returns a `Subscribe` that can be used to subscribe to the result
-- | asynchronously. Subscribing to the result is necessary for the action to
-- | actually be scheduled, though multiple subscriptions will only result in
-- | it being scheduled once, and it will not be cancelled unless all
-- | subscribers cancel their subscription. `scheduleFrame` is probably a
-- | simpler API to use.
scheduleFrameEffect :: forall a. FrameMoment a -> Effect (Subscribe a)
scheduleFrameEffect = unsafeThrow "TODO not implemented yet"
