module Effect.Reactive.Internal.Pipe where

import Prelude

import Data.Exists (mkExists)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.WeakBag as WeakBag
import Effect.Class (liftEffect)
import Effect.RW (runRWEffect)
import Effect.Reactive.Internal
  ( Behaviour
  , Pipe
  , PullM
  , PullSubscriber(..)
  , PullSubscription(..)
  , trackSubscriber
  )
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

_pull :: PullM ~> Behaviour
_pull evaluate = unsafePerformEffect do
  cache <- Ref.new Nothing
  pure $ pipeBehaviour { evaluate, cache }

pipeBehaviour :: Pipe ~> Behaviour
pipeBehaviour pipe = do
  mCache <- liftEffect $ Ref.read pipe.cache
  cache <- case mCache of
    Just cache -> pure cache
    Nothing -> do
      Tuple subscription value <- liftEffect $ runRWEffect pipe.evaluate
        $ Just
        $ mkExists
        $ PipeSubscriber pipe
      subscribers <- liftEffect $ WeakBag.new case subscription of
        Inactive -> pure unit
        Active { unsubscribe } -> unsubscribe
      let cache = { subscription, subscribers, value }
      liftEffect $ Ref.write (Just cache) pipe.cache
      pure cache
  trackSubscriber cache.subscribers
  pure $ cache.value
