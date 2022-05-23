module Effect.Reactive.Internal.Pipe where

import Prelude

import Data.Exists (mkExists)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.WeakBag as WeakBag
import Effect.Class (liftEffect)
import Effect.RW (runRWEffect)
import Effect.Reactive.Internal
  ( BehaviourRep
  , BehaviourSubscriber(..)
  , Pipe
  , trackSubscriber
  )
import Effect.Ref.Maybe as RM
import Effect.Unsafe (unsafePerformEffect)

_pull :: BehaviourRep ~> BehaviourRep
_pull evaluate = unsafePerformEffect do
  cache <- RM.empty
  pure $ pipeBehaviour { evaluate, cache }

pipeBehaviour :: Pipe ~> BehaviourRep
pipeBehaviour pipe = do
  mCache <- liftEffect $ RM.read pipe.cache
  cache <- case mCache of
    Just cache -> pure cache
    Nothing -> do
      Tuple invalidator value <- liftEffect
        $ runRWEffect pipe.evaluate
        $ Just
        $ mkExists
        $ PipeSubscriber pipe
      subscribers <- liftEffect $ WeakBag.new invalidator
      let cache = { invalidator, subscribers, value }
      liftEffect $ RM.write cache pipe.cache
      pure cache
  trackSubscriber cache.subscribers
  pure cache.value
