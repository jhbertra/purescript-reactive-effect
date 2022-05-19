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
  , BehaviourSubscription(..)
  , Pipe
  , SampleHint(..)
  , readBehaviourUntracked
  , tellHint
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
      Tuple subscription value <- liftEffect
        $ runRWEffect pipe.evaluate
        $ Just
        $ mkExists
        $ PipeSubscriber pipe
      subscribers <- liftEffect $ WeakBag.new case subscription of
        Inactive -> pure unit
        Active { unsubscribe } -> unsubscribe
      let
        getValue = case subscription of
          Active { hint: SampleContinuous } ->
            readBehaviourUntracked pipe.evaluate
          _ -> pure value
      let cache = { subscription, subscribers, getValue }
      liftEffect $ RM.write cache pipe.cache
      pure cache
  case cache.subscription of
    Active { hint } -> tellHint hint
    _ -> pure unit
  trackSubscriber cache.subscribers
  liftEffect $ cache.getValue
