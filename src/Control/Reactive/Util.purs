module Control.Reactive.Util where

import Prelude

import Data.Tuple.Nested (type (/\))
import Effect (Effect)

-- An action that registers a subscriber and returns an action used to cancel
-- the subscription.
type Subscribe a = Subscribe' Effect a
type Subscribe' m a = Subscriber' m a -> Effect Canceller

-- An action to run when an event occurs.
type Subscriber a = Subscriber' Effect a
type Subscriber' m a = a -> m Unit

-- An effect that can cancel a pending operation.
type Canceller = Effect Unit

-- An action that creates a subscriber and a canceller.
type Publish a = Publish' Effect a
type Publish' m a = Effect (Subscriber' m a /\ Canceller)
