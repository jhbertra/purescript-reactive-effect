module Effect.Reactive.Internal.Reaction where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( BuildM
  , EventRep
  , InitializeReaction
  , Reaction(..)
  , Reactor(..)
  , _subscribe
  , raiseReaction
  , terminalSubscriber
  )
import Effect.Reactive.Internal.Build (addReactor)
import Effect.Ref.Maybe as RM

_react :: forall a. EventRep a -> InitializeReaction a -> BuildM (Effect Unit)
_react event initialize = do
  connection <- liftEffect RM.empty
  subscriptionRef <- liftEffect RM.empty
  let
    reactor = Reactor
      { connection
      , subscription: subscriptionRef
      , initialize: do
          { subscription, occurrence } <-
            _subscribe event $ terminalSubscriber \value ->
              raiseReaction $ Reaction { reactor, value }
          liftEffect do
            con <- initialize occurrence
            RM.write con connection
            RM.write subscription subscriptionRef
      }
  addReactor reactor
  pure do
    RM.read connection >>= traverse_ _.dispose
    RM.read subscriptionRef >>= traverse_ _.unsubscribe
    RM.clear connection
    RM.clear subscriptionRef
