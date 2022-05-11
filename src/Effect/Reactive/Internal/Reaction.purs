module Effect.Reactive.Internal.Reaction where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( BuildM
  , Event
  , InitializeReaction
  , Reaction(..)
  , Reactor(..)
  , raiseReaction
  , subscribeAndRead
  , terminalSubscriber
  )
import Effect.Reactive.Internal.Build (addReactor)
import Effect.Ref.Maybe as RM

_react :: forall a. Event a -> InitializeReaction a -> BuildM (Effect Unit)
_react event initialize = do
  connection <- liftEffect RM.empty
  subscriptionRef <- liftEffect RM.empty
  let
    reactor = Reactor
      { connection
      , subscription: subscriptionRef
      , initialize: do
          { subscription, occurrence } <-
            subscribeAndRead event $ terminalSubscriber \value ->
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
