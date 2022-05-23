module Effect.Reactive.Internal.Perform where

import Prelude

import Data.Exists (mkExists)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( BuildM
  , EventRep
  , Perform(..)
  , PerformParent(..)
  , RunPerform(..)
  , _subscribe
  , runPerform
  , terminalSubscriber
  )
import Effect.Reactive.Internal.Build (addPerform, groundLater)
import Effect.Reactive.Internal.Input (inputEvent, newInputWithTriggerRef)
import Effect.Ref as Ref

_perform
  :: forall a r
   . PerformParent a r
  -> BuildM (EventRep a)
_perform parent@(PerformParent { setup, teardown, event }) = do
  { input, trigger } <- liftEffect $ newInputWithTriggerRef
  let perform = Perform { parent: mkExists parent, responseTriggerRef: trigger }
  addPerform perform
  groundLater \_ -> do
    cancelsRef <- liftEffect $ Ref.new mempty
    resource <- liftEffect $ setup
    let
      handleCancel cancel = Ref.modify_ (_ *> cancel) cancelsRef
      runPerform' register = runPerform $ RunPerform
        { perform
        , register: register resource
        , handleCancel
        }
    { occurrence, subscription } <-
      _subscribe event $ terminalSubscriber runPerform'
    traverse_ runPerform' occurrence
    pure
      { occurrence: Nothing
      , subscription: subscription
          { unsubscribe = do
              subscription.unsubscribe
              join $ Ref.read cancelsRef
              teardown resource
          }
      }
  pure $ inputEvent input
