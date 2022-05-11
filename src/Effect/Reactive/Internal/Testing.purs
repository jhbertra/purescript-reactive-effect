module Effect.Reactive.Internal.Testing where

import Prelude

import Data.Align (aligned)
import Data.Array as Array
import Data.Compactable (compact)
import Data.Exists (mkExists)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.These (theseLeft, theseRight)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( BuildM
  , Event
  , FireTriggers(..)
  , InvokeTrigger(..)
  , getEventHandle
  )
import Effect.Reactive.Internal.Build (fireAndRead, runBuildM, runFrame)
import Effect.Reactive.Internal.Input (inputEvent, newInputWithTriggerRef)
import Effect.Ref as Ref
import Effect.Ref.Maybe as RM

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

interpret
  :: forall a b
   . (Event a -> BuildM (Event b))
  -> Array (Maybe a)
  -> Effect (Array (Maybe b))
interpret f as = interpret2 (\e _ -> f e) as []

interpret2
  :: forall a b c
   . (Event a -> Event b -> BuildM (Event c))
  -> Array (Maybe a)
  -> Array (Maybe b)
  -> Effect (Array (Maybe c))
interpret2 f as bs = runBuildM (FireTriggers mempty) do
  result <- liftEffect $ Ref.new []
  input1 <- liftEffect newInputWithTriggerRef
  input2 <- liftEffect newInputWithTriggerRef
  let ea = inputEvent input1.input
  let eb = inputEvent input2.input
  ec <- f ea eb
  handle <- runFrame $ getEventHandle ec
  for_ (aligned as bs) \mab -> do
    mTrigger1 <- liftEffect $ RM.read input1.trigger
    mTrigger2 <- liftEffect $ RM.read input2.trigger
    let
      mInvokeTrigger1 = do
        trigger <- mTrigger1
        value <- join $ theseLeft mab
        pure $ mkExists $ InvokeTrigger $ { trigger, value }
      mInvokeTrigger2 = do
        trigger <- mTrigger2
        value <- join $ theseRight mab
        pure $ mkExists $ InvokeTrigger $ { trigger, value }
    fireAndRead (compact [ mInvokeTrigger1, mInvokeTrigger2 ]) do
      mc <- RM.read handle.currentValue
      Ref.modify_ (\cs -> Array.snoc cs mc) result
  liftEffect $ Ref.read result
