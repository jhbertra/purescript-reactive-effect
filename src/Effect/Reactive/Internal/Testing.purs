module Effect.Reactive.Internal.Testing where

import Prelude

import Data.Align (aligned)
import Data.Array as Array
import Data.Compactable (compact)
import Data.Exists (mkExists)
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.OrderedBag as OB
import Data.These (theseLeft, theseRight)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( BuildM
  , EventRep
  , FireTriggers(..)
  , InvokeTrigger(..)
  , getEventHandle
  )
import Effect.Reactive.Internal.Build (fireAndRead, runBuildM, runFrame)
import Effect.Reactive.Internal.Input (inputEvent, newInputWithTriggerRef)
import Effect.Ref as Ref
import Effect.Ref.Maybe as RM

interpret
  :: forall a b
   . (EventRep a -> BuildM (EventRep b))
  -> Array (Maybe a)
  -> Effect (Array (Maybe b))
interpret f as = interpret2 (\e _ -> f e) as []

interpret2
  :: forall a b c
   . (EventRep a -> EventRep b -> BuildM (EventRep c))
  -> Array (Maybe a)
  -> Array (Maybe b)
  -> Effect (Array (Maybe c))
interpret2 f as bs = do
  currentTime <- Ref.new $ Milliseconds 0.0
  postBuildSubscribers <- OB.new
  let
    postBuild subscriber = liftEffect do
      _ <- OB.insert subscriber postBuildSubscribers
      depth <- Ref.new 0
      pure
        { subscription:
            { unsubscribe: OB.delete subscriber postBuildSubscribers
            , depth
            }
        , occurrence: Nothing
        }
  runBuildM (pure postBuild) (Ref.read currentTime) (FireTriggers mempty) do
    result <- liftEffect $ Ref.new []
    input1 <- liftEffect newInputWithTriggerRef
    input2 <- liftEffect newInputWithTriggerRef
    let ea = inputEvent input1.input
    let eb = inputEvent input2.input
    ec <- f ea eb
    handle <- runFrame do
      liftEffect (OB.toArray postBuildSubscribers) >>= traverse_ \subscriber ->
        subscriber.propagate unit
      getEventHandle ec
    forWithIndex_ (aligned as bs) \time mab -> do
      liftEffect $ Ref.write (Milliseconds $ toNumber time) currentTime
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
