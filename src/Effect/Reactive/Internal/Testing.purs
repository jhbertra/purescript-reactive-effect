module Effect.Reactive.Internal.Testing where

import Prelude

import Concurrent.Queue as Queue
import Data.Align (aligned)
import Data.Array as Array
import Data.Compactable (compact)
import Data.Exists (mkExists)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.These (theseLeft, theseRight)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( BuildM
  , EventRep
  , InvokeTrigger(..)
  , _neverE
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
  -> Aff (Array (Maybe b))
interpret f as = interpret2 (\e _ -> f e) as []

interpret2
  :: forall a b c
   . (EventRep a -> EventRep b -> BuildM (EventRep c))
  -> Array (Maybe a)
  -> Array (Maybe b)
  -> Aff (Array (Maybe c))
interpret2 f as bs = do
  currentTime <- liftEffect $ Ref.new $ Milliseconds 0.0
  queue <- Queue.new
  r <- liftEffect $ runBuildM queue (pure _neverE) (Ref.read currentTime) do
    result <- liftEffect $ Ref.new []
    input1 <- liftEffect newInputWithTriggerRef
    input2 <- liftEffect newInputWithTriggerRef
    let ea = inputEvent input1.input
    let eb = inputEvent input2.input
    ec <- f ea eb
    handle <- runFrame $ getEventHandle ec
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
  pure r.result
