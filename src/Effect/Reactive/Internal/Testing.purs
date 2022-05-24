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
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( BuildM
  , EventRep
  , FireTriggers(..)
  , InvokeTrigger(..)
  , Time(..)
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
  queue <- Queue.new
  liftEffect do
    currentTime <- Ref.new $ Time 0.0
    result <- Ref.new []
    input1 <- newInputWithTriggerRef
    input2 <- newInputWithTriggerRef
    let ea = inputEvent input1.input
    let eb = inputEvent input2.input
    r <- runBuildM queue (Ref.read currentTime) do
      ec <- f ea eb
      runFrame $ getEventHandle ec
    let FireTriggers fire = r.fire
    let handle = r.result
    forWithIndex_ (aligned as bs) \time mab -> do
      Ref.write (Time $ toNumber time) currentTime
      mTrigger1 <- RM.read input1.trigger
      mTrigger2 <- RM.read input2.trigger
      let
        mInvokeTrigger1 = do
          trigger <- mTrigger1
          value <- join $ theseLeft mab
          pure $ mkExists $ InvokeTrigger $ { trigger, value }
        mInvokeTrigger2 = do
          trigger <- mTrigger2
          value <- join $ theseRight mab
          pure $ mkExists $ InvokeTrigger $ { trigger, value }
      fire (compact [ mInvokeTrigger1, mInvokeTrigger2 ]) do
        mc <- RM.read handle.currentValue
        Ref.modify_ (\cs -> Array.snoc cs mc) result
    r.dispose
    Ref.read result
