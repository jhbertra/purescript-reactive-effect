module Effect.Reactive.Internal.Latch where

import Prelude

import Data.Exists (runExists)
import Data.Foldable (for_, traverse_)
import Data.Identity (Identity(..))
import Data.Patch (class Patch, applyPatch)
import Data.WeakBag as WeakBag
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( BehaviourRep
  , EventRep
  , Latch(..)
  , LatchUpdate(..)
  , PullHint(..)
  , PullSource(..)
  , _subscribe
  , invalidatePullSubscriber
  , tellHint
  , tellSource
  , terminalSubscriber
  , trackSubscriber
  , updateLatch
  )
import Effect.Reactive.Internal.Build (class MonadBuild, initializeLatch)
import Effect.Ref as Ref
import Effect.Ref.Maybe as RM
import Safe.Coerce (coerce)

newLatch
  :: forall m patch a
   . MonadBuild m
  => Patch patch a
  => a
  -> EventRep patch
  -> m (Latch a)
newLatch initialValue updateOn = do
  parentRef <- liftEffect $ RM.empty
  subscribers <- liftEffect $ WeakBag.new mempty
  valueRef <- liftEffect $ Ref.new initialValue
  let
    latch = Latch
      { value: valueRef
      , subscribers
      , initialize: do
          pure unit
          unlessM (liftEffect $ RM.isFilled parentRef) do
            { occurrence, subscription } <- _subscribe updateOn
              $ terminalSubscriber
              $ maybeUpdateLatch valueRef subscribers
            traverse_ (maybeUpdateLatch valueRef subscribers) occurrence
            liftEffect $ RM.write subscription parentRef
      }
  initializeLatch latch
  pure latch
  where
  maybeUpdateLatch valueRef subscribers patch = do
    oldValue <- liftEffect $ Ref.read valueRef
    for_ (applyPatch patch oldValue) \newValue -> updateLatch $ LatchUpdate
      { valueRef
      , newValue
      , invalidateOld: WeakBag.traverseMembers_
          (runExists invalidatePullSubscriber)
          subscribers
      }

latchBehaviour :: forall a. Latch a -> BehaviourRep a
latchBehaviour (Latch latch) = do
  value <- liftEffect $ Ref.read latch.value
  tellHint PullOnce
  tellSource $ LatchSource $ Latch latch
  trackSubscriber latch.subscribers
  pure value

-- TODO move to combinators
stepper :: forall m a. MonadBuild m => a -> EventRep a -> m (BehaviourRep a)
stepper a e = do
  latch <- newLatch (Identity a) e
  pure $ coerce $ latchBehaviour latch
