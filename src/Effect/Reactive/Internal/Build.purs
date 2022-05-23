module Effect.Reactive.Internal.Build where

import Prelude

import Concurrent.Queue (Queue)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (for_, traverse_)
import Data.Lazy (force)
import Data.Maybe (Maybe(..))
import Data.OrderedBag (inOrder)
import Data.OrderedBag as OB
import Data.Queue.Existential as EQ
import Data.Queue.Priority as PQ
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Data.WeakBag as WeakBag
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.RW (runRWEffect)
import Effect.Reactive.Internal
  ( BehaviourSubscriber(..)
  , BehaviourSubscription(..)
  , BuildM(..)
  , Clear(..)
  , EventRep
  , FireParams
  , FireTriggers(..)
  , InvokeTrigger(..)
  , JoinReset(..)
  , Latch(..)
  , LatchUpdate(..)
  , Perform(..)
  , PropagateEnv
  , PropagateM(..)
  , RunPerform(..)
  , SwitchCache(..)
  , _subscribe
  , currentDepth
  , groundEvent
  , newGround
  , propagations
  , updateDepth
  , writeNowClearLater
  )
import Effect.Reactive.Internal.Input (inputEvent, newInputWithTriggerRef)
import Effect.Reactive.Internal.Join (recalculateJoinDepth)
import Effect.Reactive.Internal.Switch (switchSubscriber)
import Effect.Reader (ReaderEffect(..), runReaderEffect)
import Effect.Ref as Ref
import Effect.Ref.Maybe as RM
import Effect.Unlift (askUnliftEffect, unliftEffect)
import Safe.Coerce (coerce)

initializeLatch :: forall m a. MonadBuild m => Latch a -> m Unit
initializeLatch latch = liftBuild $ BM $ RE \env ->
  EQ.enqueue latch env.newLatches

addPerform :: forall m a. MonadBuild m => Perform a -> m Unit
addPerform perform = liftBuild $ BM $ RE \env -> do
  void $ OB.insert (mkExists perform) env.performs

groundLater :: forall m a. MonadBuild m => EventRep a -> m Unit
groundLater event = liftBuild $ BM $ RE \env -> do
  EQ.enqueue (groundEvent event) env.groundQueue

class MonadEffect m <= MonadBuild m where
  liftBuild :: BuildM ~> m

instance MonadBuild BuildM where
  liftBuild = identity

instance MonadBuild PropagateM where
  liftBuild (BM m) =
    PM $ RE
      \{ triggerQueue
       , newLatches
       , performs
       , getTime
       , asap
       , ground
       , groundQueue
       } ->
        runReaderEffect
          m
          { triggerQueue
          , newLatches
          , performs
          , getTime
          , asap
          , ground
          , groundQueue
          }

runBuildM
  :: forall a
   . Queue FireParams
  -> Effect Milliseconds
  -> BuildM a
  -> Effect { fire :: FireTriggers, result :: a, dispose :: Effect Unit }
runBuildM triggerQueue getTime (BM m) = do
  newLatches <- EQ.new
  groundQueue <- EQ.new
  performs <- OB.new
  asapIO <- newInputWithTriggerRef
  { ground, dispose } <- newGround
  let asap = inputEvent asapIO.input
  let
    env =
      { triggerQueue, newLatches, performs, groundQueue, getTime, asap, ground }
  let
    fire@(FireTriggers fire') = FireTriggers \triggers read ->
      runReaderEffect (coerce $ fireAndRead triggers read) env
  result <- runReaderEffect
    ( do
        result <- m
        let BM mGround = runFrame (pure unit)
        mGround
        pure result
    )
    env
  mAsapTrigger <- RM.read asapIO.trigger
  for_ mAsapTrigger \trigger ->
    fire' [ mkExists $ InvokeTrigger { trigger, value: unit } ] $ pure unit
  pure { result, fire, dispose }

fireAndRead :: forall a. Array (Exists InvokeTrigger) -> Effect a -> BuildM a
fireAndRead triggers read = runFrame do
  u <- askUnliftEffect
  -- propagate inputs
  for_ triggers $ runExists \(InvokeTrigger { trigger, value }) -> do
    unlessM (liftEffect $ RM.isFilled trigger.occurrence) do
      writeNowClearLater value trigger.occurrence
      flip WeakBag.traverseMembers_ trigger.subscribers \subscriber ->
        subscriber.propagate value
  -- drain propagations
  propagationsQ <- propagations
  liftEffect $ PQ.drain propagationsQ \depth evaluate -> unliftEffect u do
    cd <- currentDepth
    updateDepth depth
    unless (depth < cd) evaluate
  updateDepth top
  liftEffect read

runFrame :: forall a. PropagateM a -> BuildM a
runFrame frame = BM $ RE \buildEnv -> do
  -- Declare frame variables
  clears <- EQ.new
  runPerforms <- EQ.new
  latchUpdates <- EQ.new
  switchesInvalidated <- EQ.new
  joinResets <- EQ.new
  currentDepthRef <- Ref.new $ -1
  propagationsQ <- PQ.new
  asapIO <- newInputWithTriggerRef
  let asap = inputEvent asapIO.input
  let
    propagateEnv :: PropagateEnv
    propagateEnv =
      { triggerQueue: buildEnv.triggerQueue
      , performs: buildEnv.performs
      , newLatches: buildEnv.newLatches
      , getTime: buildEnv.getTime
      , ground: buildEnv.ground
      , groundQueue: buildEnv.groundQueue
      , asap
      , clears
      , runPerforms
      , latchUpdates
      , joinResets
      , currentDepth: currentDepthRef
      , propagations: propagationsQ
      }
  -- Propagate events throughout network
  result <- runPropagateM propagateEnv do
    u <- askUnliftEffect
    result <- frame
    -- At this point we are done propagating in this frame. Even if we run
    -- actions that call propagate, we just ignore them, because it's unsafe to
    -- further propagate events after updating latch values.
    -- Initialize latches (assume there won't be any more propagations).
    liftEffect $ EQ.drain buildEnv.newLatches \(Latch { initialize }) ->
      unliftEffect u initialize
    -- Run deferred grounds
    liftEffect $ EQ.drain buildEnv.groundQueue \ground ->
      void $ unliftEffect u ground
    pure result
  -- Clear allocated refs
  EQ.drain clears case _ of
    FlagClear ref -> Ref.write false ref
    MaybeRefClear ref -> RM.clear ref
  -- Update latches
  EQ.drain latchUpdates case _ of
    LatchUpdate { valueRef, invalidateOld, newValue } -> do
      Ref.write newValue valueRef
      invalidateOld switchesInvalidated
  -- Reconnect invalidated switches (assume there won't be any more propagations).
  EQ.drain switchesInvalidated \(SwitchCache cache) -> do
    -- switch off old parent
    oldParent <- Ref.read cache.currentParent
    oldSubscription <- Ref.read cache.subscription
    case oldSubscription of
      Active { unsubscribe } -> unsubscribe
      _ -> pure unit
    oldParent.unsubscribe
    -- pull new parent
    Tuple pullSubscription e <- runRWEffect cache.parent
      $ Just
      $ mkExists
      $ SwitchSubscriber
      $ pure
      $ SwitchCache cache
    Ref.write pullSubscription cache.subscription
    -- switch to new parent
    let subscriber = switchSubscriber $ pure $ SwitchCache cache
    { subscription } <- runPropagateM propagateEnv $ _subscribe e $ subscriber
    newDepth <- Ref.read subscription.depth
    subscriber.recalculateDepth newDepth
    Ref.write subscription cache.currentParent
  -- Reset joins
  EQ.drain joinResets \(JoinReset { cache, subscription }) -> do
    subscription.unsubscribe
    traverse_ (recalculateJoinDepth <<< force) cache
  -- Perform effects
  runPerformsArray <- EQ.toArray runPerforms
  orderedPerforms <-
    inOrder buildEnv.performs
      (runExists \(RunPerform { perform }) -> mkExists perform)
      runPerformsArray
  for_ orderedPerforms $ runExists case _ of
    RunPerform
      { perform: Perform { responseTriggerRef }
      , register
      , handleCancel
      } -> do
      cancel <- register \value -> do
        mTrigger <- RM.read responseTriggerRef
        for_ mTrigger \trigger -> do
          let triggers = [ mkExists $ InvokeTrigger { trigger, value } ]
          runReaderEffect
            (let (BM m) = fireAndRead triggers $ pure unit in m)
            buildEnv
      handleCancel cancel

  -- fire ASAP
  mAsapTrigger <- RM.read asapIO.trigger
  for_ mAsapTrigger \trigger -> do
    let triggers = [ mkExists $ InvokeTrigger { trigger, value: unit } ]
    runReaderEffect
      (let (BM m) = fireAndRead triggers $ pure unit in m)
      buildEnv
  pure result

runPropagateM :: forall a. PropagateEnv -> PropagateM a -> Effect a
runPropagateM env (PM m) = runReaderEffect m env
