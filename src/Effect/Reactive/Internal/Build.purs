module Effect.Reactive.Internal.Build where

import Prelude

import Concurrent.Queue (Queue)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (for_, traverse_)
import Data.Lazy (force)
import Data.OrderedBag (inOrder)
import Data.OrderedBag as OB
import Data.Queue.Existential as EQ
import Data.Queue.Priority as PQ
import Data.Tuple (Tuple(..))
import Data.WeakBag as WeakBag
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.RW (runRWEffect)
import Effect.Reactive.Internal
  ( BuildM(..)
  , Clear(..)
  , FireParams
  , FireTriggers(..)
  , JoinReset(..)
  , Latch(..)
  , LatchUpdate(..)
  , Perform(..)
  , PropagateEnv
  , PropagateM(..)
  , PullSubscriber(..)
  , RunPerform(..)
  , SwitchCache(..)
  , Time
  , TriggerInvocation(..)
  , _subscribe
  , currentDepth
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

_onReady :: forall m. MonadBuild m => PropagateM Unit -> m Unit
_onReady task = liftBuild $ BM $ RE \env -> do
  EQ.enqueue task env.setupQueue

_onCleanup :: forall m. MonadBuild m => Effect Unit -> m Unit
_onCleanup task = liftBuild $ BM $ RE \env -> do
  Ref.modify_ (_ *> task) env.cleanup

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
       , time
       , asap
       , setupQueue
       , cleanup
       } ->
        runReaderEffect
          m
          { triggerQueue
          , newLatches
          , performs
          , time
          , asap
          , setupQueue
          , cleanup
          }

runBuildM
  :: forall a
   . Queue FireParams
  -> Time
  -> BuildM a
  -> Effect { fire :: FireTriggers, result :: a, cleanup :: Effect Unit }
runBuildM triggerQueue time (BM m) = do
  newLatches <- EQ.new
  setupQueue <- EQ.new
  performs <- OB.new
  asapIO <- newInputWithTriggerRef
  cleanupRef <- Ref.new mempty
  let asap = inputEvent asapIO.input
  let
    env =
      { triggerQueue
      , newLatches
      , performs
      , asap
      , time
      , setupQueue
      , cleanup: cleanupRef
      }
  let
    fire :: Time -> Array (Exists TriggerInvocation) -> Effect ~> Effect
    fire time' triggers read = do
      runReaderEffect (coerce $ fireAndRead triggers read) env { time = time' }
  result <- runReaderEffect
    ( do
        result <- m
        let BM mGround = runFrame $ pure unit
        mGround
        pure result
    )
    env
  mAsapTrigger <- RM.read asapIO.trigger
  for_ mAsapTrigger \trigger ->
    fire time [ mkExists $ TriggerInvocation { trigger, value: unit } ] $ pure
      unit
  cleanup <- Ref.read cleanupRef
  pure { result, fire: FireTriggers fire, cleanup }

fireAndRead
  :: forall a. Array (Exists TriggerInvocation) -> Effect a -> BuildM a
fireAndRead triggers read = runFrame do
  u <- askUnliftEffect
  -- propagate inputs
  for_ triggers $ runExists \(TriggerInvocation { trigger, value }) -> do
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
      , time: buildEnv.time
      , setupQueue: buildEnv.setupQueue
      , cleanup: buildEnv.cleanup
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
    -- After updating the latches, we can run the setup actions (which may
    -- sample behaviours).
    liftEffect $ EQ.drain buildEnv.setupQueue \task -> void
      (unliftEffect u task)
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
    join $ Ref.read cache.invalidator
    oldParent.unsubscribe
    -- pull new parent
    Tuple { canceller } e <- runRWEffect cache.parent
      $
        { time: buildEnv.time
        , subscriber: SwitchSubscriber
            $ pure
            $ mkExists
            $ SwitchCache cache
        }
    Ref.write canceller cache.invalidator
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
          let triggers = [ mkExists $ TriggerInvocation { trigger, value } ]
          runReaderEffect
            (let (BM m) = fireAndRead triggers $ pure unit in m)
            buildEnv
      handleCancel cancel

  -- fire ASAP
  mAsapTrigger <- RM.read asapIO.trigger
  for_ mAsapTrigger \trigger -> do
    let triggers = [ mkExists $ TriggerInvocation { trigger, value: unit } ]
    runReaderEffect
      (let (BM m) = fireAndRead triggers $ pure unit in m)
      buildEnv
  pure result

runPropagateM :: forall a. PropagateEnv -> PropagateM a -> Effect a
runPropagateM env (PM m) = runReaderEffect m env
