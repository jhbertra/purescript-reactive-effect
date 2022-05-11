module Effect.Reactive.Internal.Build where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (for_)
import Data.OrderedBag (inOrder)
import Data.OrderedBag as OB
import Data.Queue.Existential as EQ
import Data.Queue.Priority as PQ
import Data.WeakBag as WeakBag
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Reactive.Internal
  ( BuildM(..)
  , Clear(..)
  , FireTriggers
  , InvokeTrigger(..)
  , Latch(..)
  , LatchUpdate(..)
  , PropagateEnv
  , PropagateM(..)
  , Reaction(..)
  , Reactor(..)
  , currentDepth
  , propagations
  , updateDepth
  , writeNowClearLater
  )
import Effect.Reader (ReaderEffect(..), runReaderEffect)
import Effect.Ref as Ref
import Effect.Ref.Maybe as RM
import Effect.Unlift (askUnliftEffect, unliftEffect)

class MonadEffect m <= MonadBuild m where
  initializeLatch :: forall a. Latch a -> m Unit
  addReactor :: forall a. Reactor a -> m Unit

instance MonadBuild BuildM where
  initializeLatch latch = BM $ RE \env -> do
    EQ.enqueue latch env.newLatches
  addReactor r = BM $ RE \env -> do
    void $ OB.insert (mkExists r) env.reactors
    EQ.enqueue r env.newReactors

instance MonadBuild PropagateM where
  initializeLatch latch = PM $ RE \env -> EQ.enqueue latch env.newLatches
  addReactor r = PM $ RE \env -> do
    void $ OB.insert (mkExists r) env.reactors
    EQ.enqueue r env.newReactors

runBuildM :: forall a. FireTriggers -> BuildM a -> Effect a
runBuildM fireTriggers (BM m) = do
  newLatches <- EQ.new
  newReactors <- EQ.new
  reactors <- OB.new
  runReaderEffect m { fireTriggers, newLatches, newReactors, reactors }

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
  reactions <- EQ.new
  latchUpdates <- EQ.new
  currentDepthRef <- Ref.new $ -1
  propagationsQ <- PQ.new
  let
    propagateEnv :: PropagateEnv
    propagateEnv =
      { fireTriggers: buildEnv.fireTriggers
      , reactors: buildEnv.reactors
      , newLatches: buildEnv.newLatches
      , newReactors: buildEnv.newReactors
      , clears
      , reactions
      , latchUpdates
      , currentDepth: currentDepthRef
      , propagations: propagationsQ
      }
  -- Propagate events throughout network
  result <- runPropagateM propagateEnv do
    u <- askUnliftEffect
    result <- frame
    -- Initialize latches (assume there won't be any more propagations).
    liftEffect $ EQ.drain buildEnv.newLatches \(Latch { initialize }) ->
      unliftEffect u initialize
    -- Initialize reactors (assume there won't be any more propagations).
    liftEffect $ EQ.drain buildEnv.newReactors \(Reactor { initialize }) ->
      unliftEffect u initialize
    pure result
  -- Clear allocated refs
  EQ.drain clears case _ of
    FlagClear ref -> Ref.write false ref
    MaybeRefClear ref -> RM.clear ref
  -- Update latches
  EQ.drain latchUpdates case _ of
    LatchUpdate { valueRef, invalidateOld, newValue } -> do
      Ref.write newValue valueRef
      invalidateOld
  -- Run raised reactions
  reactionsArray <- EQ.toArray reactions
  orderedReactions <-
    inOrder buildEnv.reactors
      (runExists \(Reaction { reactor }) -> mkExists reactor)
      reactionsArray
  for_ orderedReactions $ runExists case _ of
    Reaction { reactor: Reactor { connection }, value } -> do
      mConnection <- RM.read connection
      for_ mConnection \{ fire } -> fire value
  pure result

runPropagateM :: forall a. PropagateEnv -> PropagateM a -> Effect a
runPropagateM env (PM m) = runReaderEffect m env
