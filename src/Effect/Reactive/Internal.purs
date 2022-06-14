module Effect.Reactive.Internal where

import Prelude

import Concurrent.Queue (Queue)
import Control.Lazy (class Lazy)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Fix (class MonadFix)
import Control.Monad.Reader (class MonadAsk, class MonadReader, asks, local)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Unlift (class MonadUnlift)
import Data.Exists (Exists, runExists)
import Data.Foldable (for_, traverse_)
import Data.Int (toNumber)
import Data.Lazy (force)
import Data.Lazy as DL
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype)
import Data.OrderedBag (OrderedBag)
import Data.Queue.Existential (ExistentialQueue)
import Data.Queue.Existential as EQ
import Data.Queue.Priority (PriorityQueue)
import Data.Queue.Priority as PQ
import Data.Traversable (traverse)
import Data.WeakBag (WeakBag, WeakBagTicket)
import Data.WeakBag as WeakBag
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.RW (RWEffect(..), evalRWEffect)
import Effect.Reader (ReaderEffect(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Ref.Maybe (MaybeRef)
import Effect.Ref.Maybe as RM
import Effect.Unlift (class MonadUnliftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Safe.Coerce (coerce)
import Test.Data.Observe (class Observable)

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

type EventRep a = EventSubscriber a -> PropagateM (EventResult a)

type EventSubscriber a =
  { propagate :: a -> PropagateM Unit
  , recalculateDepth :: Int -> Effect Unit
  }

terminalSubscriber :: forall a. (a -> PropagateM Unit) -> EventSubscriber a
terminalSubscriber f =
  { propagate: f
  , recalculateDepth: mempty
  }

type EventResult a =
  { occurrence :: Maybe a
  , subscription :: EventSubscription
  }

type EventSubscription =
  { unsubscribe :: Effect Unit
  , depth :: Ref Int
  }

type EventHandle a =
  { subscription :: EventSubscription
  , currentValue :: MaybeRef a
  }

invalidDepth :: Int
invalidDepth = -2

-------------------------------------------------------------------------------
-- Time
-------------------------------------------------------------------------------

newtype Time = Time Number

derive instance Eq Time
derive instance Ord Time
instance Show Time where
  show (Time t) = "(Time " <> show t <> ")"

instance Observable Unit Time Time where
  observe _ = identity

subTime :: Time -> Time -> Time
subTime (Time t0) (Time t1) = Time $ t0 - t1

addTime :: Time -> Time -> Time
addTime (Time t0) (Time t1) = Time $ t0 + t1

zeroTime :: Time
zeroTime = Time 0.0

timeFromInt :: Int -> Time
timeFromInt = Time <<< toNumber

-------------------------------------------------------------------------------
-- Behaviours
-------------------------------------------------------------------------------

type PullM a = RWEffect PullEnv PullW a

type PullEnv =
  { time :: Time
  , subscriber :: PullSubscriber
  }

type PullCanceller = Effect Unit

type PullW =
  { canceller :: PullCanceller
  , isContinuous :: Disj Boolean
  }

type BehaviourRep a = PullM a

data PullSubscriber
  = AnonymousSubscriber
  | PipeSubscriber (Exists Pipe)
  | SwitchSubscriber (DL.Lazy (Exists SwitchCache))
  | AnimationSubscriber (DL.Lazy Animation)

trackSubscriber :: WeakBag PullSubscriber -> PullM Unit
trackSubscriber weakBag = RWE \env subscription -> case env.subscriber of
  AnonymousSubscriber -> pure unit
  _ -> do
    ticket <- WeakBag.insert env.subscriber weakBag
    Ref.modify_
      (_ <> { canceller: WeakBag.destroyTicket ticket, isContinuous: mempty })
      subscription

newtype Latch a = Latch
  { value :: Ref a
  , subscribers :: WeakBag PullSubscriber
  , initialize :: PropagateM Unit
  }

newtype LatchUpdate a = LatchUpdate
  { valueRef :: Ref a
  , invalidateOld :: ExistentialQueue SwitchCache -> Effect Unit
  , newValue :: a
  }

newtype Pipe a = Pipe
  { cache :: MaybeRef (PipeCache a)
  , evaluate :: PullM a
  }

type PipeCache a =
  { getValue :: PullM a
  , subscribers :: WeakBag PullSubscriber
  }

invalidateBehaviourSubscriber
  :: ExistentialQueue SwitchCache
  -> PullSubscriber
  -> Effect Unit
invalidateBehaviourSubscriber switchesInvalidated = case _ of
  AnonymousSubscriber -> pure unit
  PipeSubscriber pipe -> runExists (invalidatePipe switchesInvalidated) pipe
  SwitchSubscriber lswitch ->
    runExists (\switch -> EQ.enqueue switch switchesInvalidated) $ force lswitch
  AnimationSubscriber animation -> (force animation).invalidate

invalidatePipe
  :: forall a. ExistentialQueue SwitchCache -> Pipe a -> Effect Unit
invalidatePipe switchesInvalidated (Pipe { cache }) = do
  mCache <- RM.read cache
  case mCache of
    Just { subscribers } -> do
      RM.clear cache
      WeakBag.traverseMembers_
        (invalidateBehaviourSubscriber switchesInvalidated)
        subscribers
    _ -> pure unit

readBehaviourUntracked :: forall a. BehaviourRep a -> Time -> Effect a
readBehaviourUntracked b time =
  evalRWEffect b { time, subscriber: AnonymousSubscriber }

_time :: BehaviourRep Time
_time = RWE \env w -> do
  Ref.modify_ _ { isContinuous = Disj true } w
  pure env.time

_sample :: forall a. BehaviourRep a -> PullM a
_sample = identity

-------------------------------------------------------------------------------
-- Inputs
-------------------------------------------------------------------------------

type Input a =
  { cache :: MaybeRef (InputCache a)
  , occurrence :: MaybeRef a
  , initialize :: InitializeInput a
  }

type InitializeInput a = InputTrigger a -> Effect (Effect Unit)

newtype InputCache a = InputCache
  { subscribers :: WeakBag (EventSubscriber a)
  , input :: Input a
  , finalize :: Effect Unit
  }

type InputTrigger a =
  { subscribers :: WeakBag (EventSubscriber a)
  , occurrence :: MaybeRef a
  }

newtype TriggerInvocation a = TriggerInvocation
  { value :: a
  , trigger :: InputTrigger a
  }

newtype FireTriggers = FireTriggers
  (Time -> Array (Exists TriggerInvocation) -> Effect ~> Effect)

-------------------------------------------------------------------------------
-- Animations
-------------------------------------------------------------------------------

type Animation =
  { dispose :: Effect Unit
  , invalidate :: Effect Unit
  }

-------------------------------------------------------------------------------
-- Graph nodes
-------------------------------------------------------------------------------

type CacheResult cache a =
  { ticket :: WeakBagTicket (EventSubscriber a)
  , cache :: cache
  , occurrence :: Maybe a
  }

type Switch a =
  { parent :: BehaviourRep (EventRep a)
  , cache :: MaybeRef (SwitchCache a)
  }

newtype SwitchCache a = SwitchCache
  { occurrence :: MaybeRef a
  , depth :: Ref Int
  , subscribers :: WeakBag (EventSubscriber a)
  , invalidator :: Ref PullCanceller
  , currentParent :: Ref EventSubscription
  , parent :: BehaviourRep (EventRep a)
  }

type MapFan k v =
  { parent :: EventRep (Map k v)
  , cache :: MaybeRef (MapFanCache k v)
  }

newtype MapFanCache k v = MapFanCache
  { occurrence :: MaybeRef (Map k v)
  , subscribers :: Ref (Map k (WeakBag (EventSubscriber v)))
  , subscription :: EventSubscription
  }

type Join a =
  { parent :: EventRep (EventRep a)
  , cache :: MaybeRef (JoinCache a)
  }

newtype JoinCache a = JoinCache
  { occurrence :: MaybeRef a
  , depth :: Ref Int
  , subscribers :: WeakBag (EventSubscriber a)
  , outerSubscriber :: EventSubscriber (EventRep a)
  , outerSubscription :: EventSubscription
  , innerSubscription :: MaybeRef EventSubscription
  }

newtype JoinReset a = JoinReset
  { subscription :: EventSubscription
  , cache :: Maybe (DL.Lazy (JoinCache a))
  }

newtype PerformParent a r = PerformParent
  { setup :: Effect r
  , teardown :: r -> Effect Unit
  , event :: EventRep (r -> (a -> Effect Unit) -> Effect (Effect Unit))
  }

newtype Perform a = Perform
  { parent :: Exists (PerformParent a)
  , responseTriggerRef :: MaybeRef (InputTrigger a)
  }

newtype RunPerform a = RunPerform
  { perform :: Perform a
  , register :: (a -> Effect Unit) -> Effect (Effect Unit)
  , handleCancel :: Effect Unit -> Effect Unit
  }

wrapSubscribeCached
  :: forall cache a m
   . MonadEffect m
  => (cache -> Ref Int)
  -> (EventSubscriber a -> m (CacheResult cache a))
  -> EventSubscriber a
  -> m (EventResult a)
wrapSubscribeCached depthRef subscribeCached subscriber = do
  { ticket, cache, occurrence } <- subscribeCached subscriber
  pure
    { occurrence
    , subscription:
        { depth: depthRef cache
        , unsubscribe: WeakBag.destroyTicket ticket
        }
    }

zeroDepth :: Ref Int
zeroDepth = unsafePerformEffect $ Ref.new 0

-------------------------------------------------------------------------------
-- Builder
-------------------------------------------------------------------------------

newtype BuildM a = BM (ReaderEffect BuildEnv a)

type FireParams =
  { triggers :: Array (Exists TriggerInvocation)
  , onComplete :: Effect Unit
  }

type BuildEnv' r =
  { triggerQueue :: Queue FireParams
  , performs :: OrderedBag (Exists Perform)
  , newLatches :: ExistentialQueue Latch
  , time :: Time
  , setupQueue :: ExistentialQueue PropagateM
  , cleanup :: Ref (Effect Unit)
  | r
  }

type BuildEnv = BuildEnv' ()

derive newtype instance Functor BuildM
derive newtype instance Apply BuildM
derive newtype instance Applicative BuildM
derive newtype instance Bind BuildM
derive newtype instance Monad BuildM
derive newtype instance MonadEffect BuildM
derive newtype instance MonadUnliftEffect BuildM
derive newtype instance MonadBase Effect BuildM
derive newtype instance MonadUnlift Effect BuildM
derive newtype instance MonadFix BuildM
derive newtype instance MonadAsk BuildEnv BuildM
derive newtype instance MonadReader BuildEnv BuildM
derive newtype instance Semigroup a => Semigroup (BuildM a)
derive newtype instance Monoid a => Monoid (BuildM a)
derive newtype instance Lazy (BuildM a)

-------------------------------------------------------------------------------
-- Propagate
-------------------------------------------------------------------------------

newtype PropagateM a = PM (ReaderEffect PropagateEnv a)

type Propagate =
  Int -- propagation height
  -> PropagateM Unit -- propagation evaluation
  -> Effect Unit

type PropagateEnv = BuildEnv'
  ( clears :: ExistentialQueue Clear
  , currentDepth :: Ref Int
  , propagations :: PriorityQueue (PropagateM Unit)
  , runPerforms :: ExistentialQueue RunPerform
  , latchUpdates :: ExistentialQueue LatchUpdate
  , joinResets :: ExistentialQueue JoinReset
  )

data Clear a
  = MaybeRefClear (MaybeRef a)
  | FlagClear (Ref Boolean)

derive instance Newtype (PropagateM a) _
derive newtype instance Functor PropagateM
derive newtype instance Apply PropagateM
derive newtype instance Applicative PropagateM
derive newtype instance Bind PropagateM
derive newtype instance Monad PropagateM
derive newtype instance MonadEffect PropagateM
derive newtype instance MonadUnliftEffect PropagateM
derive newtype instance MonadBase Effect PropagateM
derive newtype instance MonadUnlift Effect PropagateM
derive newtype instance MonadFix PropagateM
derive newtype instance MonadAsk PropagateEnv PropagateM
derive newtype instance MonadReader PropagateEnv PropagateM
derive newtype instance MonadRec PropagateM
derive newtype instance Semigroup a => Semigroup (PropagateM a)
derive newtype instance Monoid a => Monoid (PropagateM a)

currentDepth :: PropagateM Int
currentDepth = PM $ RE \env -> Ref.read env.currentDepth

updateDepth :: Int -> PropagateM Unit
updateDepth newDepth =
  PM $ RE \env -> Ref.modify_ (max newDepth) env.currentDepth

updateLatch :: forall a. LatchUpdate a -> PropagateM Unit
updateLatch latchUpdate =
  PM $ RE \env -> EQ.enqueue latchUpdate env.latchUpdates

resetJoin
  :: forall a
   . EventSubscription
  -> Maybe (DL.Lazy (JoinCache a))
  -> PropagateM Unit
resetJoin subscription cache = PM $ RE \env ->
  EQ.enqueue (JoinReset { subscription, cache }) env.joinResets

clearLater :: forall a. Clear a -> PropagateM Unit
clearLater clear = PM $ RE \env -> EQ.enqueue clear env.clears

runPerform :: forall a. RunPerform a -> PropagateM Unit
runPerform reaction = PM $ RE \env -> EQ.enqueue reaction env.runPerforms

writeNowClearLater :: forall a. a -> MaybeRef a -> PropagateM Unit
writeNowClearLater a ref = do
  liftEffect $ RM.write a ref
  clearLater $ MaybeRefClear ref

raiseNowClearLater :: Ref Boolean -> PropagateM Unit
raiseNowClearLater flag = do
  liftEffect $ Ref.write true flag
  clearLater $ FlagClear flag

propagate :: Int -> PropagateM Unit -> PropagateM Unit
propagate depth evaluate =
  PM $ RE \env -> do
    current <- Ref.read env.currentDepth
    when (depth >= current) do
      if (depth == current) then
        coerce evaluate env
      else
        PQ.enqueue depth evaluate env.propagations

propagations :: PropagateM (PriorityQueue (PropagateM Unit))
propagations = PM $ RE \env -> pure env.propagations

askTime :: PropagateM Time
askTime = asks _.time

-------------------------------------------------------------------------------
-- Combinators - EventRep
-------------------------------------------------------------------------------

_subscribe
  :: forall a. EventRep a -> EventSubscriber a -> PropagateM (EventResult a)
_subscribe = identity

getEventHandle :: forall a. EventRep a -> PropagateM (EventHandle a)
getEventHandle event = do
  currentValue <- liftEffect RM.empty
  { occurrence, subscription } <- _subscribe event $ terminalSubscriber \a ->
    writeNowClearLater a currentValue
  for_ occurrence \a -> writeNowClearLater a currentValue
  pure { currentValue, subscription }

_pushRaw :: forall a b. (a -> PropagateM (Maybe b)) -> EventRep a -> EventRep b
_pushRaw f e1 sub = do
  previousCleanup <- liftEffect $ Ref.new mempty
  let
    f' a = do
      cleanupRef <- liftEffect do
        join $ Ref.read previousCleanup
        Ref.new mempty
      mb <- local _ { cleanup = cleanupRef } $ f a
      liftEffect do
        cleanup <- Ref.read cleanupRef
        Ref.write cleanup previousCleanup
      pure mb
  result <- _subscribe e1 sub
    { propagate = traverse_ sub.propagate <=< f'
    }
  occurrence <- traverse f' result.occurrence
  pure
    { occurrence: join occurrence
    , subscription: result.subscription
        { unsubscribe = do
            result.subscription.unsubscribe
            join $ Ref.read previousCleanup
        }
    }

_neverE :: forall a. EventRep a
_neverE = const $ pure $
  { occurrence: Nothing
  , subscription: { depth: zeroDepth, unsubscribe: mempty }
  }

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

unlessRaised
  :: forall m a. MonadEffect m => Monoid a => Ref Boolean -> m a -> m a
unlessRaised flag m = do
  b <- liftEffect $ Ref.read flag
  if b then pure mempty else m
