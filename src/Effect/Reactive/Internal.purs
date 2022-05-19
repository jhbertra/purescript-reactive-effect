module Effect.Reactive.Internal where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Fix (class MonadFix)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Monad.Writer (tell)
import Data.Exists (Exists, runExists)
import Data.Foldable (for_, traverse_)
import Data.Lazy (force)
import Data.Lazy as DL
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.OrderedBag (OrderedBag)
import Data.Queue.Existential (ExistentialQueue)
import Data.Queue.Existential as EQ
import Data.Queue.Priority (PriorityQueue)
import Data.Queue.Priority as PQ
import Data.Time.Duration (Milliseconds)
import Data.Traversable (traverse)
import Data.WeakBag (WeakBag, WeakBagTicket)
import Data.WeakBag as WeakBag
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.RW (RWEffect(..))
import Effect.Reader (ReaderEffect(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Ref.Maybe (MaybeRef)
import Effect.Ref.Maybe as RM
import Effect.Unlift (class MonadUnliftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Safe.Coerce (coerce)

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
-- Behaviours
-------------------------------------------------------------------------------

type BehaviourRep a = RWEffect BehaviourEnv BehaviourSubscription a

type BehaviourEnv = Maybe SomeBehaviourSubscriber

data BehaviourSubscriber a
  = PipeSubscriber (Pipe a)
  | SwitchSubscriber (DL.Lazy (SwitchCache a))
  | AnimationSubscriber Int (DL.Lazy AnimationInitialized)

data SampleHint
  = SampleOnce
  | SampleContinuous

instance Semigroup SampleHint where
  append SampleContinuous _ = SampleContinuous
  append _ b = b

instance Monoid SampleHint where
  mempty = SampleOnce

type SomeBehaviourSubscriber = Exists BehaviourSubscriber
data BehaviourSubscription
  = Inactive
  | Active
      { hint :: SampleHint
      , unsubscribe :: Effect Unit
      }

instance Semigroup BehaviourSubscription where
  append Inactive b = b
  append a Inactive = a
  append (Active a) (Active b) = Active
    { hint: a.hint <> b.hint
    , unsubscribe: a.unsubscribe *> b.unsubscribe
    }

instance Monoid BehaviourSubscription where
  mempty = Inactive

tellHint :: SampleHint -> BehaviourRep Unit
tellHint hint = tell $ Active { hint, unsubscribe: pure unit }

trackSubscriber :: WeakBag SomeBehaviourSubscriber -> BehaviourRep Unit
trackSubscriber weakBag = RWE \mSubscriber subscription ->
  for_ mSubscriber \subscriber -> do
    ticket <- WeakBag.insert subscriber weakBag
    Ref.modify_
      ( _ <> Active
          { hint: SampleOnce
          , unsubscribe: WeakBag.destroyTicket ticket
          }
      )
      subscription

newtype Latch a = Latch
  { value :: Ref a
  , subscribers :: WeakBag SomeBehaviourSubscriber
  , initialize :: PropagateM Unit
  }

newtype LatchUpdate a = LatchUpdate
  { valueRef :: Ref a
  , invalidateOld :: ExistentialQueue SwitchCache -> Effect Unit
  , newValue :: a
  }

type Pipe a =
  { cache :: MaybeRef (PipeCache a)
  , evaluate :: BehaviourRep a
  }

type PipeCache a =
  { getValue :: Effect a
  , subscribers :: WeakBag SomeBehaviourSubscriber
  , subscription :: BehaviourSubscription
  }

invalidateBehaviourSubscriber
  :: forall a
   . ExistentialQueue SwitchCache
  -> BehaviourSubscriber a
  -> Effect Unit
invalidateBehaviourSubscriber switchesInvalidated = case _ of
  PipeSubscriber pipe -> invalidatePipe switchesInvalidated pipe
  SwitchSubscriber switch -> EQ.enqueue (force switch) switchesInvalidated
  AnimationSubscriber _ animation -> (force animation).invalidate

invalidatePipe
  :: forall a. ExistentialQueue SwitchCache -> Pipe a -> Effect Unit
invalidatePipe switchesInvalidated pipe = do
  mCache <- RM.read pipe.cache
  case mCache of
    Just { subscribers } -> do
      RM.clear pipe.cache
      WeakBag.traverseMembers_
        (runExists (invalidateBehaviourSubscriber switchesInvalidated))
        subscribers
    _ -> pure unit

readBehaviourUntracked :: BehaviourRep ~> Effect
readBehaviourUntracked (RWE b) = do
  w <- Ref.new mempty
  b Nothing w

_time :: BuildM (BehaviourRep Milliseconds)
_time = BM $ RE \env -> pure do
  tellHint SampleContinuous
  liftEffect $ env.getTime

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

newtype InvokeTrigger a = InvokeTrigger
  { value :: a
  , trigger :: InputTrigger a
  }

newtype FireTriggers = FireTriggers
  (Array (Exists InvokeTrigger) -> Effect Unit -> Aff Unit)

-------------------------------------------------------------------------------
-- Animations
-------------------------------------------------------------------------------

type AnimationInitialized =
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
  , subscription :: Ref BehaviourSubscription
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
-- RaffFrame
-------------------------------------------------------------------------------

type ReactionConnection a =
  { dispose :: Effect Unit
  , fire :: a -> Effect Unit
  }

type InitializeReaction a = Maybe a -> Effect (ReactionConnection a)

newtype Reactor a = Reactor
  { connection :: MaybeRef (ReactionConnection a)
  , subscription :: MaybeRef EventSubscription
  , initialize :: PropagateM Unit
  }

newtype Reaction a = Reaction
  { reactor :: Reactor a
  , value :: a
  }

-------------------------------------------------------------------------------
-- Builder
-------------------------------------------------------------------------------

newtype BuildM a = BM (ReaderEffect BuildEnv a)

type BuildEnv' r =
  { fireTriggers :: FireTriggers
  , reactors :: OrderedBag (Exists Reactor)
  , newLatches :: ExistentialQueue Latch
  , newReactors :: ExistentialQueue Reactor
  , getTime :: Effect Milliseconds
  , getPostBuild :: Effect (EventRep Unit)
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
  , reactions :: ExistentialQueue Reaction
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

raiseReaction :: forall a. Reaction a -> PropagateM Unit
raiseReaction reaction = PM $ RE \env -> EQ.enqueue reaction env.reactions

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
  result <- _subscribe e1 sub
    { propagate = traverse_ sub.propagate <=< f
    }
  occurrence <- traverse f result.occurrence
  pure result { occurrence = join occurrence }

_neverE :: forall a. EventRep a
_neverE = const $ pure $
  { occurrence: Nothing
  , subscription: { depth: zeroDepth, unsubscribe: mempty }
  }

_postBuild :: BuildM (EventRep Unit)
_postBuild = BM $ RE \env -> env.getPostBuild

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

unlessRaised
  :: forall m a. MonadEffect m => Monoid a => Ref Boolean -> m a -> m a
unlessRaised flag m = do
  b <- liftEffect $ Ref.read flag
  if b then pure mempty else m
