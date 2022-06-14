module Effect.Reactive
  ( class MonadPull
  , class MonadRaff
  , Behaviour
  , Event
  , EventSelector
  , Raff
  , RaffPull
  , RaffPush
  , Timeline
  , (<&)
  , (<&>)
  , (~>)
  , accumB
  , accumE
  , accumMB
  , accumME
  , accumMaybeB
  , accumMaybeE
  , accumMaybeMB
  , accumMaybeME
  , alignM
  , alignMaybe
  , alignMaybeM
  , animate
  , animateWithSetup
  , asap
  , delayEvent
  , fanMap
  , filterApply
  , gate
  , indexed
  , indexedFrom
  , indexedFrom_
  , indexed_
  , interpret
  , interpret2
  , intervalEvent
  , launchRaff
  , launchRaff_
  , liftPull
  , liftRaff
  , liftSample2
  , liftSampleM2
  , liftSampleMaybe2
  , liftSampleMaybeM2
  , makeBehaviour
  , makeEvent
  , makeEventAff
  , mapAccum
  , mapAccumM
  , mapAccumM_
  , mapAccumMaybe
  , mapAccumMaybeM
  , mapAccumMaybeM_
  , mapAccumMaybe_
  , mapAccum_
  , newBehaviour
  , newEvent
  , onReady
  , onCleanup
  , partitionApply
  , partitionMapApply
  , patcher
  , perform
  , performAsync
  , performAsyncWithSetup
  , performWithSetup
  , pull
  , push
  , pushAlways
  , pushFlipped
  , pushed
  , sample
  , sampleApply
  , sampleApplyMaybe
  , selectEvent
  , split
  , stepper
  , switch
  , switchE
  , switchEImmediately
  , switcher
  , tag
  , tagMaybe
  , timeB
  , traceEvent
  , traceEventWith
  ) where

import Prelude

import Concurrent.Queue as Queue
import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Plus)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Fix (class MonadFix, mfix, mfix2)
import Control.Monad.Reader (asks)
import Control.Monad.Rec.Class (forever, untilJust)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Plus (empty)
import Data.Align (class Align, class Alignable, align)
import Data.Compactable (class Compactable, separate)
import Data.Either (Either(..), either, hush)
import Data.Exists (mkExists)
import Data.Filterable (class Filterable, eitherBool, filterMap, maybeBool)
import Data.Foldable (for_)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Patch (class Patch)
import Data.These (These(..), these)
import Data.Time.Duration (class Duration, fromDuration)
import Data.Traversable (Accum)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (class DebugWarning, traceM)
import Effect (Effect)
import Effect.Aff (Aff, bracket, delay, killFiber, launchAff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Effect.RW (RWEffect(..), evalRWEffect)
import Effect.Reactive.Internal
  ( BehaviourRep
  , BuildM
  , EventRep
  , PerformParent(..)
  , PropagateM
  , PullM
  , PullSubscriber(..)
  , TriggerInvocation(..)
  , _asap
  , _neverE
  , _pushRaw
  , _sample
  , _time
  ) as Internal
import Effect.Reactive.Internal
  ( FireTriggers(..)
  , Time
  , getEventHandle
  , subTime
  , zeroTime
  )
import Effect.Reactive.Internal.Animation (InitializeAnimation, newAnimation)
import Effect.Reactive.Internal.Build (liftBuild, runBuildM, runFrame)
import Effect.Reactive.Internal.Build as Build
import Effect.Reactive.Internal.Cached (_cached) as Internal
import Effect.Reactive.Internal.Fan (mapFanEvent)
import Effect.Reactive.Internal.Input
  ( inputEvent
  , newInput
  , newInputWithTriggerRef
  )
import Effect.Reactive.Internal.Join (joinEvent)
import Effect.Reactive.Internal.Latch (latchBehaviour, newLatch)
import Effect.Reactive.Internal.Merge (_mergeWithMaybeM) as Internal
import Effect.Reactive.Internal.Perform (_perform) as Internal
import Effect.Reactive.Internal.Pipe (_pull)
import Effect.Reactive.Internal.Switch (switchEvent)
import Effect.Reactive.Internal.Testing (interpret2) as Internal
import Effect.Ref.Maybe as RM
import Effect.Unlift (class MonadUnliftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Safe.Coerce (coerce)
import Web.HTML (Window, window)
import Web.HTML.Window (RequestAnimationFrameId, cancelAnimationFrame)

-------------------------------------------------------------------------------
-- Raff monad
-------------------------------------------------------------------------------

-- | Used to prevent primitives like `Events` and `Behaviours` from escaping
-- | the context that created them (similar to `ST`'s `Region` kind.
foreign import data Timeline :: Type

-- | An effectful computation that can build a reactive network from FRP
-- | primitives.
newtype Raff (t :: Timeline) a = Raff (Internal.BuildM a)

derive newtype instance Functor (Raff t)
derive newtype instance Apply (Raff t)
derive newtype instance Applicative (Raff t)
derive newtype instance Bind (Raff t)
derive newtype instance Monad (Raff t)
derive newtype instance MonadEffect (Raff t)
derive newtype instance MonadUnliftEffect (Raff t)
instance MonadBase (Raff t) (Raff t) where
  liftBase = identity

instance MonadUnlift (Raff t) (Raff t) where
  withRunInBase runAction = runAction identity

derive newtype instance MonadFix (Raff t)
derive newtype instance Semigroup a => Semigroup (Raff t a)
derive newtype instance Monoid a => Monoid (Raff t a)
derive newtype instance Lazy (Raff t a)

class MonadPull t m <= MonadRaff t m | m -> t where
  liftRaff :: Raff t ~> m

instance MonadRaff t (Raff t) where
  liftRaff = identity

instance MonadRaff t (RaffPush t) where
  liftRaff (Raff m) = RaffPush $ liftBuild m

-- | Run a reactive guest application inside an `Aff` host and discard the result.
-- | The guest application returns an `Event` that will terminate the
-- | application when it fires.
launchRaff_ :: forall a. (forall t. Raff t (Event t a)) -> Aff Unit
launchRaff_ m = void $ launchRaff m

foreign import getHighResTimestamp :: Effect Time

-- | Run a reactive guest application inside an `Aff` host. The guest application
-- | returns an `Event` that will terminate the application when it fires. The
-- | occurrence of the event is returned.
launchRaff :: forall a. (forall t. Raff t (Event t a)) -> Aff a
launchRaff (Raff m) = do
  queue <- Queue.new
  bracket
    ( liftEffect $ runBuildM queue zeroTime do
        Event eResult <- m
        runFrame do
          eResultHandle <- getEventHandle eResult
          liftEffect do
            mResult <- RM.read eResultHandle.currentValue
            pure $ eResultHandle /\ mResult
    )
    (liftEffect <<< _.cleanup)
    ( \{ result, fire: FireTriggers fire } -> do
        let eResultHandle /\ mResult = result
        startTime <- liftEffect getHighResTimestamp
        case mResult of
          Just a -> pure a
          Nothing -> untilJust do
            { triggers, onComplete } <- Queue.read queue
            liftEffect do
              now <- getHighResTimestamp
              fire (now `subTime` startTime) triggers do
                onComplete
                RM.read eResultHandle.currentValue
    )

onReady :: forall t m. MonadRaff t m => RaffPush t Unit -> m Unit
onReady (RaffPush task) = liftRaff $ Raff $ Build._onReady task

onCleanup :: forall t m. MonadRaff t m => Effect Unit -> m Unit
onCleanup = liftRaff <<< Raff <<< Build._onCleanup

-------------------------------------------------------------------------------
-- RaffPush monad
-------------------------------------------------------------------------------

-- | A computation that can build reactive networks, accumulate state and sample
-- | behaviours. Used with the `push` combinator to allow dynamic network
-- | construction in reaction to `Event`s.
newtype RaffPush (t :: Timeline) a = RaffPush (Internal.PropagateM a)

type role RaffPush nominal representational

derive newtype instance Functor (RaffPush t)
derive newtype instance Apply (RaffPush t)
derive newtype instance Applicative (RaffPush t)
derive newtype instance Bind (RaffPush t)
derive newtype instance Monad (RaffPush t)
derive newtype instance MonadEffect (RaffPush t)
derive newtype instance MonadUnliftEffect (RaffPush t)
instance MonadBase (RaffPush t) (RaffPush t) where
  liftBase = identity

instance MonadUnlift (RaffPush t) (RaffPush t) where
  withRunInBase runAction = runAction identity

derive newtype instance MonadFix (RaffPush t)
derive newtype instance Semigroup a => Semigroup (RaffPush t a)
derive newtype instance Monoid a => Monoid (RaffPush t a)

-------------------------------------------------------------------------------
-- RaffPull monad
-------------------------------------------------------------------------------

-- | A computation that can sample behaviours. Used with the `pull` combinator
-- | to pull from arbitrary behaviour sources.
newtype RaffPull (t :: Timeline) a = RaffPull (Internal.PullM a)

type role RaffPull nominal representational

derive newtype instance Functor (RaffPull t)
derive newtype instance Apply (RaffPull t)
derive newtype instance Applicative (RaffPull t)
derive newtype instance Bind (RaffPull t)
derive newtype instance Monad (RaffPull t)
derive newtype instance MonadFix (RaffPull t)
derive newtype instance Semigroup a => Semigroup (RaffPull t a)
derive newtype instance Monoid a => Monoid (RaffPull t a)

class MonadFix m <= MonadPull t m | m -> t where
  liftPull :: RaffPull t ~> m

instance MonadPull t (Raff t) where
  liftPull (RaffPull m) = Raff do
    time <- asks _.time
    liftEffect
      $ evalRWEffect m { time, subscriber: Internal.AnonymousSubscriber }

instance MonadPull t (RaffPush t) where
  liftPull = liftRaff <<< liftPull

instance MonadPull t (RaffPull t) where
  liftPull = coerce

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

-- | A discrete occurence at an instantaneous moment in time. An `Event` firing
-- | signals that something is "happening" in an FRP network. `Events` cannot
-- | hold state or be sampled, but they can trigger effects to and be merged (they
-- | support a notion of simultaneous firing which allows one to detect when
-- | two events are firing at the same time).
newtype Event (t :: Timeline) a = Event (Internal.EventRep a)

type role Event nominal representational

-- | A record that contains an event and an action to fire it.
type EventIO (t :: Timeline) a =
  { event :: Event t a, fire :: a -> Effect Unit }

-- | A record that contains a behaviour and a action to update it.
type BehaviourIO (t :: Timeline) a =
  { behaviour :: Behaviour t a, update :: a -> Effect Unit }

-- | A function that efficiently produces an `Event` when given some key `k`.
newtype EventSelector t k v = EventSelector (k -> Event t v)

-- | Efficiently create a new `Event` keyed by some key `k`.
selectEvent :: forall t k v. EventSelector t k v -> k -> Event t v
selectEvent (EventSelector f) = f

instance Functor (Event t) where
  map f = push $ pure <<< Just <<< f

instance Apply (Event t) where
  apply = alignMaybe case _ of
    Both f a -> Just $ f a
    _ -> Nothing

instance Bind (Event t) where
  bind (Event event) k = Event $ unsafePerformEffect do
    cache <- RM.empty
    pure $ joinEvent
      { cache
      , parent: Internal._pushRaw (pure <<< Just <<< coerce k) event
      }

instance Alt (Event t) where
  alt = align $ these identity identity const

instance Plus (Event t) where
  empty = Event Internal._neverE

instance Align (Event t) where
  align f = alignMaybe (Just <<< f)

instance Alignable (Event t) where
  nil = empty

instance Compactable (Event t) where
  compact (Event e) = Event $ Internal._pushRaw pure e
  separate (Event e) =
    { left: Event $ Internal._pushRaw (pure <<< either Just (const Nothing)) e
    , right: Event $ Internal._pushRaw (pure <<< hush) e
    }

instance Filterable (Event t) where
  filter p = push $ pure <<< maybeBool p
  filterMap f = push $ pure <<< f
  partition p e =
    let
      { left, right } = separate $ map (eitherBool p) e
    in
      { no: left, yes: right }
  partitionMap f = separate <<< map f

instance Semigroup a => Semigroup (Event t a) where
  append = align $ these identity identity append

instance Semigroup a => Monoid (Event t a) where
  mempty = empty

derive newtype instance Lazy (Event t a)

-- | An event that will fire as soon as its surrounding scope is finished
-- | building.
asap :: forall t. Raff t (Event t Unit)
asap = coerce Internal._asap

-- | Make an event from a function that receives a trigger callback. In
-- | addition to the value to be fired, the callback also allows an action to
-- | be provided which will be executed when the event has actually been fired,
-- | enabling safe, sequential operation (e.g. waiting for the first value to
-- | fire before trigging another one).
-- |
-- | The action returned from the callback is used to free resources and
-- | cancel effects setup in the callback.
makeEventWithFireCallback
  :: forall t m a
   . MonadRaff t m
  => ((a -> Effect Unit -> Effect Unit) -> Effect (Effect Unit))
  -> m (Event t a)
makeEventWithFireCallback subscribe = liftRaff $ Raff do
  triggerQueue <- asks _.triggerQueue
  input <- liftEffect $ newInput \trigger ->
    subscribe \value onComplete -> launchAff_ do
      Queue.write
        triggerQueue
        { triggers: [ mkExists $ Internal.TriggerInvocation { trigger, value } ]
        , onComplete
        }
  pure $ Event $ inputEvent input

-- | Make an event from a function that receives a trigger callback. The
-- | resulting event can be fired asynchronously by invoking the provided
-- | trigger function.
-- |
-- | The action returned from the callback is used to free resources and
-- | cancel effects setup in the callback.
makeEvent
  :: forall t m a
   . MonadRaff t m
  => ((a -> Effect Unit) -> Effect (Effect Unit))
  -> m (Event t a)
makeEvent subscribe = makeEventWithFireCallback \fire ->
  subscribe \value -> fire value $ pure unit

-- | Create a new event along with a trigger function that can be used to fire
-- | the event imperatively.
newEvent :: forall t m a. MonadRaff t m => m (EventIO t a)
newEvent = liftRaff $ Raff do
  { input, trigger } <- liftEffect newInputWithTriggerRef
  triggerQueue <- asks _.triggerQueue
  pure
    { event: Event $ inputEvent input
    , fire: \value -> do
        mTrigger <- RM.read trigger
        for_ mTrigger \t -> launchAff_ do
          Queue.write
            triggerQueue
            { triggers:
                [ mkExists $ Internal.TriggerInvocation { trigger: t, value } ]
            , onComplete: (pure unit)
            }
    }

-- | Make a behaviour from a setup action that receives an update action. The
-- | resulting behaviour can be updated asynchronously by invoking the provided
-- | update action.
-- |
-- | The action returned from the callback is used to free resources and
-- | cancel effects acquired by the setup action.
makeBehaviour
  :: forall t m a
   . MonadRaff t m
  => a
  -> ((a -> Effect Unit) -> Effect (Effect Unit))
  -> m (Behaviour t a)
makeBehaviour initialValue = stepper initialValue <=< makeEvent

-- | Create a new behaviour along with an action that can be used to update
-- | its value imperatively.
newBehaviour :: forall t m a. MonadRaff t m => a -> m (BehaviourIO t a)
newBehaviour initialValue = do
  { event, fire } <- newEvent
  behaviour <- stepper initialValue event
  pure { behaviour, update: fire }

-- | Make an event from a function that receives a trigger callback inside of
-- | an `Aff` context.
-- |
-- | A canceller can be attached to the `Aff` context to free resources
-- | acquired inside the callback.
makeEventAff
  :: forall t m a
   . MonadRaff t m
  => ((a -> Aff Unit) -> Aff Unit)
  -> m (Event t a)
makeEventAff f = makeEvent \fire -> do
  fiber <- launchAff $ f $ liftEffect <<< fire
  pure $ launchAff_ $ killFiber (error "killed") fiber

-- | Make an event that fires after a given duration. Note that the timing may
-- | not be precise in practice.
delayEvent :: forall d t m. Duration d => MonadRaff t m => d -> m (Event t Unit)
delayEvent d = makeEventAff \fire -> do
  delay $ fromDuration d
  fire unit

-- | Make an event that fires regularly on an interval defined by the given
-- | duration. Note that the timing of the interval is lossy and the error
-- | accumulates (i.e. the event performs no error correction with each
-- | firing). As such, it is not a good idea to use this in contexts where
-- | it is important that total time elapsed aligns with the number of
-- | occurrences, such as setting up a clock.
-- |
-- | To setup a clock with `intervalEvent`, it is a good idea to use it to
-- | sample `timeB` which uses high-precision timers:
-- |
-- | ```purescript
-- | firesApproxEvery30ms <- intervalEvent $ Milliseconds 30.0
-- | let accurateMsSampledApproxEvery30ms = timeB <& firesApproxEvery30ms
-- | ```
intervalEvent
  :: forall d t m. Duration d => MonadRaff t m => d -> m (Event t Unit)
intervalEvent d = makeEventAff \fire -> forever do
  delay $ fromDuration d
  fire unit

-- | Create an event by reacting to another event. This general-purpose
-- | combinator is extremely flexible, and can be used to create new events and
-- | state accumulations in response to another event firing, and can also
-- | end the propagation (by returning a `Nothing`)
push :: forall t a b. (a -> RaffPush t (Maybe b)) -> Event t a -> Event t b
push f (Event e) = Event $ Internal._cached $ Internal._pushRaw (coerce f) e

-- | Flipped version of `push` for aliasing as `==>`.
pushFlipped
  :: forall t a b. Event t a -> (a -> RaffPush t (Maybe b)) -> Event t b
pushFlipped = flip push

infixl 1 push as <~
infixl 1 pushFlipped as ~>

-- | Collapses an `Event` of push actions into a regular `Event`. Alias for
-- | `push identity`.
pushed :: forall t a. Event t (RaffPush t (Maybe a)) -> Event t a
pushed = push identity

-- | A version of `push` which always propagates the result.
pushAlways :: forall t a b. (a -> RaffPush t b) -> Event t a -> Event t b
pushAlways f = push $ map Just <<< f

-- | A bracket-like version of `performAsync` which allows the caller to
-- | work with a resource whose lifecycle should be tied to that of the event.
-- | Accepts a setup action, a teardown action, and an `Event` of actions to
-- | perform in the context of the acquired resources. Returns an `Event` that
-- | collects the results (possibly many) produced by each invocation.
performAsyncWithSetup
  :: forall t m r a
   . MonadRaff t m
  => Effect r
  -> (r -> Effect Unit)
  -> Event t (r -> (a -> Effect Unit) -> Effect (Effect Unit))
  -> m (Event t a)
performAsyncWithSetup setup teardown (Event event) = liftRaff
  $ Raff
  $ coerce
  $ Internal._perform (Internal.PerformParent { setup, teardown, event })

-- | Perform an async effect whenever an `Event` fires and collect the result in a
-- | new `Event`. Note that the resulting `Event` may fire more than once per
-- | firing of the parent event, as the trigger function can be called an
-- | arbitrary number of times. Occurences of the resulting event will also
-- | occur _slightly_ later than the original event, even if they are fired
-- | synchronously (i.e. they fire in a new frame).
performAsync
  :: forall t m a
   . MonadRaff t m
  => Event t ((a -> Effect Unit) -> Effect (Effect Unit))
  -> m (Event t a)
performAsync (Event e) =
  performAsyncWithSetup (pure unit) (\_ -> pure unit)
    $ Event
    $ Internal._pushRaw (pure <<< Just <<< const)
    $ e

-- | Perform an effect whenever an `Event` fires and collect the results in a
-- | new `Event`. Note that occurences of the resulting event will
-- | occur _slightly_ later than the original event (i.e. they fire in a new frame).
perform
  :: forall t m a
   . MonadRaff t m
  => Event t (Effect a)
  -> m (Event t a)
perform (Event event) = performAsync
  $ Event
  $ Internal._pushRaw
      ( \effect -> pure $ Just \fire -> do
          fire =<< effect
          pure $ pure unit
      )
  $ event

-- | A bracket-like version of `perform` which allows the caller to
-- | work with a resource whose lifecycle should be tied to that of the event.
-- | Accepts a setup action, a teardown action, and an `Event` of actions to
-- | perform in the context of the acquired resources. Returns an `Event` that
-- | collects the results produced by each invocation.
performWithSetup
  :: forall t m r a
   . MonadRaff t m
  => Effect r
  -> (r -> Effect Unit)
  -> Event t (r -> Effect a)
  -> m (Event t a)
performWithSetup setup teardown (Event event) =
  performAsyncWithSetup setup teardown
    $ Event
    $ Internal._pushRaw
        ( \effect -> pure $ Just \resource fire -> do
            fire =<< effect resource
            pure $ pure unit
        )
    $ event

-- | A debug function that logs occurences of an event to the console.
traceEvent :: forall t. DebugWarning => String -> Event t ~> Event t
traceEvent msg = traceEventWith msg identity

-- | A debug function that logs filtered occurences of an event to the console.
traceEventWith
  :: forall t a b
   . DebugWarning
  => String
  -> (a -> b)
  -> Event t a
  -> Event t a
traceEventWith msg f = pushAlways \a -> do
  traceM { msg: "traceEvent: " <> msg, value: f a }
  pure a

-- | A version of `align` that allows the resulting `Event` to omit
-- | occurrences. Equivalent to `compact $ align f e1 e2`.
alignMaybe
  :: forall t a b c
   . (These a b -> Maybe c)
  -> Event t a
  -> Event t b
  -> Event t c
alignMaybe f = alignMaybeM (pure <<< f)

-- | A version of `align` that allows the resulting `Event` to trigger `push`
-- | actions. Equivalent to `pushAlways f $ aligned e1 e2`.
alignM
  :: forall t a b c
   . (These a b -> RaffPush t c)
  -> Event t a
  -> Event t b
  -> Event t c
alignM f = alignMaybeM (map Just <<< f)

-- | A version of `align` that allows the resulting `Event` to trigger `push`
-- | actions and omit occurrences. Equivalent to `pushed $ align f e1 e2` or
-- | `push f $ aligned e1 e2`.
alignMaybeM
  :: forall t a b c
   . (These a b -> RaffPush t (Maybe c))
  -> Event t a
  -> Event t b
  -> Event t c
alignMaybeM f (Event a) (Event b) = Event
  $ Internal._cached
  $ Internal._mergeWithMaybeM
      (f' <<< This)
      (f' <<< That)
      (map f' <<< Both)
      a
      b
  where
  f' :: These a b -> Internal.PropagateM (Maybe c)
  f' = coerce f

-- | Accumulate state in an event. This behaves like a left-scan of the input
-- | event. The initial state does not get fired.
accumE
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> b)
  -> b
  -> Event t a
  -> m (Event t b)
accumE f = accumMaybeE \b a -> Just $ f b a

-- | A version of `accumE` that allows state to be accumulated in the `RaffPush`
-- | monad.
accumME
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> RaffPush t b)
  -> b
  -> Event t a
  -> m (Event t b)
accumME f = accumMaybeME \b a -> Just <$> f b a

-- | A version of `accumE` that allows occurences of the parent `Event` to be ignored.
accumMaybeE
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> Maybe b)
  -> b
  -> Event t a
  -> m (Event t b)
accumMaybeE f = accumMaybeME \b a -> pure $ f b a

-- | A version of `accumE` that allows occurences of the parent `Event` to be ignored
-- | and operates in the `RafPush` monad.
accumMaybeME
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> RaffPush t (Maybe b))
  -> b
  -> Event t a
  -> m (Event t b)
accumMaybeME f seed ea = mfix \eb -> do
  bb <- stepper seed eb
  pure $ ea ~> \a -> do
    b <- sample bb
    f b a

-- | Tag each occurence with a monotonically incrementing index value.
indexed
  :: forall t m a n
   . MonadRaff t m
  => Semiring n
  => Event t a
  -> m (Event t (Tuple n a))
indexed = indexedFrom zero

-- | Tag each occurence with a monotonically incrementing index value,
-- | discarding the original value of the `Event`
indexed_
  :: forall t m a n. MonadRaff t m => Semiring n => Event t a -> m (Event t n)
indexed_ = indexedFrom_ zero

-- | Tag each occurence with a monotonically incrementing index value, starting
-- | at an arbitrary point.
indexedFrom
  :: forall t m a n
   . MonadRaff t m
  => Semiring n
  => n
  -> Event t a
  -> m (Event t (Tuple n a))
indexedFrom = mapAccum_ \n a -> { accum: n + one, value: Tuple n a }

-- | Tag each occurence with a monotonically incrementing index value,
-- | discarding the original value of the `Event`, starting at an arbitrary
-- | point.
indexedFrom_
  :: forall t m a n
   . MonadRaff t m
  => Semiring n
  => n
  -> Event t a
  -> m (Event t n)
indexedFrom_ = mapAccum_ \n _ -> { accum: n + one, value: n }

-- | Accumulate state and emit event occurences at the same time.
mapAccum_
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> Accum b c)
  -> b
  -> Event t a
  -> m (Event t c)
mapAccum_ f b e = _.value <$> mapAccum f b e

-- | Effectfully accumulate state and emit event occurences at the same time.
mapAccumM_
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> RaffPush t (Accum b c))
  -> b
  -> Event t a
  -> m (Event t c)
mapAccumM_ f b e = _.value <$> mapAccumM f b e

-- | Accumulate state and emit event occurences at the same time, allowing both
-- | state updates and event firings to be skipped independently.
mapAccumMaybe_
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> Accum (Maybe b) (Maybe c))
  -> b
  -> Event t a
  -> m (Event t c)
mapAccumMaybe_ f b e = _.value <$> mapAccumMaybe f b e

-- | Effectfully accumulate state and emit event occurences at the same time,
-- | allowing both state updates and event firings to be skipped independently.
mapAccumMaybeM_
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> RaffPush t (Accum (Maybe b) (Maybe c)))
  -> b
  -> Event t a
  -> m (Event t c)
mapAccumMaybeM_ f b e = _.value <$> mapAccumMaybeM f b e

-- | Collapse an `Event` of `Events` into a single event that fires whatever
-- | the most recently fired `Event` fires, starting with an initial `Event`.
-- | Note that if the outer event fires a new event which is its self firing at
-- | that same instance, the resulting event _will not_ fire with the value of the
-- | new event (it will fire any value the previous event happens to be firing,
-- | however). Generally, this is preferred as it is more performant and rarely
-- | causes problems. However, if you absolutely need the observe the new
-- | event's value, use `switchEImmediately` instead.
switchE
  :: forall t m a
   . MonadRaff t m
  => Event t a
  -> Event t (Event t a)
  -> m (Event t a)
switchE e0 ee = switch <$> stepper e0 ee

-- | Works similarly to `switchE` except that if an `Event` is firing the
-- | moment we switch to it, the resulting `Event` will fire as well. `switchE`
-- | should generally be preferred unless this behaviour is necessary.
switchEImmediately
  :: forall t m a
   . MonadRaff t m
  => Event t a
  -> Event t (Event t a)
  -> m (Event t a)
switchEImmediately e0 ee = do
  e <- switchE e0 ee
  pure $ join ee <|> e

-- | Efficiently fan an `Event` of `Map`s to an `EventSelector` that can cheaply
-- | produce new events for each key. The resulting even will fire whenever its
-- | associated key is present in a `Map` fired by the parent `Event`.
fanMap :: forall t k v. Ord k => Event t (Map k v) -> EventSelector t k v
fanMap (Event parent) = unsafePerformEffect do
  cache <- RM.empty
  let fan = { parent, cache }
  pure $ EventSelector \k -> Event $ mapFanEvent k fan

-------------------------------------------------------------------------------
-- Behaviours
-------------------------------------------------------------------------------

-- | A value that changes continuously over time. The notion of continuity is
-- | represented by the fact that Behaviours cannot be reacted to, only
-- | sampled (A behaviour cannot tell you when it changes, only what its
-- | current value is).
newtype Behaviour (t :: Timeline) a = Behaviour (Internal.BehaviourRep a)

type role Behaviour nominal representational

derive newtype instance Functor (Behaviour t)
derive newtype instance Apply (Behaviour t)
derive newtype instance Applicative (Behaviour t)
derive newtype instance Bind (Behaviour t)
instance Monad (Behaviour t)
derive newtype instance Lazy (Behaviour t a)

instance Semigroup a => Semigroup (Behaviour t a) where
  append = lift2 append

instance Monoid a => Monoid (Behaviour t a) where
  mempty = pure mempty

instance Semiring a => Semiring (Behaviour t a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance Ring a => Ring (Behaviour t a) where
  sub = lift2 sub

instance CommutativeRing a => CommutativeRing (Behaviour t a)

instance DivisionRing a => DivisionRing (Behaviour t a) where
  recip = map recip

instance Field a => EuclideanRing (Behaviour t a) where
  degree = const 1
  div = lift2 div
  mod = lift2 mod

instance HeytingAlgebra a => HeytingAlgebra (Behaviour t a) where
  ff = pure ff
  tt = pure tt
  implies = lift2 implies
  conj = lift2 conj
  disj = lift2 disj
  not = map not

instance BooleanAlgebra a => BooleanAlgebra (Behaviour t a)

-- | Create a new behaviour whose value is computed by evaluating the given
-- | computation (which may sample other behaviours).
pull :: forall t a. RaffPull t a -> Behaviour t a
pull = coerce (_pull :: Internal.PullM a -> Internal.BehaviourRep a)

-- | A behaviour that gives the current network time in milliseconds (with 0ms
-- | occurring when the network was started).
timeB :: forall t. Behaviour t Time
timeB = Behaviour $ Internal._time

-- | Create a behaviour whose value is patched over time by an `Event`, satring
-- | from an initial value.
patcher
  :: forall patch t m a
   . MonadRaff t m
  => Patch patch a
  => a
  -> Event t patch
  -> m (Behaviour t a)
patcher i (Event e) = liftRaff $ Raff do
  latch <- newLatch i e
  pure $ Behaviour $ latchBehaviour latch

-- | Create a behaviour whose value is replaced over time by an `Event`, satring
-- | from an initial value.
stepper :: forall t m a. MonadRaff t m => a -> Event t a -> m (Behaviour t a)
stepper i = map coerce <<< patcher (Identity i)

-- | Create a behaviour that switches between behaviours fired by an event,
-- | starting from an initial behaviour.
switcher
  :: forall t m a
   . MonadRaff t m
  => Behaviour t a
  -> Event t (Behaviour t a)
  -> m (Behaviour t a)
switcher i e = join <$> stepper i e

-- | Sample the current value of a `Behaviour`.
sample :: forall t m a. MonadPull t m => Behaviour t a -> m a
sample (Behaviour b) = liftPull $ RaffPull $ Internal._sample b

-- | Sample a `Behaviour` of functions whenever an `Event` fires and apply the
-- | sampled function to the value fired by that `Event`. Commonly used in its
-- | infix operator form `(<&>)`.
sampleApply :: forall t a b. Behaviour t (a -> b) -> Event t a -> Event t b
sampleApply = liftSample2 identity

infixl 4 sampleApply as <&>

-- | Sample a `Behaviour` of functions whenever an `Event` fires and apply the
-- | sampled function to the value fired by that `Event`. The resulting event
-- | only fires when the function returns a `Just`.
sampleApplyMaybe
  :: forall t a b. Behaviour t (a -> Maybe b) -> Event t a -> Event t b
sampleApplyMaybe = liftSampleMaybe2 identity

-- | Lift a 2-argument function over a `Behaviour` and sample the result with
-- | an `Event`.
liftSample2
  :: forall t a b c. (a -> b -> c) -> Behaviour t a -> Event t b -> Event t c
liftSample2 f = liftSampleMaybe2 \a b -> Just $ f a b

-- | Lift an effectful 2-argument function over a `Behaviour` and sample the
-- | result with an `Event`.
liftSampleM2
  :: forall t a b c
   . (a -> b -> RaffPush t c)
  -> Behaviour t a
  -> Event t b
  -> Event t c
liftSampleM2 f = liftSampleMaybeM2 \a b -> Just <$> f a b

-- | Lift a 2-argument function over a `Behaviour` and sample the result with
-- | an `Event`. The resulting event only fires when the function returns a
-- | `Just`.
liftSampleMaybe2
  :: forall t a b c
   . (a -> b -> Maybe c)
  -> Behaviour t a
  -> Event t b
  -> Event t c
liftSampleMaybe2 f = liftSampleMaybeM2 \a b -> pure $ f a b

-- | Lift an effectful 2-argument function over a `Behaviour` and sample the
-- | result with an `Event`. The resulting event only fires when the function
-- | returns a `Just`.
liftSampleMaybeM2
  :: forall t a b c
   . (a -> b -> RaffPush t (Maybe c))
  -> Behaviour t a
  -> Event t b
  -> Event t c
liftSampleMaybeM2 f ba = push \b -> do
  a <- sample ba
  f a b

-- | Replace the value of an event with the value of a behaviour at the time
-- | the event was firing.
tag :: forall t a b. Behaviour t a -> Event t b -> Event t a
tag = liftSample2 \a _ -> a

infixl 4 tag as <&

-- | Replace the value of an event with the value of a behaviour at the time
-- | the event was firing. If the `Behaviour` held `Nothing`, the resulting
-- | event will not fire.
tagMaybe :: forall t a b. Behaviour t (Maybe a) -> Event t b -> Event t a
tagMaybe = liftSampleMaybe2 \ma _ -> ma

-- | Filter the values fired by an Event such that they only fire when the
-- | `Behaviour`'s value is `true`.
gate :: forall t a. Behaviour t Boolean -> Event t a -> Event t a
gate = liftSampleMaybe2 case _ of
  true -> Just
  _ -> const Nothing

-- | Partition the values fired by an `Event` into two events such that the
-- | `yes` `Event` only fires when the `Behaviour`'s value is `true` and the
-- | `no` `Event` only fires when the `Behaviour`'s value is `false`.
split
  :: forall t a
   . Behaviour t Boolean
  -> Event t a
  -> { no :: Event t a, yes :: Event t a }
split b e =
  let
    { left, right } = separate $ liftSample2
      ( case _ of
          false -> Left
          true -> Right
      )
      b
      e
  in
    { no: left, yes: right }

-- | Filter the values fired by an Event with a time-varying predicate.
filterApply :: forall t a. Behaviour t (a -> Boolean) -> Event t a -> Event t a
filterApply = liftSampleMaybe2 maybeBool

-- | Partition the values fired by an Event with a time-varying predicate.
partitionApply
  :: forall t a
   . Behaviour t (a -> Boolean)
  -> Event t a
  -> { no :: Event t a, yes :: Event t a }
partitionApply b e =
  let
    { left, right } = separate $ liftSample2 eitherBool b e
  in
    { no: left, yes: right }

-- | Partition the values fired by an Event with a time-varying disjunction.
partitionMapApply
  :: forall t a b c
   . Behaviour t (a -> Either b c)
  -> Event t a
  -> { left :: Event t b, right :: Event t c }
partitionMapApply b e = separate $ b <&> e

-- | Accumulate state inside of a behaviour.
accumB
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> b)
  -> b
  -> Event t a
  -> m (Behaviour t b)
accumB f = accumMaybeB \b a -> Just $ f b a

-- | Accumulate state effectfully inside of a behaviour.
accumMB
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> RaffPush t b)
  -> b
  -> Event t a
  -> m (Behaviour t b)
accumMB f = accumMaybeMB \b a -> Just <$> f b a

-- | Accumulate state inside of a behaviour, ignoring certain imputs.
accumMaybeB
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> Maybe b)
  -> b
  -> Event t a
  -> m (Behaviour t b)
accumMaybeB f = accumMaybeMB \b a -> pure $ f b a

-- | Accumulate state effectfully inside of a behaviour, ignoring certain imputs.
accumMaybeMB
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> RaffPush t (Maybe b))
  -> b
  -> Event t a
  -> m (Behaviour t b)
accumMaybeMB f seed ea = mfix \bb -> do
  let
    eb = ea ~> \a -> do
      b <- sample bb
      f b a
  stepper seed eb

-- | Efficiently accumulate state and fire events at the same time.
mapAccum
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> Accum b c)
  -> b
  -> Event t a
  -> m (Accum (Behaviour t b) (Event t c))
mapAccum f = mapAccumMaybe \b a ->
  let
    { accum, value } = f b a
  in
    { accum: Just accum, value: Just value }

-- | Efficiently accumulate state and fire events effectfully at the same time.
mapAccumM
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> RaffPush t (Accum b c))
  -> b
  -> Event t a
  -> m (Accum (Behaviour t b) (Event t c))
mapAccumM f = mapAccumMaybeM \b a -> do
  { accum, value } <- f b a
  pure { accum: Just accum, value: Just value }

-- | Efficiently accumulate state and fire events at the same time.
-- | Both state updates and event firings can be independently skipped.
mapAccumMaybe
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> Accum (Maybe b) (Maybe c))
  -> b
  -> Event t a
  -> m (Accum (Behaviour t b) (Event t c))
mapAccumMaybe f = mapAccumMaybeM \b a -> pure $ f b a

-- | Efficiently accumulate state and fire events effectfully at the same time.
-- | Both state updates and event firings can be independently skipped.
mapAccumMaybeM
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> RaffPush t (Accum (Maybe b) (Maybe c)))
  -> b
  -> Event t a
  -> m (Accum (Behaviour t b) (Event t c))
mapAccumMaybeM f seed ea = do
  Tuple eaccum accum <- mfix2 \_ accum -> do
    let
      eaccum = ea ~> \a -> do
        b <- sample accum
        result <- f b a
        pure case result of
          { accum: Nothing, value: Nothing } -> Nothing
          _ -> Just result
    accum' <- stepper seed $ filterMap _.accum eaccum
    pure $ Tuple eaccum accum'
  pure { accum, value: filterMap _.value eaccum }

-- | Create an `Event` that fires whatever the `Event` currently held by the
-- | `Behaviour` fires.
switch :: forall t a. Behaviour t (Event t a) -> Event t a
switch (Behaviour parent) = unsafePerformEffect do
  cache <- RM.empty
  pure $ Event $ switchEvent { parent: coerce parent, cache }

foreign import requestAnimationFrame
  :: (Time -> Effect Unit) -> Window -> Effect RequestAnimationFrameId

-- | Connect a `Bevhaviour` to an external animation sink. This is the only
-- | exception of being able to react to behaviour values, but it is unsafe to
-- | treat it this way. Instead, the given function should assume that it is
-- | reacting to a poll of values from the behaviour.
animate
  :: forall t m a
   . MonadRaff t m
  => Behaviour t a
  -> (a -> Effect Unit)
  -> m Unit
animate b handle =
  animateWithSetup b (pure unit) (const $ pure unit) (const <<< handle)

-- | A version of `animate` that allows a resource to be setup.
animateWithSetup
  :: forall t m r a
   . MonadRaff t m
  => Behaviour t a
  -> Effect r
  -> (r -> Effect Unit)
  -> (a -> r -> Effect Unit)
  -> m Unit
animateWithSetup (Behaviour b) setup teardown handle = liftRaff do
  networkTime <- Raff $ asks _.time
  animation <- liftEffect do
    globalTime <- getHighResTimestamp
    let startTime = globalTime `subTime` networkTime
    let
      initialize :: InitializeAnimation a
      initialize sampler = do
        resource <- setup
        requestIdRef <- RM.empty
        w <- window
        let
          cancel = do
            mRequestId <- RM.read requestIdRef
            for_ mRequestId \requestId -> cancelAnimationFrame requestId w
          invalidate = do
            cancel
            let
              go time = do
                Tuple isContinuous a <- sampler $ time `subTime` startTime
                handle a resource
                if isContinuous then do
                  nextFrameId <- requestAnimationFrame go w
                  RM.write nextFrameId requestIdRef
                else
                  RM.clear requestIdRef
            go globalTime
        pure
          { invalidate
          , dispose: do
              cancel
              teardown resource
          }
    newAnimation b initialize
  onReady do
    liftEffect animation.invalidate
    onCleanup animation.dispose

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

-- | Interpret an Event morphism as a sparse Array morphism. Used for testing.
interpret
  :: forall a b
   . (forall t. Event t a -> Raff t (Event t b))
  -> Array (Maybe a)
  -> Aff (Array (Maybe b))
interpret f = interpret2 (const f) []

-- | Interpret an Event morphism as a sparse Array morphism. Used for testing.
interpret2
  :: forall a b c
   . (forall t. Event t a -> Event t b -> Raff t (Event t c))
  -> Array (Maybe a)
  -> Array (Maybe b)
  -> Aff (Array (Maybe c))
interpret2 f as bs = do
  Internal.interpret2
    ( \e1 e2 -> do
        Event e3 <- coerce f (Event e1) (Event e2)
        pure e3
    )
    as
    bs
