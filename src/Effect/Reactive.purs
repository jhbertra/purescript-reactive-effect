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
  , (==>)
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
  , makeEvent
  , makeEventAff
  , mapAccumB
  , mapAccumMB
  , mapAccumM_
  , mapAccumMaybeB
  , mapAccumMaybeMB
  , mapAccumMaybeM_
  , mapAccumMaybe_
  , mapAccum_
  , newEvent
  , partitionApply
  , partitionMapApply
  , patcher
  , perform
  , performAsync
  , performAsyncWithSetup
  , performWithSetup
  , pull
  , pullContinuous
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
  , time
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
import Data.Time.Duration (class Duration, fromDuration, toDuration)
import Data.Traversable (Accum)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Debug (class DebugWarning, traceM)
import Effect (Effect)
import Effect.Aff (Aff, bracket, delay, killFiber, launchAff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Effect.RW (RWEffect(..), runRWEffect)
import Effect.Reactive.Internal
  ( BehaviourRep
  , BuildM
  , EventRep
  , InvokeTrigger(..)
  , PerformParent(..)
  , PropagateM
  , SampleHint(..)
  , _asap
  , _neverE
  , _pushRaw
  , _time
  , tellHint
  ) as Internal
import Effect.Reactive.Internal (FireTriggers(..), getEventHandle)
import Effect.Reactive.Internal.Build (liftBuild, runBuildM, runFrame)
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

-------------------------------------------------------------------------------
-- Raff monad
-------------------------------------------------------------------------------

foreign import data Timeline :: Type

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

launchRaff_ :: forall a. (forall t. Raff t (Event t a)) -> Aff Unit
launchRaff_ m = void $ launchRaff m

launchRaff :: forall a. (forall t. Raff t (Event t a)) -> Aff a
launchRaff (Raff m) = do
  queue <- Queue.new
  bracket
    ( liftEffect $ runBuildM queue (pure mempty) do
        Event eResult <- m
        runFrame do
          eResultHandle <- getEventHandle eResult
          liftEffect do
            mResult <- RM.read eResultHandle.currentValue
            pure $ eResultHandle /\ mResult
    )
    (liftEffect <<< _.dispose)
    ( \{ result, fire: FireTriggers fire } -> do
        let eResultHandle /\ mResult = result
        case mResult of
          Just a -> pure a
          Nothing -> untilJust do
            { triggers, onComplete } <- Queue.read queue
            liftEffect $ fire triggers do
              onComplete
              RM.read eResultHandle.currentValue
    )

-------------------------------------------------------------------------------
-- RaffPush monad
-------------------------------------------------------------------------------

newtype RaffPush (t :: Timeline) a = RaffPush (Internal.PropagateM a)

type role RaffPush nominal representational

derive newtype instance Functor (RaffPush t)
derive newtype instance Apply (RaffPush t)
derive newtype instance Applicative (RaffPush t)
derive newtype instance Bind (RaffPush t)
derive newtype instance Monad (RaffPush t)
derive newtype instance MonadFix (RaffPush t)
derive newtype instance Semigroup a => Semigroup (RaffPush t a)
derive newtype instance Monoid a => Monoid (RaffPush t a)

-------------------------------------------------------------------------------
-- RaffPull monad
-------------------------------------------------------------------------------

newtype RaffPull (t :: Timeline) a = RaffPull (Internal.BehaviourRep a)

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
  liftPull (RaffPull m) = liftEffect $ snd <$> runRWEffect m Nothing

instance MonadPull t (RaffPush t) where
  liftPull (RaffPull m) = RaffPush $ liftEffect $ snd <$> runRWEffect m Nothing

instance MonadPull t (RaffPull t) where
  liftPull = coerce

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

newtype Event (t :: Timeline) a = Event (Internal.EventRep a)

type role Event nominal representational

type EventIO (t :: Timeline) a =
  { event :: Event t a, fire :: a -> Effect Unit }

newtype EventSelector t k v = EventSelector (k -> Event t v)

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

asap :: forall t. Raff t (Event t Unit)
asap = coerce Internal._asap

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
        { triggers: [ mkExists $ Internal.InvokeTrigger { trigger, value } ]
        , onComplete
        }

  pure $ Event $ inputEvent input

makeEvent
  :: forall t m a
   . MonadRaff t m
  => ((a -> Effect Unit) -> Effect (Effect Unit))
  -> m (Event t a)
makeEvent subscribe = makeEventWithFireCallback \fire ->
  subscribe \value -> fire value $ pure unit

delayEvent :: forall d t m. Duration d => MonadRaff t m => d -> m (Event t Unit)
delayEvent d = makeEventAff \fire -> do
  delay $ fromDuration d
  fire unit

intervalEvent
  :: forall d t m. Duration d => MonadRaff t m => d -> m (Event t Unit)
intervalEvent d = makeEventAff \fire -> forever do
  delay $ fromDuration d
  fire unit

makeEventAff
  :: forall t m a
   . MonadRaff t m
  => ((a -> Aff Unit) -> Aff Unit)
  -> m (Event t a)
makeEventAff f = makeEvent \fire -> do
  fiber <- launchAff $ f $ liftEffect <<< fire
  pure $ launchAff_ $ killFiber (error "killed") fiber

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
                [ mkExists $ Internal.InvokeTrigger { trigger: t, value } ]
            , onComplete: (pure unit)
            }
    }

push :: forall t a b. (a -> RaffPush t (Maybe b)) -> Event t a -> Event t b
push f (Event e) = Event $ Internal._cached $ Internal._pushRaw (coerce f) e

pushFlipped
  :: forall t a b. Event t a -> (a -> RaffPush t (Maybe b)) -> Event t b
pushFlipped = flip push

infixl 1 pushFlipped as ==>

pushed :: forall t a. Event t (RaffPush t (Maybe a)) -> Event t a
pushed = push identity

pushAlways :: forall t a b. (a -> RaffPush t b) -> Event t a -> Event t b
pushAlways f = push $ map Just <<< f

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

traceEvent :: forall t. DebugWarning => String -> Event t ~> Event t
traceEvent msg = traceEventWith msg identity

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

alignMaybe
  :: forall t a b c
   . (These a b -> Maybe c)
  -> Event t a
  -> Event t b
  -> Event t c
alignMaybe f = alignMaybeM (pure <<< f)

alignM
  :: forall t a b c
   . (These a b -> RaffPush t c)
  -> Event t a
  -> Event t b
  -> Event t c
alignM f = alignMaybeM (map Just <<< f)

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

accumE
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> b)
  -> b
  -> Event t a
  -> m (Event t b)
accumE f = accumMaybeE \b a -> Just $ f b a

accumME
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> RaffPush t b)
  -> b
  -> Event t a
  -> m (Event t b)
accumME f = accumMaybeME \b a -> Just <$> f b a

accumMaybeE
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> Maybe b)
  -> b
  -> Event t a
  -> m (Event t b)
accumMaybeE f = accumMaybeME \b a -> pure $ f b a

accumMaybeME
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> RaffPush t (Maybe b))
  -> b
  -> Event t a
  -> m (Event t b)
accumMaybeME f seed ea = mfix \eb -> do
  bb <- stepper seed eb
  pure $ ea ==> \a -> do
    b <- sample bb
    f b a

indexed
  :: forall t m a n
   . MonadRaff t m
  => Semiring n
  => Event t a
  -> m (Event t (Tuple n a))
indexed = indexedFrom zero

indexed_
  :: forall t m a n. MonadRaff t m => Semiring n => Event t a -> m (Event t n)
indexed_ = indexedFrom_ zero

indexedFrom
  :: forall t m a n
   . MonadRaff t m
  => Semiring n
  => n
  -> Event t a
  -> m (Event t (Tuple n a))
indexedFrom = mapAccum_ \n a -> { accum: n + one, value: Tuple n a }

indexedFrom_
  :: forall t m a n
   . MonadRaff t m
  => Semiring n
  => n
  -> Event t a
  -> m (Event t n)
indexedFrom_ = mapAccum_ \n _ -> { accum: n + one, value: n }

mapAccum_
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> Accum b c)
  -> b
  -> Event t a
  -> m (Event t c)
mapAccum_ f b e = _.value <$> mapAccumB f b e

mapAccumM_
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> RaffPush t (Accum b c))
  -> b
  -> Event t a
  -> m (Event t c)
mapAccumM_ f b e = _.value <$> mapAccumMB f b e

mapAccumMaybe_
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> Accum (Maybe b) (Maybe c))
  -> b
  -> Event t a
  -> m (Event t c)
mapAccumMaybe_ f b e = _.value <$> mapAccumMaybeB f b e

mapAccumMaybeM_
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> RaffPush t (Accum (Maybe b) (Maybe c)))
  -> b
  -> Event t a
  -> m (Event t c)
mapAccumMaybeM_ f b e = _.value <$> mapAccumMaybeMB f b e

switchE
  :: forall t m a
   . MonadRaff t m
  => Event t a
  -> Event t (Event t a)
  -> m (Event t a)
switchE e0 ee = switch <$> stepper e0 ee

switchEImmediately
  :: forall t m a
   . MonadRaff t m
  => Event t a
  -> Event t (Event t a)
  -> m (Event t a)
switchEImmediately e0 ee = do
  e <- switchE e0 ee
  pure $ join ee <|> e

fanMap :: forall t k v. Ord k => Event t (Map k v) -> EventSelector t k v
fanMap (Event parent) = unsafePerformEffect do
  cache <- RM.empty
  let fan = { parent, cache }
  pure $ EventSelector \k -> Event $ mapFanEvent k fan

-------------------------------------------------------------------------------
-- Behaviours
-------------------------------------------------------------------------------

newtype Behaviour (t :: Timeline) a = Behaviour (Internal.BehaviourRep a)

type role Behaviour nominal representational

instance Functor (Behaviour t) where
  map f = pull <<< map f <<< sample

instance Apply (Behaviour t) where
  apply f a = pull $ sample f <*> sample a

instance Applicative (Behaviour t) where
  pure = Behaviour <<< pure

instance Bind (Behaviour t) where
  bind a f = pull $ sample a >>= sample <<< f

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

pull :: forall t a. RaffPull t a -> Behaviour t a
pull = coerce (_pull :: Internal.BehaviourRep a -> Internal.BehaviourRep a)

pullContinuous :: forall t a. RaffPull t a -> Behaviour t a
pullContinuous (RaffPull m) =
  pull $ RaffPull $ Internal.tellHint Internal.SampleContinuous *> m

time :: forall t m d. MonadRaff t m => Duration d => m (Behaviour t d)
time = do
  b <- liftRaff $ Raff Internal._time
  pure $ toDuration <$> Behaviour b

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

stepper :: forall t m a. MonadRaff t m => a -> Event t a -> m (Behaviour t a)
stepper i = map coerce <<< patcher (Identity i)

switcher
  :: forall t m a
   . MonadRaff t m
  => Behaviour t a
  -> Event t (Behaviour t a)
  -> m (Behaviour t a)
switcher i e = join <$> stepper i e

sample :: forall t m a. MonadPull t m => Behaviour t a -> m a
sample (Behaviour b) = liftPull $ RaffPull b

sampleApply :: forall t a b. Behaviour t (a -> b) -> Event t a -> Event t b
sampleApply = liftSample2 identity

infixl 4 sampleApply as <&>

sampleApplyMaybe
  :: forall t a b. Behaviour t (a -> Maybe b) -> Event t a -> Event t b
sampleApplyMaybe = liftSampleMaybe2 identity

liftSample2
  :: forall t a b c. (a -> b -> c) -> Behaviour t a -> Event t b -> Event t c
liftSample2 f = liftSampleMaybe2 \a b -> Just $ f a b

liftSampleM2
  :: forall t a b c
   . (a -> b -> RaffPush t c)
  -> Behaviour t a
  -> Event t b
  -> Event t c
liftSampleM2 f = liftSampleMaybeM2 \a b -> Just <$> f a b

liftSampleMaybe2
  :: forall t a b c
   . (a -> b -> Maybe c)
  -> Behaviour t a
  -> Event t b
  -> Event t c
liftSampleMaybe2 f = liftSampleMaybeM2 \a b -> pure $ f a b

liftSampleMaybeM2
  :: forall t a b c
   . (a -> b -> RaffPush t (Maybe c))
  -> Behaviour t a
  -> Event t b
  -> Event t c
liftSampleMaybeM2 f ba = push \b -> do
  a <- sample ba
  f a b

tag :: forall t a b. Behaviour t a -> Event t b -> Event t a
tag = liftSample2 \a _ -> a

infixl 4 tag as <&

tagMaybe :: forall t a b. Behaviour t (Maybe a) -> Event t b -> Event t a
tagMaybe = liftSampleMaybe2 \ma _ -> ma

gate :: forall t a. Behaviour t Boolean -> Event t a -> Event t a
gate = liftSampleMaybe2 case _ of
  true -> Just
  _ -> const Nothing

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

filterApply :: forall t a. Behaviour t (a -> Boolean) -> Event t a -> Event t a
filterApply = liftSampleMaybe2 maybeBool

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

partitionMapApply
  :: forall t a b c
   . Behaviour t (a -> Either b c)
  -> Event t a
  -> { left :: Event t b, right :: Event t c }
partitionMapApply b e = separate $ b <&> e

accumB
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> b)
  -> b
  -> Event t a
  -> m (Behaviour t b)
accumB f = accumMaybeB \b a -> Just $ f b a

accumMB
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> RaffPush t b)
  -> b
  -> Event t a
  -> m (Behaviour t b)
accumMB f = accumMaybeMB \b a -> Just <$> f b a

accumMaybeB
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> Maybe b)
  -> b
  -> Event t a
  -> m (Behaviour t b)
accumMaybeB f = accumMaybeMB \b a -> pure $ f b a

accumMaybeMB
  :: forall t m a b
   . MonadRaff t m
  => (b -> a -> RaffPush t (Maybe b))
  -> b
  -> Event t a
  -> m (Behaviour t b)
accumMaybeMB f seed ea = mfix \bb -> do
  let
    eb = ea ==> \a -> do
      b <- sample bb
      f b a
  stepper seed eb

mapAccumB
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> Accum b c)
  -> b
  -> Event t a
  -> m (Accum (Behaviour t b) (Event t c))
mapAccumB f = mapAccumMaybeB \b a ->
  let
    { accum, value } = f b a
  in
    { accum: Just accum, value: Just value }

mapAccumMB
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> RaffPush t (Accum b c))
  -> b
  -> Event t a
  -> m (Accum (Behaviour t b) (Event t c))
mapAccumMB f = mapAccumMaybeMB \b a -> do
  { accum, value } <- f b a
  pure { accum: Just accum, value: Just value }

mapAccumMaybeB
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> Accum (Maybe b) (Maybe c))
  -> b
  -> Event t a
  -> m (Accum (Behaviour t b) (Event t c))
mapAccumMaybeB f = mapAccumMaybeMB \b a -> pure $ f b a

mapAccumMaybeMB
  :: forall t m a b c
   . MonadRaff t m
  => (b -> a -> RaffPush t (Accum (Maybe b) (Maybe c)))
  -> b
  -> Event t a
  -> m (Accum (Behaviour t b) (Event t c))
mapAccumMaybeMB f seed ea = do
  Tuple eaccum accum <- mfix2 \_ accum -> do
    let
      eaccum = ea ==> \a -> do
        b <- sample accum
        result <- f b a
        pure case result of
          { accum: Nothing, value: Nothing } -> Nothing
          _ -> Just result
    accum' <- stepper seed $ filterMap _.accum eaccum
    pure $ Tuple eaccum accum'
  pure { accum, value: filterMap _.value eaccum }

switch :: forall t a. Behaviour t (Event t a) -> Event t a
switch (Behaviour parent) = unsafePerformEffect do
  cache <- RM.empty
  pure $ Event $ switchEvent { parent: coerce parent, cache }

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

interpret
  :: forall a b
   . (forall t. Event t a -> Raff t (Event t b))
  -> Array (Maybe a)
  -> Aff (Array (Maybe b))
interpret f = interpret2 (const f) []

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
