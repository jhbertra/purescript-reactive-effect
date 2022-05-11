module Effect.Reactive
  ( (<&)
  , (<&>)
  , class MonadPull
  , Behaviour
  , Event
  , Raff
  , RaffPull
  , RaffPush
  , Timeline
  , accumB
  , accumE
  , accumMB
  , accumME
  , accumMaybeB
  , accumMaybeE
  , accumMaybeMB
  , accumMaybeME
  , alignPush
  , bracketReact
  , filterApply
  , gate
  , indexed
  , indexedFrom
  , indexedFrom_
  , indexed_
  , interpret
  , interpret2
  , liftPull
  , liftSample2
  , liftSampleM2
  , liftSampleMaybe2
  , liftSampleMaybeM2
  , makeEvent
  , mapAccumB
  , mapAccumMB
  , mapAccumMaybeB
  , mapAccumMaybeMB
  , mapAccum_
  , mapAccumM_
  , mapAccumMaybe_
  , mapAccumMaybeM_
  , newEvent
  , partitionApply
  , partitionMapApply
  , patcher
  , pull
  , pullContinuous
  , push
  , pushAlways
  , pushRaff
  , pushed
  , react
  , runRaff
  , runRaff_
  , sample
  , sampleApply
  , sampleApplyMaybe
  , split
  , stepper
  , switcher
  , tag
  , tagMaybe
  , traceEvent
  , traceEventWith
  ) where

import Prelude

import Concurrent.Queue (Queue)
import Concurrent.Queue as Q
import Control.Alt (class Alt)
import Control.Alternative (class Plus)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Fix (class MonadFix, mfix, mfix2)
import Control.Monad.Reader (ReaderT(..), ask, asks, runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Plus (empty)
import Data.Align (class Align, class Alignable, align)
import Data.Compactable (class Compactable, separate)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists)
import Data.Filterable (class Filterable, eitherBool, filterMap, maybeBool)
import Data.Foldable (for_, traverse_)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Patch (class Patch)
import Data.These (These, both, these)
import Data.Traversable (Accum)
import Data.Tuple (Tuple(..), snd)
import Debug (class DebugWarning, traceM)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, forkAff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.RW (RWEffect(..), runRWEffect)
import Effect.Reactive.Internal
  ( Behaviour
  , BuildM
  , Event
  , PropagateM
  , PullM
  , _alignE
  , _compactE
  , _filterE
  , _filterMapE
  , _mapE
  , _neverE
  , _partitionE
  , _partitionMapE
  , _push
  , _separateE
  ) as Internal
import Effect.Reactive.Internal
  ( BuildM(..)
  , FireTriggers(..)
  , InvokeTrigger(..)
  , PropagateM(..)
  , PullHint(..)
  , PullM
  , _push
  , tellHint
  )
import Effect.Reactive.Internal.Build (fireAndRead, runBuildM, runFrame)
import Effect.Reactive.Internal.Input
  ( inputEvent
  , newInput
  , newInputWithTriggerRef
  )
import Effect.Reactive.Internal.Latch (latchBehaviour, newLatch)
import Effect.Reactive.Internal.Pipe (_pull)
import Effect.Reactive.Internal.Reaction (_react)
import Effect.Reactive.Internal.Testing (interpret2) as Internal
import Effect.Reader (ReaderEffect(..), runReaderEffect)
import Effect.Ref.Maybe as RM
import Effect.Unlift (class MonadUnliftEffect, askUnliftEffect, unliftEffect)
import Safe.Coerce (coerce)

-------------------------------------------------------------------------------
-- Raff monad
-------------------------------------------------------------------------------

foreign import data Timeline :: Type

newtype Raff (t :: Timeline) a = Raff (ReaderT (RaffEnv t) Internal.BuildM a)

type RaffEnv (t :: Timeline) =
  { triggerQueue :: Queue (Array (Exists InvokeTrigger))
  }

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

runRaff :: forall a b. (forall t. Raff t a) -> Aff (Fiber b)
runRaff (Raff r) = do
  triggerQueue <- Q.new
  let fireTriggers = FireTriggers $ launchAff_ <<< Q.write triggerQueue
  { u } <- liftEffect $ runBuildM fireTriggers do
    u <- askUnliftEffect
    a <- runReaderT r { triggerQueue }
    runFrame $ pure { u, a }
  forkAff $ forever do
    triggers <- Q.read triggerQueue
    liftEffect $ unliftEffect u $ fireAndRead triggers $ pure unit

runRaff_ :: forall a. (forall t. Raff t a) -> Aff Unit
runRaff_ r = void (runRaff r)

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

newtype RaffPull (t :: Timeline) a = RaffPull (Internal.PullM a)

type role RaffPull nominal representational

derive newtype instance Functor (RaffPull t)
derive newtype instance Apply (RaffPull t)
derive newtype instance Applicative (RaffPull t)
derive newtype instance Bind (RaffPull t)
derive newtype instance Monad (RaffPull t)
derive newtype instance MonadEffect (RaffPull t)
derive newtype instance MonadUnliftEffect (RaffPull t)
instance MonadBase (RaffPull t) (RaffPull t) where
  liftBase = identity

instance MonadUnlift (RaffPull t) (RaffPull t) where
  withRunInBase runAction = runAction identity

derive newtype instance MonadFix (RaffPull t)
derive newtype instance Semigroup a => Semigroup (RaffPull t a)
derive newtype instance Monoid a => Monoid (RaffPull t a)

class Monad m <= MonadPull t m | m -> t where
  liftPull :: RaffPull t ~> m

instance MonadPull t (Raff t) where
  liftPull (RaffPull m) = liftEffect $ snd <$> runRWEffect m Nothing

instance MonadPull t (RaffPush t) where
  liftPull (RaffPull m) = liftEffect $ snd <$> runRWEffect m Nothing

instance MonadPull t (RaffPull t) where
  liftPull = coerce

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

newtype Event (t :: Timeline) a = Event (Internal.Event a)

type role Event nominal representational

type EventIO (t :: Timeline) a = { event :: Event t a, fire :: a -> Aff Unit }

instance Functor (Event t) where
  map f = coerce $ Internal._mapE f

instance Apply (Event t) where
  apply (Event e1) (Event e2) = Event
    $ Internal._push (pure <<< map (\(Tuple f a) -> f a) <<< both)
    $ Internal._alignE e1 e2

instance Alt (Event t) where
  alt (Event e1) (Event e2) = Event
    $ Internal._mapE (these identity identity const)
    $ Internal._alignE e1 e2

instance Plus (Event t) where
  empty = Event Internal._neverE

instance Align (Event t) where
  align f = alignPush (pure <<< Just <<< f)

instance Alignable (Event t) where
  nil = empty

instance Compactable (Event t) where
  compact (Event e) = Event $ Internal._compactE e
  separate (Event e) = coerce $ Internal._separateE e

instance Filterable (Event t) where
  filter p (Event e) = Event $ Internal._filterE p e
  filterMap f (Event e) = Event $ Internal._filterMapE f e
  partition p (Event e) = coerce $ Internal._partitionE p e
  partitionMap f (Event e) = coerce $ Internal._partitionMapE f e

instance Semigroup a => Semigroup (Event t a) where
  append = align $ these identity identity append

instance Semigroup a => Monoid (Event t a) where
  mempty = empty

derive newtype instance Lazy (Event t a)

makeEvent
  :: forall t a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Raff t (Event t a)
makeEvent register = Raff do
  triggerQueue <- asks _.triggerQueue
  input <- liftEffect $ newInput \trigger -> register \value -> launchAff_ $
    Q.write triggerQueue [ mkExists $ InvokeTrigger { trigger, value } ]
  pure $ Event $ inputEvent input

newEvent :: forall t a. Raff t (EventIO t a)
newEvent = Raff do
  { input, trigger } <- liftEffect newInputWithTriggerRef
  triggerQueue <- asks _.triggerQueue
  pure
    { event: Event $ inputEvent input
    , fire: \value -> do
        mTrigger <- liftEffect $ RM.read trigger
        for_ mTrigger \t ->
          Q.write triggerQueue
            [ mkExists $ InvokeTrigger { trigger: t, value } ]
    }

pushRaff
  :: forall t a b. (a -> Raff t (Maybe b)) -> Event t a -> Raff t (Event t b)
pushRaff f (Event e) = Raff do
  env <- ask
  let
    f' a = PM $ RE \{ fireTriggers, newLatches, newReactors, reactors } -> do
      let Raff m = f a
      let BM m' = runReaderT m env
      runReaderEffect m' { fireTriggers, newLatches, newReactors, reactors }
  pure $ Event $ _push f' e

push :: forall t a b. (a -> RaffPush t (Maybe b)) -> Event t a -> Event t b
push = coerce
  (_push :: (a -> PropagateM (Maybe b)) -> Internal.Event a -> Internal.Event b)

pushed :: forall t a. Event t (RaffPush t (Maybe a)) -> Event t a
pushed = push identity

pushAlways :: forall t a b. (a -> RaffPush t b) -> Event t a -> Event t b
pushAlways f = push $ map Just <<< f

react :: forall t a. Event t a -> (a -> Effect Unit) -> Raff t (Effect Unit)
react e = bracketReact e (pure unit) (const $ pure unit) <<< const

bracketReact
  :: forall t a r
   . Event t a
  -> Effect r
  -> (r -> Effect Unit)
  -> (r -> a -> Effect Unit)
  -> Raff t (Effect Unit)
bracketReact (Event e) acquire release fire = Raff $ ReaderT \_ ->
  _react e \ma -> do
    r <- acquire
    catchError (traverse_ (fire r) ma) \err -> do
      release r
      throwError err
    pure { fire: fire r, dispose: release r }

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

alignPush
  :: forall t a b c
   . (These a b -> RaffPush t (Maybe c))
  -> Event t a
  -> Event t b
  -> Event t c
alignPush f (Event a) (Event b) =
  Event $ Internal._push (coerce f) $ Internal._alignE a b

accumE
  :: forall t a b
   . (b -> a -> b)
  -> b
  -> Event t a
  -> Raff t (Event t b)
accumE f = accumMaybeE \b a -> Just $ f b a

accumME
  :: forall t a b
   . (b -> a -> RaffPush t b)
  -> b
  -> Event t a
  -> Raff t (Event t b)
accumME f = accumMaybeME \b a -> Just <$> f b a

accumMaybeE
  :: forall t a b
   . (b -> a -> Maybe b)
  -> b
  -> Event t a
  -> Raff t (Event t b)
accumMaybeE f = accumMaybeME \b a -> pure $ f b a

accumMaybeME
  :: forall t a b
   . (b -> a -> RaffPush t (Maybe b))
  -> b
  -> Event t a
  -> Raff t (Event t b)
accumMaybeME f seed ea = mfix \eb -> do
  bb <- stepper seed eb
  pure $ flip push ea \a -> do
    b <- sample bb
    f b a

indexed :: forall t a n. Semiring n => Event t a -> Raff t (Event t (Tuple n a))
indexed = indexedFrom zero

indexed_
  :: forall t a n. Semiring n => Event t a -> Raff t (Event t n)
indexed_ = indexedFrom_ zero

indexedFrom
  :: forall t a n. Semiring n => n -> Event t a -> Raff t (Event t (Tuple n a))
indexedFrom = mapAccum_ \n a -> { accum: n + one, value: Tuple n a }

indexedFrom_
  :: forall t a n. Semiring n => n -> Event t a -> Raff t (Event t n)
indexedFrom_ = mapAccum_ \n _ -> { accum: n + one, value: n }

mapAccum_
  :: forall t a b c
   . (b -> a -> Accum b c)
  -> b
  -> Event t a
  -> Raff t (Event t c)
mapAccum_ f b e = _.value <$> mapAccumB f b e

mapAccumM_
  :: forall t a b c
   . (b -> a -> RaffPush t (Accum b c))
  -> b
  -> Event t a
  -> Raff t (Event t c)
mapAccumM_ f b e = _.value <$> mapAccumMB f b e

mapAccumMaybe_
  :: forall t a b c
   . (b -> a -> Accum (Maybe b) (Maybe c))
  -> b
  -> Event t a
  -> Raff t (Event t c)
mapAccumMaybe_ f b e = _.value <$> mapAccumMaybeB f b e

mapAccumMaybeM_
  :: forall t a b c
   . (b -> a -> RaffPush t (Accum (Maybe b) (Maybe c)))
  -> b
  -> Event t a
  -> Raff t (Event t c)
mapAccumMaybeM_ f b e = _.value <$> mapAccumMaybeMB f b e

-------------------------------------------------------------------------------
-- Behaviours
-------------------------------------------------------------------------------

newtype Behaviour (t :: Timeline) a = Behaviour (Internal.Behaviour a)

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
pull = coerce (_pull :: PullM a -> Internal.Behaviour a)

pullContinuous :: forall t a. RaffPull t a -> Behaviour t a
pullContinuous (RaffPull m) = pull $ RaffPull $ tellHint PullContinuous *> m

patcher
  :: forall patch t a
   . Patch patch a
  => a
  -> Event t patch
  -> Raff t (Behaviour t a)
patcher i (Event e) = Raff $ ReaderT \_ -> do
  latch <- newLatch i e
  pure $ Behaviour $ latchBehaviour latch

stepper :: forall t a. a -> Event t a -> Raff t (Behaviour t a)
stepper i =
  coerce
    (patcher (Identity i) :: Event t a -> Raff t (Behaviour t (Identity a)))

switcher
  :: forall t a
   . Behaviour t a
  -> Event t (Behaviour t a)
  -> Raff t (Behaviour t a)
switcher i e = join <$> stepper i e

sample :: forall t a m. MonadPull t m => Behaviour t a -> m a
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
  :: forall t a b
   . (b -> a -> b)
  -> b
  -> Event t a
  -> Raff t (Behaviour t b)
accumB f = accumMaybeB \b a -> Just $ f b a

accumMB
  :: forall t a b
   . (b -> a -> RaffPush t b)
  -> b
  -> Event t a
  -> Raff t (Behaviour t b)
accumMB f = accumMaybeMB \b a -> Just <$> f b a

accumMaybeB
  :: forall t a b
   . (b -> a -> Maybe b)
  -> b
  -> Event t a
  -> Raff t (Behaviour t b)
accumMaybeB f = accumMaybeMB \b a -> pure $ f b a

accumMaybeMB
  :: forall t a b
   . (b -> a -> RaffPush t (Maybe b))
  -> b
  -> Event t a
  -> Raff t (Behaviour t b)
accumMaybeMB f seed ea = mfix \bb -> do
  let
    eb = flip push ea \a -> do
      b <- sample bb
      f b a
  stepper seed eb

mapAccumB
  :: forall t a b c
   . (b -> a -> Accum b c)
  -> b
  -> Event t a
  -> Raff t (Accum (Behaviour t b) (Event t c))
mapAccumB f = mapAccumMaybeB \b a ->
  let
    { accum, value } = f b a
  in
    { accum: Just accum, value: Just value }

mapAccumMB
  :: forall t a b c
   . (b -> a -> RaffPush t (Accum b c))
  -> b
  -> Event t a
  -> Raff t (Accum (Behaviour t b) (Event t c))
mapAccumMB f = mapAccumMaybeMB \b a -> do
  { accum, value } <- f b a
  pure { accum: Just accum, value: Just value }

mapAccumMaybeB
  :: forall t a b c
   . (b -> a -> Accum (Maybe b) (Maybe c))
  -> b
  -> Event t a
  -> Raff t (Accum (Behaviour t b) (Event t c))
mapAccumMaybeB f = mapAccumMaybeMB \b a -> pure $ f b a

mapAccumMaybeMB
  :: forall t a b c
   . (b -> a -> RaffPush t (Accum (Maybe b) (Maybe c)))
  -> b
  -> Event t a
  -> Raff t (Accum (Behaviour t b) (Event t c))
mapAccumMaybeMB f seed ea = do
  Tuple eaccum accum <- mfix2 \_ accum -> do
    let
      eaccum = flip push ea \a -> do
        b <- sample accum
        result <- f b a
        pure case result of
          { accum: Nothing, value: Nothing } -> Nothing
          _ -> Just result
    accum' <- stepper seed $ filterMap _.accum eaccum
    pure $ Tuple eaccum accum'
  pure { accum, value: filterMap _.value eaccum }

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
  triggerQueue <- Q.new
  liftEffect $ Internal.interpret2
    ( \e1 e2 -> do
        Event e3 <- case f (Event e1) (Event e2) of
          Raff r -> runReaderT r { triggerQueue }
        pure e3
    )
    as
    bs
