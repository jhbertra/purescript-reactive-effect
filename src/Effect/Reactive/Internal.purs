module Effect.Reactive.Internal where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Fix (class MonadFix)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Monad.Writer (tell)
import Data.CatList (CatList(..))
import Data.CatList as Cat
import Data.Either (Either, either, hush)
import Data.Exists (Exists, mkExists, runExists)
import Data.Filterable (maybeBool)
import Data.Foldable (for_, traverse_)
import Data.Lazy (force)
import Data.Lazy as DL
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.OrderedBag (OrderedBag)
import Data.Queue.Existential (ExistentialQueue)
import Data.Queue.Existential as EQ
import Data.Queue.Priority (PriorityQueue)
import Data.Queue.Priority as PQ
import Data.These (These, maybeThese, thatOrBoth, thisOrBoth)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
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

type Event a = EventSubscriber a -> PropagateM (EventResult a)

type EventSubscriber a =
  { propagate :: a -> PropagateM Unit
  , recalclateDepth :: Int -> Effect Unit
  }

terminalSubscriber :: forall a. (a -> PropagateM Unit) -> EventSubscriber a
terminalSubscriber f =
  { propagate: f
  , recalclateDepth: mempty
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

type Behaviour a = PullM a

type PullM = RWEffect PullEnv PullSubscription

type PullEnv = Maybe SomePullSubscriber

data PullSource a
  = LatchSource (Latch a)
  | PipeSource (PipeCache a)

data PullSubscriber a
  = PipeSubscriber (Pipe a)
  | AnimationSubscriber Int (DL.Lazy AnimationInitialized)

data PullHint
  = PullOnce
  | PullContinuous

instance Semigroup PullHint where
  append PullContinuous _ = PullContinuous
  append _ b = b

instance Monoid PullHint where
  mempty = PullOnce

type SomePullSubscriber = Exists PullSubscriber
data PullSubscription
  = Inactive
  | Active
      { sources :: CatList (Exists PullSource)
      , hint :: PullHint
      , unsubscribe :: Effect Unit
      }

instance Semigroup PullSubscription where
  append Inactive b = b
  append a Inactive = a
  append (Active a) (Active b) = Active
    { sources: a.sources <> b.sources
    , hint: a.hint <> b.hint
    , unsubscribe: a.unsubscribe *> b.unsubscribe
    }

instance Monoid PullSubscription where
  mempty = Inactive

tellHint :: PullHint -> PullM Unit
tellHint hint = tell $ Active { hint, sources: CatNil, unsubscribe: pure unit }

tellSource :: forall a. PullSource a -> PullM Unit
tellSource source = tell $
  Active
    { hint: PullOnce
    , sources: Cat.singleton $ mkExists source
    , unsubscribe: pure unit
    }

trackSubscriber :: WeakBag (Exists PullSubscriber) -> PullM Unit
trackSubscriber weakBag = RWE \mSubscriber subscription ->
  for_ mSubscriber \subscriber -> do
    ticket <- WeakBag.insert subscriber weakBag
    Ref.modify_
      ( _ <> Active
          { hint: PullOnce
          , sources: CatNil
          , unsubscribe: WeakBag.destroyTicket ticket
          }
      )
      subscription

newtype Latch a = Latch
  { value :: Ref a
  , subscribers :: WeakBag (Exists PullSubscriber)
  , initialize :: PropagateM Unit
  }

newtype LatchUpdate a = LatchUpdate
  { valueRef :: Ref a
  , invalidateOld :: Effect Unit
  , newValue :: a
  }

type Pipe a =
  { cache :: Ref (Maybe (PipeCache a))
  , evaluate :: PullM a
  }

type PipeCache a =
  { value :: a
  , subscribers :: WeakBag (Exists PullSubscriber)
  , subscription :: PullSubscription
  }

invalidatePullSubscriber :: forall a. PullSubscriber a -> Effect Unit
invalidatePullSubscriber = case _ of
  PipeSubscriber pipe -> invalidatePipe pipe
  AnimationSubscriber _ animation -> (force animation).invalidate

invalidatePipe :: forall a. Pipe a -> Effect Unit
invalidatePipe pipe = do
  mCache <- Ref.read pipe.cache
  case mCache of
    Just { subscribers } -> do
      Ref.write Nothing pipe.cache
      WeakBag.traverseMembers_ (runExists invalidatePullSubscriber) subscribers
    _ -> pure unit

readBehaviourUntracked :: Behaviour ~> Effect
readBehaviourUntracked (RWE b) = do
  w <- Ref.new mempty
  b Nothing w

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

newtype FireTriggers = FireTriggers
  (Array (Exists InvokeTrigger) -> Aff Unit)

newtype InvokeTrigger a = InvokeTrigger
  { value :: a
  , trigger :: InputTrigger a
  }

type InputTrigger a =
  { subscribers :: WeakBag (EventSubscriber a)
  , occurrence :: MaybeRef a
  }

-------------------------------------------------------------------------------
-- Animations
-------------------------------------------------------------------------------

type AnimationInitialized =
  { dispose :: Effect Unit
  , invalidate :: Effect Unit
  }

-------------------------------------------------------------------------------
-- Merges
-------------------------------------------------------------------------------

_alignE
  :: forall a b
   . Event a
  -> Event b
  -> Event (These a b)
_alignE ea eb subscriber = do
  occurrenceA <- liftEffect RM.empty
  occurrenceB <- liftEffect RM.empty
  propagated <- liftEffect $ Ref.new false
  depthA <- liftEffect $ Ref.new invalidDepth
  depthB <- liftEffect $ Ref.new invalidDepth
  depth <- liftEffect $ Ref.new invalidDepth

  resultA <- subscribeAndRead ea
    { propagate: \a -> do
        writeNowClearLater a occurrenceA
        myDepth <- liftEffect $ Ref.read depth
        propagate myDepth $ unlessRaised propagated do
          raiseNowClearLater propagated
          mb <- liftEffect $ RM.read occurrenceB
          subscriber.propagate $ thisOrBoth a mb
    , recalclateDepth: \newDepthA -> do
        oldDepthA <- Ref.read depthA
        unless (oldDepthA == newDepthA) do
          Ref.write newDepthA depthA
          depthB' <- Ref.read depthB
          Ref.write ((max depthB' newDepthA) + 1) depth
    }

  resultB <- subscribeAndRead eb
    { propagate: \b -> do
        writeNowClearLater b occurrenceB
        myDepth <- liftEffect $ Ref.read depth
        propagate myDepth $ unlessRaised propagated do
          raiseNowClearLater propagated
          ma <- liftEffect $ RM.read occurrenceA
          subscriber.propagate $ thatOrBoth b ma
    , recalclateDepth: \newDepthB -> do
        oldDepthB <- Ref.read depthB
        unless (oldDepthB == newDepthB) do
          Ref.write newDepthB depthB
          depthA' <- Ref.read depthA
          Ref.write ((max depthA' newDepthB) + 1) depth
    }

  liftEffect do
    depthA' <- Ref.read resultA.subscription.depth
    depthB' <- Ref.read resultB.subscription.depth
    Ref.write depthA' depthA
    Ref.write depthB' depthB
    Ref.write ((max depthA' depthB') + 1) depth

  frameDepth <- currentDepth
  myDepth <- liftEffect $ Ref.read depth
  let ma = resultA.occurrence
  let mb = resultB.occurrence
  occurrence <-
    if frameDepth >= myDepth then
      -- If this event should have already fired
      -- calculate the occurrence from the results read from the parents,
      -- as they should have already propagated this frame if firing, and
      -- return it.
      pure $ maybeThese ma mb
    else do
      -- Run the initial merge later, when the depth is right. The parents
      -- may not have propagad yet, so the readings will be inaccurate
      -- until they have. We still need to write an occurrence that has
      -- occurred now.
      for_ ma \a -> writeNowClearLater a occurrenceA
      for_ mb \b -> writeNowClearLater b occurrenceB
      propagate myDepth do
        occA <- liftEffect $ RM.read occurrenceA
        occB <- liftEffect $ RM.read occurrenceB
        traverse_ subscriber.propagate $ maybeThese occA occB
      pure Nothing

  pure
    { occurrence
    , subscription:
        { depth
        , unsubscribe: do
            resultA.subscription.unsubscribe
            resultB.subscription.unsubscribe
        }
    }

-------------------------------------------------------------------------------
-- Graph nodes
-------------------------------------------------------------------------------

type CacheResult cache a =
  { ticket :: WeakBagTicket (EventSubscriber a)
  , cache :: cache
  , occurrence :: Maybe a
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
-- Combinators - Event
-------------------------------------------------------------------------------

type CachedSubscription a =
  { subscribers :: WeakBag (EventSubscriber a)
  , parent :: EventSubscription
  , occurrence :: MaybeRef a
  }

cached :: Event ~> Event
cached event = unsafePerformEffect do
  cacheRef <- RM.empty
  pure \subscriber -> do
    mCache <- liftEffect $ RM.read cacheRef
    cache <- case mCache of
      Just cache -> pure cache
      Nothing -> do
        parentSubRef <- liftEffect $ RM.empty
        occurrence <- liftEffect $ RM.empty
        subscribers <- liftEffect $ WeakBag.new do
          RM.clear cacheRef
          parentSub <- RM.read parentSubRef
          RM.clear parentSubRef
          traverse_ _.unsubscribe parentSub
        result <- subscribeAndRead event
          { propagate: \a -> do
              writeNowClearLater a occurrence
              WeakBag.traverseMembers_ (\s -> s.propagate a) subscribers
          , recalclateDepth: \depth -> WeakBag.traverseMembers_
              (\s -> s.recalclateDepth depth)
              subscribers
          }
        liftEffect $ RM.write result.subscription parentSubRef
        traverse_ (flip writeNowClearLater occurrence) result.occurrence
        let cache = { subscribers, parent: result.subscription, occurrence }
        liftEffect $ RM.write cache cacheRef
        pure cache
    ticket <- liftEffect $ WeakBag.insert subscriber cache.subscribers
    occurrence <- liftEffect $ RM.read cache.occurrence
    pure
      { occurrence
      , subscription:
          { depth: cache.parent.depth
          , unsubscribe: WeakBag.destroyTicket ticket
          }
      }

subscribeAndRead
  :: forall a. Event a -> EventSubscriber a -> PropagateM (EventResult a)
subscribeAndRead = identity

subscribe
  :: forall a. Event a -> EventSubscriber a -> PropagateM EventSubscription
subscribe e s = _.subscription <$> subscribeAndRead e s

getEventHandle :: forall a. Event a -> PropagateM (EventHandle a)
getEventHandle event = do
  currentValue <- liftEffect RM.empty
  subscription <- subscribe event $ terminalSubscriber \a ->
    writeNowClearLater a currentValue
  pure { currentValue, subscription }

_pushUncached :: forall a b. (a -> PropagateM (Maybe b)) -> Event a -> Event b
_pushUncached f e1 sub = do
  result <- subscribeAndRead e1 sub
    { propagate = traverse_ sub.propagate <=< f
    }
  occurrence <- traverse f result.occurrence
  pure result { occurrence = join occurrence }

_push :: forall a b. (a -> PropagateM (Maybe b)) -> Event a -> Event b
_push f = cached <<< _pushUncached f

_mapE :: forall a b. (a -> b) -> Event a -> Event b
_mapE f = _push $ pure <<< Just <<< f

_neverE :: forall a. Event a
_neverE = const $ pure $
  { occurrence: Nothing
  , subscription: { depth: zeroDepth, unsubscribe: mempty }
  }

_compactE :: forall a. Event (Maybe a) -> Event a
_compactE = _pushUncached pure

_separateE
  :: forall a b. Event (Either a b) -> { left :: Event a, right :: Event b }
_separateE e =
  { left: _pushUncached (pure <<< either Just (const Nothing)) e
  , right: _pushUncached (pure <<< hush) e
  }

_filterE :: forall a. (a -> Boolean) -> Event a -> Event a
_filterE p = _push $ pure <<< maybeBool p

_filterMapE :: forall a b. (a -> Maybe b) -> Event a -> Event b
_filterMapE f = _push $ pure <<< f

_partitionE
  :: forall a. (a -> Boolean) -> Event a -> { no :: Event a, yes :: Event a }
_partitionE p e =
  let
    tagged = _mapE (\a -> Tuple (p a) a) e
  in
    { no: _pushUncached (\(Tuple b a) -> pure $ a <$ guard (not b)) tagged
    , yes: _pushUncached (\(Tuple b a) -> pure $ a <$ guard b) tagged
    }

_partitionMapE
  :: forall a b c
   . (a -> Either b c)
  -> Event a
  -> { left :: Event b, right :: Event c }
_partitionMapE f = _separateE <<< _mapE f

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

unlessRaised
  :: forall m a. MonadEffect m => Monoid a => Ref Boolean -> m a -> m a
unlessRaised flag m = do
  b <- liftEffect $ Ref.read flag
  if b then pure mempty else m
