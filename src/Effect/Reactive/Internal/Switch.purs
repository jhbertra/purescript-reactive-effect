module Effect.Reactive.Internal.Switch where

import Prelude

import Control.Monad.Fix (mfix)
import Data.Exists (mkExists)
import Data.Foldable (for_)
import Data.Lazy (force)
import Data.Lazy as DL
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Data.WeakBag (WeakBag)
import Data.WeakBag as WeakBag
import Effect.Class (liftEffect)
import Effect.RW (runRWEffect)
import Effect.Reactive.Internal
  ( BehaviourSubscriber(..)
  , CacheResult
  , Clear(..)
  , EventRep
  , EventSubscriber
  , PropagateM
  , Switch
  , SwitchCache(..)
  , _subscribe
  , clearLater
  , wrapSubscribeCached
  , writeNowClearLater
  )
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Ref.Maybe (MaybeRef)
import Effect.Ref.Maybe as RM

switchEvent :: forall a. Switch a -> EventRep a
switchEvent = wrapSubscribeCached cacheDepth <<< subscribeSwitchCache

subscribeSwitchCache
  :: forall a
   . Switch a
  -> EventSubscriber a
  -> PropagateM (CacheResult (SwitchCache a) a)
subscribeSwitchCache switch subscriber = do
  mCache <- liftEffect $ RM.read switch.cache
  cache <- case mCache of
    Just cache -> pure cache
    Nothing -> force <$> mfix \cache -> do
      subscribers <- liftEffect $ WeakBag.new $ cleanup switch.cache
      Tuple pullSubscription e <- liftEffect
        $ runRWEffect switch.parent
        $ Just
        $ mkExists
        $ SwitchSubscriber cache
      { subscription, occurrence } <- _subscribe e $ switchSubscriber cache
      occurrenceRef <- liftEffect $ RM.new occurrence
      when (isJust occurrence)
        $ clearLater
        $ MaybeRefClear occurrenceRef
      liftEffect do
        currentParent <- Ref.new subscription
        currentDepth <- Ref.read subscription.depth
        depth <- Ref.new currentDepth
        pullSubscriptionRef <- Ref.new pullSubscription
        let
          c = SwitchCache
            { currentParent
            , occurrence: occurrenceRef
            , depth
            , subscribers
            , subscription: pullSubscriptionRef
            , parent: switch.parent
            }
        RM.write c switch.cache
        pure $ pure c
  liftEffect do
    ticket <- WeakBag.insert subscriber $ cacheSubscribers cache
    occurrence <- RM.read $ cacheOccurrence cache
    pure { ticket, cache, occurrence }
  where
  cleanup cacheRef = do
    mCache <- RM.read cacheRef
    for_ mCache \(SwitchCache cache) -> do
      subscription <- Ref.read cache.currentParent
      subscription.unsubscribe
      RM.clear cacheRef

switchSubscriber :: forall a. DL.Lazy (SwitchCache a) -> EventSubscriber a
switchSubscriber lcache =
  { propagate: \a -> do
      let SwitchCache cache = force lcache
      writeNowClearLater a cache.occurrence
      WeakBag.traverseMembers_ (\s -> s.propagate a) cache.subscribers
  , recalculateDepth: \depth -> do
      let SwitchCache cache = force lcache
      Ref.write depth cache.depth
      WeakBag.traverseMembers_
        (\s -> s.recalculateDepth depth)
        cache.subscribers
  }

cacheSubscribers :: forall a. SwitchCache a -> WeakBag (EventSubscriber a)
cacheSubscribers (SwitchCache c) = c.subscribers

cacheOccurrence :: forall a. SwitchCache a -> MaybeRef a
cacheOccurrence (SwitchCache c) = c.occurrence

cacheDepth :: forall a. SwitchCache a -> Ref Int
cacheDepth (SwitchCache c) = c.depth
