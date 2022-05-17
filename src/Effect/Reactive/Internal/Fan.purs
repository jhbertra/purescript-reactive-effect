module Effect.Reactive.Internal.Fan where

import Prelude

import Control.Monad.Fix (mfix)
import Data.Foldable (for_, sequence_, traverse_)
import Data.Lazy (force)
import Data.Lazy as DL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.WeakBag (WeakBag)
import Data.WeakBag as WeakBag
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( CacheResult
  , Clear(..)
  , EventRep
  , EventSubscriber
  , MapFan
  , MapFanCache(..)
  , PropagateM
  , _subscribe
  , clearLater
  , wrapSubscribeCached
  , writeNowClearLater
  )
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Ref.Maybe (MaybeRef)
import Effect.Ref.Maybe as RM

mapFanEvent :: forall k v. Ord k => k -> MapFan k v -> EventRep v
mapFanEvent k fan = wrapSubscribeCached cacheDepth $ subscribeMapFanCache k fan

subscribeMapFanCache
  :: forall k v
   . Ord k
  => k
  -> MapFan k v
  -> EventSubscriber v
  -> PropagateM (CacheResult (MapFanCache k v) v)
subscribeMapFanCache k fan subscriber = do
  mCache <- liftEffect $ RM.read fan.cache
  cache <- case mCache of
    Just cache -> pure cache
    Nothing -> force <$> mfix \cache -> do
      subscribersForKey <-
        liftEffect $ WeakBag.new $ cleanupMapFanCache k fan.cache
      subscribers <- liftEffect $ Ref.new $ Map.singleton k subscribersForKey
      { subscription, occurrence } <-
        _subscribe fan.parent $ mapFanSubscriber cache
      occurrenceRef <- liftEffect $ RM.new occurrence
      when (isJust occurrence)
        $ clearLater
        $ MaybeRefClear occurrenceRef
      liftEffect do
        let
          c = MapFanCache
            { occurrence: occurrenceRef
            , subscribers
            , subscription
            }
        RM.write c fan.cache
        pure $ pure c
  liftEffect do
    ticket <- WeakBag.insert subscriber =<< cacheSubscribers k fan.cache cache
    occurrence <- RM.read $ cacheOccurrence cache
    pure { ticket, cache, occurrence: Map.lookup k =<< occurrence }

cleanupMapFanCache
  :: forall k v. Ord k => k -> MaybeRef (MapFanCache k v) -> Effect Unit
cleanupMapFanCache k cacheRef = do
  mCache <- RM.read cacheRef
  for_ mCache \(MapFanCache cache) -> do
    subscribers <- Ref.modify (Map.delete k) cache.subscribers
    when (Map.isEmpty subscribers) do
      cache.subscription.unsubscribe
      RM.clear cacheRef

mapFanSubscriber
  :: forall k v. Ord k => DL.Lazy (MapFanCache k v) -> EventSubscriber (Map k v)
mapFanSubscriber lcache =
  { propagate: \a -> do
      let MapFanCache cache = force lcache
      writeNowClearLater a cache.occurrence
      subscribers <- liftEffect $ Ref.read cache.subscribers
      sequence_ $ Map.intersectionWith
        (\value -> WeakBag.traverseMembers_ (\s -> s.propagate value))
        a
        subscribers
  , recalculateDepth: \depth -> do
      let MapFanCache cache = force lcache
      subscribers <- liftEffect $ Ref.read cache.subscribers
      traverse_
        (WeakBag.traverseMembers_ (\s -> s.recalculateDepth depth))
        subscribers
  }

cacheDepth :: forall k v. MapFanCache k v -> Ref Int
cacheDepth (MapFanCache cache) = cache.subscription.depth

cacheOccurrence :: forall k v. MapFanCache k v -> MaybeRef (Map k v)
cacheOccurrence (MapFanCache c) = c.occurrence

cacheSubscribers
  :: forall k v
   . Ord k
  => k
  -> MaybeRef (MapFanCache k v)
  -> MapFanCache k v
  -> Effect (WeakBag (EventSubscriber v))
cacheSubscribers k cacheRef (MapFanCache cache) = do
  subscribers <- Ref.read cache.subscribers
  case Map.lookup k subscribers of
    Nothing -> do
      subscribersForKey <- WeakBag.new $ cleanupMapFanCache k cacheRef
      Ref.write (Map.insert k subscribersForKey subscribers) cache.subscribers
      pure subscribersForKey
    Just subscribersForKey -> pure subscribersForKey
