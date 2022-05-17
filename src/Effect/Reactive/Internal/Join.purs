module Effect.Reactive.Internal.Join where

import Prelude

import Control.Monad.Fix (mfix)
import Data.Foldable (for_)
import Data.Lazy (force)
import Data.Lazy as DL
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.WeakBag (WeakBag)
import Data.WeakBag as WeakBag
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( CacheResult
  , Clear(..)
  , EventRep
  , EventResult
  , EventSubscriber
  , Join
  , JoinCache(..)
  , PropagateM
  , _subscribe
  , clearLater
  , resetJoin
  , wrapSubscribeCached
  , writeNowClearLater
  )
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Ref.Maybe (MaybeRef)
import Effect.Ref.Maybe as RM

joinEvent :: forall a. Join a -> EventRep a
joinEvent = wrapSubscribeCached cacheDepth <<< subscribeJoinCache

subscribeJoinCache
  :: forall a
   . Join a
  -> EventSubscriber a
  -> PropagateM (CacheResult (JoinCache a) a)
subscribeJoinCache j subscriber = do
  mCache <- liftEffect $ RM.read j.cache
  cache <- case mCache of
    Just cache -> pure cache
    Nothing -> force <$> mfix \cache -> do
      subscribers <- liftEffect $ WeakBag.new $ cleanup j.cache
      let outerSubscriber = joinSubscriberOuter cache
      { subscription, occurrence } <- _subscribe j.parent outerSubscriber
      outerDepth <- liftEffect $ Ref.read subscription.depth
      Tuple mInnerResult depth <- case occurrence of
        Nothing -> pure $ Tuple Nothing outerDepth
        Just inner -> do
          Tuple innerResult depth <- subscribeJoinInner inner outerDepth cache
          pure $ Tuple (Just innerResult) depth
      occurrenceRef <- liftEffect $ RM.new $ _.occurrence =<< mInnerResult
      when (isJust occurrence) $ clearLater $ MaybeRefClear occurrenceRef
      innerSubscription <- liftEffect $ RM.new $ _.subscription <$> mInnerResult
      when (isJust occurrence) $ clearLater $ MaybeRefClear innerSubscription
      liftEffect do
        depthRef <- Ref.new depth
        let
          c = JoinCache
            { outerSubscriber
            , outerSubscription: subscription
            , innerSubscription
            , occurrence: occurrenceRef
            , depth: depthRef
            , subscribers
            }
        RM.write c j.cache
        pure $ pure c
  liftEffect do
    ticket <- WeakBag.insert subscriber $ cacheSubscribers cache
    occurrence <- RM.read $ cacheOccurrence cache
    pure { ticket, cache, occurrence }
  where
  cleanup cacheRef = do
    mCache <- RM.read cacheRef
    for_ mCache \(JoinCache cache) -> do
      cache.outerSubscription.unsubscribe
      RM.clear cacheRef

joinSubscriberOuter
  :: forall a. DL.Lazy (JoinCache a) -> EventSubscriber (EventRep a)
joinSubscriberOuter lcache =
  { propagate: \inner -> do
      let JoinCache cache = force lcache
      outerDepth <- liftEffect $ Ref.read cache.depth
      Tuple { occurrence, subscription } depth <-
        subscribeJoinInner inner outerDepth lcache
      writeNowClearLater subscription cache.innerSubscription
      case occurrence of
        Nothing -> liftEffect do
          -- The inner event may fire later in the frame
          when (depth > outerDepth) do
            Ref.write depth cache.depth
            WeakBag.traverseMembers_
              (\s -> s.recalculateDepth depth)
              cache.subscribers
        Just a -> do
          writeNowClearLater a cache.occurrence
          WeakBag.traverseMembers_
            (\s -> s.propagate a)
            cache.subscribers
  , recalculateDepth: \_ -> recalculateJoinDepth $ force lcache
  }

subscribeJoinInner
  :: forall a
   . EventRep a
  -> Int
  -> DL.Lazy (JoinCache a)
  -> PropagateM (Tuple (EventResult a) Int)
subscribeJoinInner inner outerDepth lcache = do
  result@{ subscription } <- _subscribe inner $ joinSubscriberInner lcache
  innerDepth <- liftEffect $ Ref.read subscription.depth
  let depth = max outerDepth innerDepth
  resetJoin subscription if depth > outerDepth then Just lcache else Nothing
  pure $ Tuple result depth

joinSubscriberInner :: forall a. DL.Lazy (JoinCache a) -> EventSubscriber a
joinSubscriberInner lcache =
  { propagate: \a -> do
      let JoinCache cache = force lcache
      occurrence <- liftEffect $ RM.read cache.occurrence
      case occurrence of
        Just _ -> pure unit
        Nothing -> do
          writeNowClearLater a cache.occurrence
          WeakBag.traverseMembers_ (\s -> s.propagate a) cache.subscribers
  , recalculateDepth: \_ -> recalculateJoinDepth $ force lcache
  }

recalculateJoinDepth :: forall a. JoinCache a -> Effect Unit
recalculateJoinDepth (JoinCache cache) = do
  outerDepth <- Ref.read cache.outerSubscription.depth
  mInnerDepth <-
    traverse (Ref.read <<< _.depth) =<< RM.read cache.innerSubscription
  let
    newDepth = case mInnerDepth of
      Nothing -> outerDepth
      Just innerDepth -> max outerDepth innerDepth
  Ref.write newDepth cache.depth
  WeakBag.traverseMembers_
    (\s -> s.recalculateDepth newDepth)
    cache.subscribers

cacheSubscribers :: forall a. JoinCache a -> WeakBag (EventSubscriber a)
cacheSubscribers (JoinCache c) = c.subscribers

cacheOccurrence :: forall a. JoinCache a -> MaybeRef a
cacheOccurrence (JoinCache c) = c.occurrence

cacheDepth :: forall a. JoinCache a -> Ref Int
cacheDepth (JoinCache c) = c.depth
