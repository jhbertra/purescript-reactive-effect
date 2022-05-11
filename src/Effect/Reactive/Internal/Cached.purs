module Effect.Reactive.Internal.Cached where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.WeakBag (WeakBag)
import Data.WeakBag as WeakBag
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( Event
  , EventSubscriber
  , EventSubscription
  , subscribeAndRead
  , writeNowClearLater
  )
import Effect.Ref.Maybe (MaybeRef)
import Effect.Ref.Maybe as RM
import Effect.Unsafe (unsafePerformEffect)

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
    liftEffect do
      ticket <- WeakBag.insert subscriber cache.subscribers
      occurrence <- RM.read cache.occurrence
      pure
        { occurrence
        , subscription:
            { depth: cache.parent.depth
            , unsubscribe: WeakBag.destroyTicket ticket
            }
        }
