module Effect.Reactive.Internal.Input where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.WeakBag (WeakBag)
import Data.WeakBag as WeakBag
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( CacheResult
  , Event
  , EventSubscriber
  , InitializeInput
  , Input
  , InputCache(..)
  , InputTrigger
  , wrapSubscribeCached
  , zeroDepth
  )
import Effect.Ref.Maybe (MaybeRef)
import Effect.Ref.Maybe as RM

cacheSubscribers :: forall a. InputCache a -> WeakBag (EventSubscriber a)
cacheSubscribers (InputCache c) = c.subscribers

cacheInput :: forall a. InputCache a -> Input a
cacheInput (InputCache c) = c.input

cacheFinalize :: forall a. InputCache a -> Effect Unit
cacheFinalize (InputCache c) = c.finalize

newInput :: forall a. InitializeInput a -> Effect (Input a)
newInput initialize = do
  cache <- RM.empty
  occurrence <- RM.empty
  pure { cache, occurrence, initialize }

newInputWithTriggerRef
  :: forall a
   . Effect { input :: Input a, trigger :: MaybeRef (InputTrigger a) }
newInputWithTriggerRef = do
  trigger <- RM.empty
  input <- newInput \t -> do
    RM.write t trigger
    pure $ RM.clear trigger
  pure { input, trigger }

subscribeInputCached
  :: forall a
   . Input a
  -> EventSubscriber a
  -> Effect (CacheResult (InputCache a) a)
subscribeInputCached input subscriber = do
  mCache <- RM.read input.cache
  cache <- case mCache of
    Just cache -> pure cache
    Nothing -> do
      subscribers <- WeakBag.new $ cleanup input.cache
      finalize <-
        input.initialize $ { subscribers, occurrence: input.occurrence }
      let cache = InputCache { subscribers, input, finalize }
      RM.write cache input.cache
      pure cache
  ticket <- WeakBag.insert subscriber $ cacheSubscribers cache
  occurrence <- RM.read input.occurrence
  pure { ticket, cache, occurrence }
  where
  cleanup cacheRef = do
    mCache <- RM.read cacheRef
    for_ mCache \(InputCache cache) -> do
      cache.finalize
      RM.clear cache.input.cache

inputEvent :: forall a. Input a -> Event a
inputEvent input = wrapSubscribeCached (const zeroDepth)
  $ liftEffect <<< subscribeInputCached input
