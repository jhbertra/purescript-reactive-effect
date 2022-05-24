module Effect.Reactive.Internal.Pipe where

import Prelude

import Control.Monad.Reader (asks)
import Data.Exists (mkExists)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.WeakBag as WeakBag
import Effect.Class (liftEffect)
import Effect.RW (runRWEffect)
import Effect.Reactive.Internal
  ( BehaviourRep(..)
  , Pipe(..)
  , PullM
  , PullSubscriber(..)
  , TimeFunc(..)
  , trackSubscriber
  )
import Effect.Ref.Maybe as RM
import Effect.Unsafe (unsafePerformEffect)

_pull :: PullM ~> BehaviourRep
_pull evaluate = unsafePerformEffect do
  cache <- RM.empty
  pure $ pipeBehaviour $ Pipe { evaluate, cache }

pipeBehaviour :: Pipe ~> BehaviourRep
pipeBehaviour p@(Pipe pipe) = B $ K do
  mCache <- liftEffect $ RM.read pipe.cache
  cache <- case mCache of
    Just cache -> pure cache
    Nothing -> do
      time <- asks _.time
      Tuple canceller value <- liftEffect
        $ runRWEffect pipe.evaluate
        $ { time, subscriber: PipeSubscriber $ mkExists p }
      subscribers <- liftEffect $ WeakBag.new canceller
      let cache = { subscribers, value }
      liftEffect $ RM.write cache pipe.cache
      pure cache
  trackSubscriber cache.subscribers
  pure cache.value
