module Effect.Reactive.Internal.Pipe where

import Prelude

import Control.Monad.Reader (asks)
import Control.Monad.Writer (tell)
import Data.Exists (mkExists)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.WeakBag as WeakBag
import Effect.Class (liftEffect)
import Effect.RW (evalRWEffect, runRWEffect)
import Effect.Reactive.Internal
  ( BehaviourRep
  , Pipe(..)
  , PullM
  , PullSubscriber(..)
  , trackSubscriber
  )
import Effect.Ref.Maybe as RM
import Effect.Unsafe (unsafePerformEffect)

_pull :: PullM ~> BehaviourRep
_pull evaluate = unsafePerformEffect do
  cache <- RM.empty
  pure $ pipeBehaviour $ Pipe { evaluate, cache }

pipeBehaviour :: Pipe ~> BehaviourRep
pipeBehaviour p@(Pipe pipe) = do
  mCache <- liftEffect $ RM.read pipe.cache
  case mCache of
    Just cache -> do
      trackSubscriber cache.subscribers
      cache.getValue
    Nothing -> do
      time <- asks _.time
      let subscriber = PipeSubscriber $ mkExists p
      Tuple { canceller, isContinuous } value <- liftEffect
        $ runRWEffect pipe.evaluate
        $ { time, subscriber }
      subscribers <- liftEffect $ WeakBag.new canceller
      let
        getValue
          | unwrap isContinuous = do
              tell { isContinuous, canceller: mempty }
              liftEffect $ evalRWEffect pipe.evaluate { time, subscriber }
          | otherwise = pure value
      let cache = { subscribers, getValue }
      liftEffect $ RM.write cache pipe.cache
      trackSubscriber cache.subscribers
      pure value
