module Effect.Reactive.Internal.Animation where

import Prelude

import Control.Monad.Fix (mfix)
import Data.Bifunctor (lmap)
import Data.Lazy (force)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.RW (runRWEffect)
import Effect.Reactive.Internal
  ( Animation
  , BehaviourRep
  , PullSubscriber(..)
  , Time
  )

type Sampler a = Time -> Effect (Tuple Boolean a)

type InitializeAnimation a = Sampler a -> Effect Animation

newAnimation
  :: forall a. BehaviourRep a -> InitializeAnimation a -> Effect Animation
newAnimation m initialize = do
  animation <- mfix \animation -> do
    let
      subscriber :: PullSubscriber
      subscriber = AnimationSubscriber animation

      sampler :: Sampler a
      sampler time = do
        lmap (unwrap <<< _.isContinuous) <$> runRWEffect m { time, subscriber }
    pure <$> initialize sampler
  pure $ force animation
