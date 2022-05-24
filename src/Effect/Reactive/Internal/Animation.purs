module Effect.Reactive.Internal.Animation where

import Prelude

import Control.Monad.Fix (mfix)
import Data.Lazy (force)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.RW (evalRWEffect)
import Effect.Reactive.Internal
  ( Animation
  , BehaviourRep(..)
  , PullEnv
  , PullSubscriber(..)
  , Time
  , TimeFunc(..)
  , evalTimeFunc
  )

type Sampler a = Time -> Effect (Tuple Boolean a)

type InitializeAnimation a = Sampler a -> Effect Animation

newAnimation
  :: forall a. BehaviourRep a -> InitializeAnimation a -> Effect Animation
newAnimation (B tf) initialize = do
  animation <- mfix \animation -> do
    let
      subscriber :: PullSubscriber
      subscriber = AnimationSubscriber animation

      sampler :: Sampler a
      sampler time = do
        let
          pullEnv :: PullEnv
          pullEnv = { time, subscriber }

          poll :: forall x. TimeFunc x -> Boolean
          poll = case _ of
            K _ -> false
            F _ -> true
            L tf' -> poll $ force tf'

        Tuple (poll tf) <$> evalRWEffect (evalTimeFunc tf time) pullEnv
    pure <$> initialize sampler
  pure $ force animation
