module Effect.Reactive.Internal.Animation where

import Prelude

import Control.Monad.Fix (mfix)
import Data.Lazy (force)
import Effect (Effect)
import Effect.RW (evalRWEffect)
import Effect.Reactive.Internal
  ( Animation
  , BehaviourRep(..)
  , BuildM(..)
  , PullSubscriber(..)
  , evalTimeFunc
  )
import Effect.Reader (ReaderEffect(..))

type Sampler = Effect

type InitializeAnimation a = Sampler a -> Effect Animation

animate
  :: forall a. BehaviourRep a -> InitializeAnimation a -> BuildM (Effect Unit)
animate (B tf) initialize = BM $ RE \env -> do
  _.dispose <<< force <$> mfix \animation -> do
    pure <$> initialize do
      time <- env.getTime
      evalRWEffect (evalTimeFunc tf time) $
        { time
        , subscriber: AnimationSubscriber 0 animation
        }
