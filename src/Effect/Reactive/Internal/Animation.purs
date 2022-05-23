module Effect.Reactive.Internal.Animation where

import Prelude

import Control.Monad.Fix (mfix)
import Data.Exists (mkExists)
import Data.Lazy (force)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.RW (evalRWEffect)
import Effect.Reactive.Internal
  ( Animation
  , BehaviourRep
  , BehaviourSubscriber(..)
  )

type Sampler = Effect

type InitializeAnimation a = Sampler a -> Effect Animation

animate
  :: forall a. BehaviourRep a -> InitializeAnimation a -> Effect (Effect Unit)
animate behaviour initialize = do
  _.dispose <<< force <$> mfix \lanimation -> do
    let
      sampler = evalRWEffect behaviour
        $ Just
        $ mkExists
        $ AnimationSubscriber 0 lanimation
    animation <- initialize sampler
    pure $ pure animation
