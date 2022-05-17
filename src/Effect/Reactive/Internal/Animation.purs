module Effect.Reactive.Internal.Animation where

import Prelude

import Control.Monad.Fix (mfix)
import Data.Exists (mkExists)
import Data.Lazy (force)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.RW (runRWEffect)
import Effect.Reactive.Internal
  ( AnimationInitialized
  , BehaviourRep
  , BehaviourSubscriber(..)
  , BehaviourSubscription
  )

type InitializeAnimation a =
  Effect (Tuple BehaviourSubscription a)
  -> BehaviourSubscription
  -> a
  -> Effect AnimationInitialized

animate
  :: forall a. BehaviourRep a -> InitializeAnimation a -> Effect (Effect Unit)
animate behaviour initialize = do
  _.dispose <<< force <$> mfix \linitialized -> do
    let
      sampler = runRWEffect behaviour
        $ Just
        $ mkExists
        $ AnimationSubscriber 0 linitialized
    Tuple subscription a <- sampler
    initialized <- initialize sampler subscription a
    pure $ pure initialized
