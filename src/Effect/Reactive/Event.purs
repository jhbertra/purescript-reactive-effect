module Effect.Reactive.Event (Event, EventIO, newEvent, sink) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( ExistsNode
  , Raff
  , addChild
  , cached
  , fire
  , mkExistsNode
  , newInput
  , newOutput
  , newProcess
  , removeChild
  , runExistsNode
  )
import Safe.Coerce (coerce)

newtype Event t a = Event (Raff t (ExistsNode t a))
type EventIO t a = { event :: Event t a, fire :: a -> Raff t Unit }

mkEvent :: forall t a. Raff t (ExistsNode t a) -> Event t a
mkEvent = coerce (cached :: Raff t (ExistsNode t a) -> _ _ _)

instance Functor (Event t) where
  map f (Event ra) = mkEvent do
    existsNode <- ra
    runExistsNode existsNode \nodeA -> do
      nodeB <- newProcess \_ a write -> write $ f $ a
      nodeA `addChild` nodeB
      pure $ mkExistsNode nodeB

newEvent :: forall t a. Raff t (EventIO t a)
newEvent = do
  input <- newInput
  pure
    { event: mkEvent $ pure $ mkExistsNode input
    , fire: \a -> fire a input
    }

sink :: forall t a. Event t a -> (a -> Effect Unit) -> Raff t (Raff t Unit)
sink (Event ra) eff = do
  output <- newOutput $ liftEffect <<< eff
  existsNode <- ra
  runExistsNode existsNode \nodeA -> do
    nodeA `addChild` output
    pure $ nodeA `removeChild` output
