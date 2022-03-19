module Control.Reactive.Class where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Fix (class MonadFix)
import Control.Reactive.Behaviour (class Behaviour)
import Control.Reactive.Event (class Event)
import Data.Maybe (Maybe)

class
  ( MonadFix m
  , Event event
  , Behaviour event behaviour
  ) <=
  Reactive event behaviour m
  | m -> event behaviour where
  accumE :: forall a. a -> event (a -> a) -> m (event a)
  hold :: forall a. a -> event a -> m (behaviour a)
  sample :: forall a. behaviour a -> m a
  sampleLazy :: forall a. Lazy a => behaviour a -> m a
  observe :: forall a. event (m a) -> event a
  switchE :: forall a. event (event a) -> m (event a)
  switchB :: forall a. behaviour a -> event (behaviour a) -> m (behaviour a)
  interpret
    :: forall a b
     . (event a -> m (event b))
    -> Array (Maybe a)
    -> m (Array (Maybe b))

accumB
  :: forall event behaviour m a
   . Reactive event behaviour m
  => a
  -> event (a -> a)
  -> m (behaviour a)
accumB acc = hold acc <=< accumE acc
