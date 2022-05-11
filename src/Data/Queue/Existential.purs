module Data.Queue.Existential
  ( ExistentialQueue
  , new
  , enqueue
  , dequeue
  , drain
  , toArray
  ) where

import Prelude

import Data.Exists (Exists)
import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import data ExistentialQueue :: (Type -> Type) -> Type

foreign import new :: forall f. Effect (ExistentialQueue f)

foreign import enqueue :: forall f a. f a -> ExistentialQueue f -> Effect Unit

foreign import toArray
  :: forall f. ExistentialQueue f -> Effect (Array (Exists f))

foreign import _dequeue
  :: forall f r
   . { just :: forall a. a -> Maybe a, nothing :: forall a. Maybe a }
  -> ExistentialQueue f
  -> (forall a. Maybe (f a) -> r)
  -> Effect r

foreign import drain
  :: forall f
   . ExistentialQueue f
  -> (forall a. f a -> Effect Unit)
  -> Effect Unit

dequeue
  :: forall f r
   . ExistentialQueue f
  -> (forall a. Maybe (f a) -> r)
  -> Effect r
dequeue = _dequeue { just: Just, nothing: Nothing }
