module Data.Queue.Priority (PriorityQueue, new, enqueue, dequeue, drain) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import data PriorityQueue :: Type -> Type

foreign import _new
  :: forall a
   . { just :: a -> Maybe a, nothing :: Maybe a }
  -> Effect (PriorityQueue a)

foreign import enqueue :: forall a. Int -> a -> PriorityQueue a -> Effect Unit

foreign import dequeue
  :: forall a. PriorityQueue a -> Effect (Maybe { priority :: Int, value :: a })

foreign import drain
  :: forall a
   . PriorityQueue a
  -> (Int -> a -> Effect Unit)
  -> Effect Unit

new :: forall a. Effect (PriorityQueue a)
new = _new { just: Just, nothing: Nothing }
