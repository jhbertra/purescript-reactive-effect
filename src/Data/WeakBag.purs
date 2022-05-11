module Data.WeakBag where

import Prelude

import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import data WeakBag :: Type -> Type
foreign import data WeakBagTicket :: Type -> Type

foreign import new :: forall a. Effect Unit -> Effect (WeakBag a)
foreign import insert :: forall a. a -> WeakBag a -> Effect (WeakBagTicket a)

foreign import get :: forall a. WeakBagTicket a -> WeakBag a -> Effect a
foreign import destroyTicket :: forall a. WeakBagTicket a -> Effect Unit

foreign import members :: forall a. WeakBag a -> Effect (Array a)

traverseMembers
  :: forall m a b. MonadEffect m => (a -> m b) -> WeakBag a -> m (Array b)
traverseMembers f bag = do
  as <- liftEffect $ members bag
  traverse f as

traverseMembers_
  :: forall m a b. MonadEffect m => (a -> m b) -> WeakBag a -> m Unit
traverseMembers_ f bag = do
  as <- liftEffect $ members bag
  traverse_ f as
