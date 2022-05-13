module Effect.Ref.Maybe
  ( MaybeRef
  , clear
  , empty
  , filled
  , isFilled
  , modify
  , modify'
  , modify_
  , new
  , newWithSelf
  , read
  , write
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

-- | A value of type `MaybeRef a` represents a mutable reference
-- | which might hold a value of type `a`, or nothing.
foreign import data MaybeRef :: Type -> Type

type role MaybeRef representational

foreign import _new :: forall s. Maybe s -> Effect (MaybeRef s)

-- | Create a new mutable reference containing the specified Maybe value.
new :: forall s. Maybe s -> Effect (MaybeRef s)
new = _new

-- | Create a new empty mutable reference.
empty :: forall s. Effect (MaybeRef s)
empty = new Nothing

-- | Create a new mutable reference containing the specified value.
filled :: forall s. s -> Effect (MaybeRef s)
filled = new <<< Just

-- | Create a new mutable reference containing a value that can refer to the
-- | `MaybeRef` being created.
foreign import newWithSelf
  :: forall s. (MaybeRef s -> Maybe s) -> Effect (MaybeRef s)

foreign import _read
  :: forall s
   . FFIUtil s
  -> MaybeRef s
  -> Effect (Maybe s)

-- | Read the current value of a mutable reference.
read :: forall s. MaybeRef s -> Effect (Maybe s)
read = _read { just: Just, nothing: Nothing }

foreign import modifyImpl
  :: forall s b
   . (Maybe s -> { state :: Maybe s, value :: b })
  -> MaybeRef s
  -> Effect b

-- | Update the value of a mutable reference by applying a function
-- | to the current value.
modify'
  :: forall s b
   . (Maybe s -> { state :: Maybe s, value :: b })
  -> MaybeRef s
  -> Effect b
modify' = modifyImpl

-- | Update the value of a mutable reference by applying a function
-- | to the current value. The updated value is returned.
modify :: forall s. (Maybe s -> Maybe s) -> MaybeRef s -> Effect (Maybe s)
modify f = modify' \s -> let s' = f s in { state: s', value: s' }

-- | A version of `modify` which does not return the updated value.
modify_ :: forall s. (Maybe s -> Maybe s) -> MaybeRef s -> Effect Unit
modify_ f s = void $ modify f s

-- | Update the value of a mutable reference to the specified value.
foreign import write :: forall s. s -> MaybeRef s -> Effect Unit

-- | Clear the value of a mutable reference.
foreign import clear :: forall s. MaybeRef s -> Effect Unit

-- | Returns true if the ref is filled, false if empty.
foreign import isFilled :: forall s. MaybeRef s -> Effect Boolean

type FFIUtil a =
  { just :: a -> Maybe a
  , nothing :: Maybe a
  }
