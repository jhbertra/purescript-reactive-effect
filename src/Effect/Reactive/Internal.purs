module Effect.Reactive.Internal
  ( class IsNode
  , class HasChildren
  , class HasParents
  , Scheduler
  , Node
  , NodeEval
  , InputNode
  , ProcessNode
  , OutputNode
  , LatchNode
  , Network
  , Raff
  , withNetwork
  , timeoutScheduler
  , animationFrameScheduler
  , schedule
  , runRaff
  , addParent
  , removeParent
  , addChild
  , removeChild
  , toNode
  , onConnected
  , newInput
  , newOutput
  , newLatch
  , newNode
  , actuate
  , deactivate
  , resume
  , suspend
  , fire
  , readNode
  ) where

import Prelude

import Control.Monad.Base (class MonadBase)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT(..)
  , runReaderT
  )
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Unlift (class MonadUnlift)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Reactive.Types (Timeline)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn2)
import Effect.Unlift (class MonadUnliftEffect)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Network :: Timeline -> Type

foreign import data Node :: Timeline -> Type -> Type -> Type

foreign import data ProcessNode :: Timeline -> Type -> Type -> Type

foreign import data InputNode :: Timeline -> Type -> Type

foreign import data OutputNode :: Timeline -> Type -> Type

foreign import data LatchNode :: Timeline -> Type -> Type

class IsNode t a b n | n -> t a b where
  toNode :: n -> Node t a b

instance IsNode t a b (Node t a b) where
  toNode = identity

instance IsNode t a a (InputNode t a) where
  toNode = unsafeCoerce

instance IsNode t a a (OutputNode t a) where
  toNode = unsafeCoerce

instance IsNode t a a (LatchNode t a) where
  toNode = unsafeCoerce

instance IsNode t a b (ProcessNode t a b) where
  toNode = unsafeCoerce

foreign import data Scheduler :: Type

foreign import _withNetwork
  :: forall r. EffectFn2 Scheduler (forall t. Network t -> Effect r) r

foreign import timeoutScheduler :: Scheduler

foreign import animationFrameScheduler :: Scheduler

foreign import schedule :: Scheduler -> EffectFn1 (Effect Unit) Unit

withNetwork
  :: forall r. Scheduler -> (forall t. Network t -> Effect r) -> Effect r
withNetwork = runEffectFn2 _withNetwork

newtype Raff t a = Raff (ReaderT (Network t) Effect a)

runRaff :: forall t a. Raff t a -> Network t -> Effect a
runRaff = coerce (runReaderT :: _ (Network t) Effect a -> _ _ -> _ a)

derive newtype instance Functor (Raff t)
derive newtype instance Apply (Raff t)
derive newtype instance Applicative (Raff t)
derive newtype instance Bind (Raff t)
derive newtype instance Monad (Raff t)
derive newtype instance MonadEffect (Raff t)
derive newtype instance MonadUnliftEffect (Raff t)
instance MonadBase (Raff t) (Raff t) where
  liftBase = identity

instance MonadUnlift (Raff t) (Raff t) where
  withRunInBase runAction = runAction identity

derive newtype instance MonadThrow Error (Raff t)
derive newtype instance MonadError Error (Raff t)
derive newtype instance MonadRec (Raff t)
derive newtype instance MonadAsk (Network t) (Raff t)
derive newtype instance MonadReader (Network t) (Raff t)

foreign import _addParent
  :: forall t a b c. EffectFn2 (Node t b c) (Node t a b) Unit

foreign import _removeParent
  :: forall t a b c. EffectFn2 (Node t b c) (Node t a b) Unit

foreign import _addChild
  :: forall t a b c. EffectFn2 (Node t a b) (Node t b c) Unit

foreign import _removeChild
  :: forall t a b c. EffectFn2 (Node t a b) (Node t b c) Unit

foreign import _fire :: forall t a. EffectFn2 a (InputNode t a) Unit

foreign import _onConnected
  :: forall t a b. EffectFn2 (Node t a b) (Effect (Effect Unit)) (Effect Unit)

class IsNode t x y p <= HasParents t x y p | p -> t x y where
  addParent :: forall z c. HasChildren t z x c => p -> c -> Raff t Unit
  removeParent :: forall z c. HasChildren t z x c => p -> c -> Raff t Unit

instance HasParents t x x (OutputNode t x) where
  addParent p c = liftEffect $ runEffectFn2 _addParent (toNode p) (toNode c)
  removeParent p c = liftEffect $ runEffectFn2 _removeParent (toNode p)
    (toNode c)

instance HasParents t x x (LatchNode t x) where
  addParent p c = liftEffect $ runEffectFn2 _addParent (toNode p) (toNode c)
  removeParent p c = liftEffect $ runEffectFn2 _removeParent (toNode p)
    (toNode c)

instance HasParents t x y (ProcessNode t x y) where
  addParent p c = liftEffect $ runEffectFn2 _addParent (toNode p) (toNode c)
  removeParent p c = liftEffect $ runEffectFn2 _removeParent (toNode p)
    (toNode c)

class IsNode t x y p <= HasChildren t x y p | p -> t x y where
  addChild :: forall z c. HasParents t y z c => p -> c -> Raff t Unit
  removeChild :: forall z c. HasParents t y z c => p -> c -> Raff t Unit

instance HasChildren t x x (InputNode t x) where
  addChild p c = liftEffect $ runEffectFn2 _addChild (toNode p) (toNode c)
  removeChild p c = liftEffect $ runEffectFn2 _removeChild (toNode p) (toNode c)

instance HasChildren t x x (LatchNode t x) where
  addChild p c = liftEffect $ runEffectFn2 _addChild (toNode p) (toNode c)
  removeChild p c = liftEffect $ runEffectFn2 _removeChild (toNode p) (toNode c)

instance HasChildren t x y (ProcessNode t x y) where
  addChild p c = liftEffect $ runEffectFn2 _addChild (toNode p) (toNode c)
  removeChild p c = liftEffect $ runEffectFn2 _removeChild (toNode p) (toNode c)

fire :: forall t a. a -> InputNode t a -> Raff t Unit
fire a = liftEffect <<< runEffectFn2 _fire a

onConnected
  :: forall t a b. Node t a b -> Effect (Effect Unit) -> Effect (Effect Unit)
onConnected = runEffectFn2 _onConnected

type NodeEval t a b =
  ProcessNode t a b -> a -> (b -> Raff t Unit) -> Raff t Unit

foreign import newInput :: forall t a. Raff t (InputNode t a)
foreign import newOutput
  :: forall t a. (a -> Effect Unit) -> Raff t (OutputNode t a)

foreign import newLatch :: forall t a. a -> Raff t (LatchNode t a)
foreign import newNode
  :: forall t a b. NodeEval t a b -> Raff t (ProcessNode t a b)

foreign import actuate :: forall t. Raff t Unit
foreign import deactivate :: forall t. Raff t Unit
foreign import resume :: forall t. Raff t Unit
foreign import suspend :: forall t. Raff t Unit
foreign import readLatch :: forall t a. LatchNode t a -> Raff t a
foreign import _readNode
  :: forall t a b. Fn3 (a -> Maybe a) (Maybe a) (Node t a b) (Raff t (Maybe b))

readNode :: forall t a b. Node t a b -> Raff t (Maybe b)
readNode = runFn3 _readNode Just Nothing
