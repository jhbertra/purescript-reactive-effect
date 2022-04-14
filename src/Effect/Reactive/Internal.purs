module Effect.Reactive.Internal
  ( class IsNode
  , class HasChildren
  , class HasParents
  , class InputRow
  , class OutputRow
  , Scheduler
  , Node
  , EvalProcess
  , ExistsNode
  , MultiNode
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
  , newProcess
  , newMulti
  , actuate
  , deactivate
  , resume
  , suspend
  , fire
  , readNode
  , mkExistsNode
  , cached
  , runExistsNode
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
import Effect.Uncurried
  ( EffectFn1
  , EffectFn2
  , EffectFn4
  , runEffectFn2
  , runEffectFn4
  )
import Effect.Unlift (class MonadUnliftEffect)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Network :: Timeline -> Type

foreign import data Node :: Timeline -> Type -> Type -> Type

foreign import data MultiNode :: Timeline -> Row Type -> Row Type -> Type

foreign import data ProcessNode :: Timeline -> Type -> Type -> Type

foreign import data InputNode :: Timeline -> Type -> Type

foreign import data OutputNode :: Timeline -> Type -> Type

foreign import data LatchNode :: Timeline -> Type -> Type

type EvalProcess t a b =
  ProcessNode t a b -> a -> (b -> Raff t Unit) -> Raff t Unit

type EvalMulti t ri riRead ro roWrite =
  MultiNode t ri ro -> { | riRead } -> { | roWrite } -> Raff t Unit

class IsNode t a b n | n -> t a b where
  toNode :: n -> Node t a b

instance IsNode t a b (Node t a b) where
  toNode = identity

instance IsNode t Unit Unit (MultiNode ri ro b) where
  toNode = unsafeCoerce

instance IsNode t a a (InputNode t a) where
  toNode = unsafeCoerce

instance IsNode t a a (OutputNode t a) where
  toNode = unsafeCoerce

instance IsNode t a a (LatchNode t a) where
  toNode = unsafeCoerce

instance IsNode t a b (ProcessNode t a b) where
  toNode = unsafeCoerce

newtype ExistsNode t b = ExistsNode
  (forall r. (forall a node. HasChildren t a b node => node -> r) -> r)

mkExistsNode
  :: forall t a b node. HasChildren t a b node => node -> ExistsNode t b
mkExistsNode node = ExistsNode \k -> k node

runExistsNode
  :: forall t b r
   . ExistsNode t b
  -> (forall a node. HasChildren t a b node => node -> r)
  -> r
runExistsNode (ExistsNode cont) = cont

foreign import data Scheduler :: Type

foreign import _withNetwork
  :: forall r
   . EffectFn4 (forall a. a -> Maybe a) (forall a. Maybe a) Scheduler
       (forall t. Network t -> Effect r)
       r

foreign import timeoutScheduler :: Scheduler

foreign import animationFrameScheduler :: Scheduler

foreign import schedule :: Scheduler -> EffectFn1 (Effect Unit) Unit

withNetwork
  :: forall r. Scheduler -> (forall t. Network t -> Effect r) -> Effect r
withNetwork = runEffectFn4 _withNetwork Just Nothing

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

foreign import cached :: forall t a. Raff t a -> Raff t a

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

foreign import newInput :: forall t a. Raff t (InputNode t a)
foreign import newOutput
  :: forall t a. (a -> Effect Unit) -> Raff t (OutputNode t a)

foreign import newLatch :: forall t a. a -> Raff t (LatchNode t a)
foreign import newProcess
  :: forall t a b. EvalProcess t a b -> Raff t (ProcessNode t a b)

foreign import _newMulti
  :: forall t ri riRead ro roWrite
   . Fn3 { | ri } { | ro } (EvalMulti t ri riRead ro roWrite)
       (Raff t (MultiNode t ri ro))

foreign import actuate :: forall t. Raff t Unit
foreign import deactivate :: forall t. Raff t Unit
foreign import resume :: forall t. Raff t Unit
foreign import suspend :: forall t. Raff t Unit
foreign import readLatch :: forall t a. LatchNode t a -> Raff t a
foreign import readNode :: forall t a b. Node t a b -> Raff t (Maybe b)

newMulti
  :: forall t rli ri riRead rlo ro roWrite
   . RowToList ri rli
  => RowToList ro rlo
  => InputRow t ri riRead rli
  => OutputRow t ro roWrite rlo
  => { | ri }
  -> { | ro }
  -> EvalMulti t ri riRead ro roWrite
  -> Raff t (MultiNode t ri ro)
newMulti = runFn3 _newMulti

class InputRow :: Timeline -> Row Type -> Row Type -> RowList Type -> Constraint
class InputRow t r rRead rl | rl -> r rRead t

instance InputRow t () () RL.Nil

instance
  ( HasChildren t x y node
  , Row.Cons label node r' r
  , Row.Cons label (Raff t (Maybe y)) rRead' rRead
  , InputRow t r' rRead' rl
  ) =>
  InputRow t r rRead (RL.Cons label node rl)

class OutputRow
  :: Timeline -> Row Type -> Row Type -> RowList Type -> Constraint
class OutputRow t r rWrite rl | rl -> r rWrite t

instance OutputRow t () () RL.Nil

instance
  ( HasParents t x y node
  , Row.Cons label node r' r
  , Row.Cons label (x -> Raff t Unit) rWrite' rWrite
  , OutputRow t r' rWrite' rl
  ) =>
  OutputRow t r rWrite (RL.Cons label node rl)
