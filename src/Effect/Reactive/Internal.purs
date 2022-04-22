module Effect.Reactive.Internal
  ( class IsNode
  , class IsChild
  , class IsParent
  , class InputRow
  , class OutputRow
  , Animation
  , BufferNode
  , ChildNode
  , EvalProcess
  , InputNode
  , Latch
  , LatchWrite
  , Network
  , Node
  , OutputNode
  , ParentNode
  , ProcessNode
  , Raff
  , Scheduler
  , TestScheduler
  , actuate
  , addChild
  , addParent
  , cached
  , deactivate
  , emptyNode
  , fire
  , ground
  , networkTime
  , newAnimation
  , newAnimationFrameScheduler
  , newBuffer
  , newCircuit
  , newExecute
  , newInput
  , newLatch
  , newLazyLatch
  , newOutput
  , newProcess
  , newTimeoutScheduler
  , onConnected
  , readLatch
  , readNode
  , removeChild
  , removeParent
  , resume
  , runRaff
  , runLater
  , setAnimation
  , suspend
  , testRaff
  , toChild
  , toNode
  , toParent
  , toScheduler
  , withNetwork
  ) where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Fix (class MonadFix)
import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT(..)
  , runReaderT
  )
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Unlift (class MonadUnlift)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Lazy as DL
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Reactive.Types (Time, Timeline)
import Effect.Uncurried
  ( EffectFn1
  , EffectFn2
  , EffectFn4
  , runEffectFn1
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

foreign import data Animation :: Timeline -> Type -> Type

foreign import data Node :: Timeline -> Type -> Type -> Type

foreign import data ProcessNode :: Timeline -> Type -> Type -> Type

foreign import data InputNode :: Timeline -> Type -> Type

foreign import data ChildNode :: Timeline -> Type -> Type -> Type

foreign import data ParentNode :: Timeline -> Type -> Type -> Type

foreign import data LatchWrite :: Timeline -> Type

foreign import data BufferNode :: Timeline -> Type -> Type

foreign import data OutputNode :: Timeline -> Type -> Type

foreign import data Latch :: Timeline -> Type -> Type

type EvalProcess t a b = (b -> Raff t Unit) -> a -> Raff t Unit

type EvalCircuit t riRead roWrite =
  { | riRead } -> { | roWrite } -> Raff t Unit

class IsNode t a b n | n -> t a b where
  toNode :: n -> Node t a b

instance IsNode t a b (Node t a b) where
  toNode = identity

instance IsNode t a a (InputNode t a) where
  toNode = unsafeCoerce

instance IsNode t Void Unit (LatchWrite t) where
  toNode = unsafeCoerce

instance IsNode t a b (ParentNode t b a) where
  toNode = unsafeCoerce

instance IsNode t a b (ChildNode t a b) where
  toNode = unsafeCoerce

instance IsNode t a a (BufferNode t a) where
  toNode = unsafeCoerce

instance IsNode t a a (OutputNode t a) where
  toNode = unsafeCoerce

instance IsNode t a b (ProcessNode t a b) where
  toNode = unsafeCoerce

class IsNode t b a n <= IsParent t a b n | n -> t a b where
  toParent :: n -> ParentNode t a b

instance IsParent t a b (ParentNode t a b) where
  toParent = identity

instance IsParent t a a (InputNode t a) where
  toParent = unsafeCoerce

instance IsParent t Unit Void (LatchWrite t) where
  toParent = unsafeCoerce

instance IsParent t a a (BufferNode t a) where
  toParent = unsafeCoerce

instance IsParent t b a (ProcessNode t a b) where
  toParent = unsafeCoerce

class IsNode t a b n <= IsChild t a b n | n -> t a b where
  toChild :: n -> ChildNode t a b

instance IsChild t a b (ChildNode t a b) where
  toChild = identity

instance IsChild t a a (OutputNode t a) where
  toChild = unsafeCoerce

instance IsChild t a a (BufferNode t a) where
  toChild = unsafeCoerce

instance IsChild t a b (ProcessNode t a b) where
  toChild = unsafeCoerce

foreign import data Scheduler :: Type

foreign import data TestScheduler :: Type

foreign import _withNetwork
  :: forall r
   . EffectFn4 (forall a. a -> Maybe a) (forall a. Maybe a) Scheduler
       (forall t. Network t -> Effect r)
       r

foreign import newTimeoutScheduler :: Effect Scheduler

foreign import newAnimationFrameScheduler :: Effect Scheduler

foreign import newTestScheduler :: Effect TestScheduler

foreign import flushTestScheduler :: TestScheduler -> Effect Unit

toScheduler :: TestScheduler -> Scheduler
toScheduler = unsafeCoerce

withNetwork
  :: forall r. Scheduler -> (forall t. Network t -> Effect r) -> Effect r
withNetwork = runEffectFn4 _withNetwork Just Nothing

newtype Raff t a = Raff (ReaderT (Network t) Effect a)

runRaff :: forall t a. Raff t a -> Network t -> Effect a
runRaff = coerce (runReaderT :: _ (Network t) Effect a -> _ _ -> _ a)

testRaff :: forall a. (forall t. Raff t Unit -> Raff t a) -> Effect a
testRaff f = do
  testScheduler <- newTestScheduler
  withNetwork
    (toScheduler testScheduler)
    (runRaff (f (liftEffect $ flushTestScheduler testScheduler)))

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

derive newtype instance MonadFix (Raff t)

instance Lazy (Raff t a) where
  defer f = Raff $ ReaderT $ defer \_ -> runReaderT $ coerce f unit

foreign import cached :: forall t a. Raff t a -> Raff t a

foreign import _addParent
  :: forall t a b c. EffectFn2 (ChildNode t b c) (ParentNode t b a) Unit

foreign import _removeParent
  :: forall t a b c. EffectFn2 (ChildNode t b c) (ParentNode t b a) Unit

foreign import _addChild
  :: forall t a b c. EffectFn2 (ParentNode t b a) (ChildNode t b c) Unit

foreign import _removeChild
  :: forall t a b c. EffectFn2 (ParentNode t b a) (ChildNode t b c) Unit

foreign import _fire :: forall t a. EffectFn2 a (InputNode t a) Unit

foreign import _onConnected
  :: forall t a b. EffectFn2 (Node t a b) (Effect (Effect Unit)) (Effect Unit)

foreign import _setAnimation
  :: forall t a
   . EffectFn2 (Animation t a) (Time t -> Effect Unit) (Effect Unit)

setAnimation
  :: forall t a
   . Animation t a
  -> (Time t -> Effect Unit)
  -> Effect (Effect Unit)
setAnimation = runEffectFn2 _setAnimation

addParent
  :: forall t x y z c p
   . IsChild t y z c
  => IsParent t y x p
  => c
  -> p
  -> Raff t Unit
addParent c p =
  liftEffect $ runEffectFn2 _addParent (toChild c) (toParent p)

removeParent
  :: forall t x y z c p
   . IsChild t y z c
  => IsParent t y x p
  => c
  -> p
  -> Raff t Unit
removeParent c p =
  liftEffect $ runEffectFn2 _removeParent (toChild c) (toParent p)

addChild
  :: forall t x y z c p
   . IsParent t y x p
  => IsChild t y z c
  => p
  -> c
  -> Raff t Unit
addChild p c =
  liftEffect $ runEffectFn2 _addChild (toParent p) (toChild c)

removeChild
  :: forall t x y z c p
   . IsParent t y x p
  => IsChild t y z c
  => p
  -> c
  -> Raff t Unit
removeChild p c =
  liftEffect $ runEffectFn2 _removeChild (toParent p) (toChild c)

fire :: forall t a. a -> InputNode t a -> Raff t Unit
fire a = liftEffect <<< runEffectFn2 _fire a

onConnected
  :: forall t a b node
   . IsNode t a b node
  => node
  -> Effect (Effect Unit)
  -> Raff t (Effect Unit)
onConnected node = liftEffect <<< runEffectFn2 _onConnected (toNode node)

foreign import newInput :: forall t a. String -> Raff t (InputNode t a)
foreign import emptyNode :: forall t a. Raff t (BufferNode t a)
foreign import ground :: forall t a. Raff t (InputNode t a)
foreign import newBuffer :: forall t a. String -> Raff t (BufferNode t a)
foreign import newAnimation :: forall t a. Raff t (Animation t a)
foreign import newOutput
  :: forall t a. String -> (a -> Effect Unit) -> Raff t (OutputNode t a)

foreign import _newLatch
  :: forall t a
   . String
  -> a
  -> Raff t
       { latch :: Latch t a
       , setUpdates :: EffectFn1 (LatchWrite t) Unit
       }

foreign import _newLazyLatch
  :: forall t a
   . String
  -> DL.Lazy a
  -> Raff t
       { latch :: Latch t a
       , setUpdates :: EffectFn1 (LatchWrite t) Unit
       }

foreign import newProcess
  :: forall t a b. String -> EvalProcess t a b -> Raff t (ProcessNode t a b)

foreign import newExecute
  :: forall t a b. String -> (a -> Raff t b) -> Raff t (ProcessNode t a b)

foreign import _newCircuit
  :: forall t ri riRead ro roWrite
   . Fn4 String { | ri } { | ro } (EvalCircuit t riRead roWrite) (Raff t Unit)

foreign import runLater :: forall t. Raff t Unit -> Raff t Unit
foreign import actuate :: forall t. Raff t Unit
foreign import deactivate :: forall t. Raff t Unit
foreign import resume :: forall t. Raff t Unit
foreign import suspend :: forall t. Raff t Unit
foreign import _readLatch :: forall t a. Latch t a -> Raff t (Unit -> a)
foreign import _readNode :: forall t a b. Node t a b -> Raff t (Maybe b)
foreign import networkTime :: forall t. Raff t (Time t)

newLatch
  :: forall t a b node
   . IsParent t a b node
  => String
  -> a
  -> Raff t { latch :: Latch t a, setUpdates :: node -> Raff t (LatchWrite t) }
newLatch name initialValue = do
  { latch, setUpdates } <- _newLatch name initialValue
  pure
    { latch
    , setUpdates: \node -> do
        let lw = unsafeCoerce node
        liftEffect $ runEffectFn1 setUpdates $ unsafeCoerce lw
        pure lw
    }

newLazyLatch
  :: forall t a b node
   . IsParent t (DL.Lazy a) b node
  => String
  -> (DL.Lazy a)
  -> Raff t { latch :: Latch t a, setUpdates :: node -> Raff t (LatchWrite t) }
newLazyLatch name initialValue = do
  { latch, setUpdates } <- _newLazyLatch name initialValue
  pure
    { latch
    , setUpdates: \node -> do
        let lw = unsafeCoerce node
        liftEffect $ runEffectFn1 setUpdates $ unsafeCoerce lw
        pure lw
    }

readLatch :: forall t a. Latch t a -> Raff t (DL.Lazy a)
readLatch = map DL.defer <<< _readLatch

readNode :: forall t a b node. IsNode t a b node => node -> Raff t (Maybe b)
readNode = _readNode <<< toNode

newCircuit
  :: forall t rli ri riRead rlo ro roWrite
   . RowToList ri rli
  => RowToList ro rlo
  => InputRow t ri riRead rli
  => OutputRow t ro roWrite rlo
  => String
  -> { | ri }
  -> { | ro }
  -> EvalCircuit t riRead roWrite
  -> Raff t Unit
newCircuit = runFn4 _newCircuit

class InputRow :: Timeline -> Row Type -> Row Type -> RowList Type -> Constraint
class InputRow t r rRead rl | rl -> r rRead t

instance InputRow t () () RL.Nil

instance
  ( IsParent t y x node
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
  ( IsChild t x y node
  , Row.Cons label node r' r
  , Row.Cons label (x -> Raff t Unit) rWrite' rWrite
  , OutputRow t r' rWrite' rl
  ) =>
  OutputRow t r rWrite (RL.Cons label node rl)
