module Control.Reactive.Model where

import Prelude hiding ((<@>))

import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Bind (bindFlipped)
import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1)
import Control.Monad.Fix (class MonadFix, mfixRecord)
import Control.Monad.Reader (class MonadAsk, Reader, ask, runReader)
import Control.Plus (class Plus)
import Control.Reactive.Behaviour (class Behaviour, ABehaviour, (<@>))
import Control.Reactive.Class (class Reactive, hold)
import Control.Reactive.Event (class Event, AStep, AnEvent)
import Data.Align (class Align, class Alignable)
import Data.Align as A
import Data.Array as Array
import Data.Compactable (class Compactable)
import Data.Either (either, hush)
import Data.Filterable
  ( class Filterable
  , filter
  , filterMapDefault
  , partitionMapDefault
  )
import Data.Lazy (force)
import Data.Lazy as DL
import Data.List.Lazy
  ( List(..)
  , drop
  , iterate
  , repeat
  , replicate
  , scanlLazy
  , tail
  , take
  , uncons
  , zipWith
  , (!!)
  , (:)
  )
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype, over, un, unwrap)
import Data.Ord.Max (Max(..))
import Data.These (These(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafePartial)

type Time = Int
newtype Future a = Future (DL.Lazy (Tuple (Max Time) a))

type Event = AnEvent Future Time
type Behaviour = ABehaviour Future Time
type Event = AStep Future Time

derive instance Functor Future

instance Lazy (Future a) where
  defer = Future <<< defer << coerce

instance Lazy1 (Future a) where
  defer1 = defer

instance Apply Future where
  apply (Future f1) (Future f2) = lift2 apply f1 f2

instance Applicative Future where
  pure = Future <<< pure <<< pure

instance Applicative Future where
  pure = Future <<< pure <<< pure

instance Alt Future where
  alt (Future a) (Future b) = Future $ DL.defer \_ -> case force a, force b of
    Tuple t1 a, Tuple t2 b
      | t1 <= t2 -> Tuple t1 a
      | otherwise -> Tuple t2 b

instance Plus Future where
  empty = Future top $ DL.defer \_ -> unsafeThrow
    "***Control.Reactive.Model.empty (Future): evaluated empty future"

instance Alternative Future

instance Align Future where
  align f (Future a) (Future b) = Future $ DL.defer \_ ->
    case force a, force b of
      Tuple t1 a, Tuple t2 b -> case compare t1 t2 of
        LT -> Tuple t1 $ f $ This a
        GT -> Tuple t2 $ f $ That b
        EQ -> Tuple t1 $ f $ Both a b

instance Alignable Future where
  nil = empty

instance Bind Future where
  bind (Future fa) k = Future $ DL.defer \_ ->
    case force fa of
      Tuple t a -> case force $ coerce $ k a of
        Tuple t' b -> Tuple (t <> t') b

instance Monad Future

instance Semigroup a => Semigroup (Future a) where
  append = lift2 append

instance Monoid a => Monoid (Future a) where
  mempty = pure mempty
