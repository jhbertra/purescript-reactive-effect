module Control.Reactive.Future
  ( Future
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Writer (WriterT(..))
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Align (class Align, class Alignable, nil)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Identity (Identity(..))
import Data.Ord.Max (Max(..))
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, ParAff, never)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

newtype Future time a = Future (WriterT (Max time) Aff a)

derive newtype instance Functor (Future time)
derive newtype instance Ord time => Apply (Future time)
derive newtype instance Ord time => Alt (Future time)
instance Ord time => Align (Future time) where
  align f (Future (WriterT affA)) (Future (WriterT affB)) = Future $ WriterT ado
    Tuple a t1 <- affA
    Tuple b t2 <- affB
    in
      flip Tuple (t1 <> t2) $ f case compare t1 t2 of
        GT -> This a
        LT -> That b
        EQ -> Both a b

derive newtype instance Ord time => Bind (Future time)
derive newtype instance Ord time => Plus (Future time)
derive newtype instance Bounded time => Applicative (Future time)
instance Ord time => Alignable (Future time) where
  nil = Future $ WriterT never

instance Bounded time => Alternative (Future time)
instance Bounded time => Monad (Future time)

instance Bifunctor Future where
  bimap f g (Future (WriterT aff)) =
    Future $ WriterT $ map (bimap g (coerce f)) aff

derive newtype instance (Ord time, Semigroup a) => Semigroup (Future time a)
derive newtype instance (Bounded time, Monoid a) => Monoid (Future time a)
derive newtype instance Bounded time => MonadRec (Future time)
derive newtype instance Bounded time => MonadThrow Error (Future time)
derive newtype instance Bounded time => MonadEffect (Future time)
derive newtype instance Bounded time => MonadAff (Future time)
instance Bounded time => MonadBase (Future time) (Future time) where
  liftBase = identity

instance Bounded time => MonadBaseControl (Future time) (Future time) Identity where
  liftBaseWith f = f coerce
  restoreM = coerce

instance Lazy (Future time a) where
  defer f = Future $ WriterT $ defer $ coerce f

instance Lazy1 (Future time) where
  defer1 = defer

newtype ParFuture time a = ParFuture (WriterT (Max time) ParAff a)

derive newtype instance Functor (ParFuture time)
derive newtype instance Ord time => Apply (ParFuture time)
derive newtype instance Ord time => Alt (ParFuture time)
instance Ord time => Align (ParFuture time) where
  align f (ParFuture (WriterT affA)) (ParFuture (WriterT affB)) = ParFuture $
    WriterT ado
      Tuple a t1 <- affA
      Tuple b t2 <- affB
      in
        flip Tuple (t1 <> t2) $ f case compare t1 t2 of
          GT -> This a
          LT -> That b
          EQ -> Both a b

instance Bounded time => Plus (ParFuture time) where
  empty = nil

derive newtype instance Bounded time => Applicative (ParFuture time)
instance Ord time => Alignable (ParFuture time) where
  nil = ParFuture $ WriterT $ parallel never

instance Bounded time => Alternative (ParFuture time)

instance Bifunctor ParFuture where
  bimap f g (ParFuture (WriterT aff)) =
    ParFuture $ WriterT $ map (bimap g (coerce f)) aff

instance (Ord time, Semigroup a) => Semigroup (ParFuture time a) where
  append = lift2 append

instance (Bounded time, Monoid a) => Monoid (ParFuture time a) where
  mempty = pure mempty

instance Lazy (ParFuture time a) where
  defer f = ParFuture $ WriterT $ parallel $ defer $ sequential <<< coerce f

instance Lazy1 (ParFuture time) where
  defer1 = defer

instance Bounded time => Parallel (ParFuture time) (Future time) where
  parallel = unsafeCoerce (parallel :: Aff ~> ParAff)
  sequential = unsafeCoerce (sequential :: ParAff ~> Aff)
