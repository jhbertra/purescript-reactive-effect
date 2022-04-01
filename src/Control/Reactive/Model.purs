module Control.Reactive.Model where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1)
import Control.Monad.Fix (class MonadFix, liftLazy, mfixTuple)
import Control.Plus (class Plus)
import Control.Reactive.Behaviour (ABehaviour)
import Control.Reactive.Event (AStep(..), AnEvent(..))
import Control.Reactive.Moment (class MonadAdjustMoment, class MonadMoment)
import Data.Align (class Align, class Alignable, nil)
import Data.Enum
  ( class BoundedEnum
  , class Enum
  , Cardinality(..)
  , fromEnum
  , pred
  , succ
  , toEnum
  )
import Data.Filterable (filter)
import Data.Lazy (force)
import Data.Lazy as DL
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.These (These(..))
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Unfoldable (unfoldr)
import Effect.Exception.Unsafe (unsafeThrow)
import Safe.Coerce (coerce)

newtype Time = Time (DL.Lazy Int)

derive newtype instance Show Time
derive instance Eq Time
derive instance Ord Time
instance Enum Time where
  pred (Time i) = toEnum =<< pred (force i)
  succ (Time i) = toEnum =<< succ (force i)

instance Bounded Time where
  bottom = Time $ pure 0
  top = Time $ pure $ top - 1

instance BoundedEnum Time where
  cardinality = Cardinality top
  fromEnum = coerce (force :: DL.Lazy Int -> Int)
  toEnum = filter ((_ <= top) && (_ >= bottom)) <<< Just <<< Time <<< pure

instance Semigroup Time where
  append = max

instance Monoid Time where
  mempty = bottom

instance Lazy Time where
  defer f = Time $ DL.defer \_ -> force $ coerce $ f unit

data Future a = Future Time (DL.Lazy a)

futureTime :: forall a. Future a -> Time
futureTime (Future t _) = t

type Event = AnEvent Future
type Behaviour = ABehaviour Future Time
type Step = AStep Future

derive instance Functor Future

instance Lazy (Future a) where
  defer f = Future
    (defer $ f >>> futureTime)
    (DL.defer \_ -> case f unit of Future _ a -> force a)

instance Lazy1 Future where
  defer1 = defer

instance Apply Future where
  apply (Future t1 f) (Future t2 a) = Future (t1 <> t2) $ apply f a

instance Applicative Future where
  pure = Future mempty <<< pure

instance Alt Future where
  alt (Future t1 a) (Future t2 b) = Future
    (liftLazy $ futureTime <$> lFuture)
    (DL.defer \_ -> case force lFuture of Future _ a' -> force a')
    where
    lFuture = DL.defer \_ ->
      if t1 <= t2 then Future t1 a else Future t2 b

instance Plus Future where
  empty = Future top $ DL.defer \_ -> unsafeThrow
    "***Control.Reactive.Model.empty (Future): evaluated empty future"

instance Alternative Future

instance Align Future where
  align f (Future t1 a) (Future t2 b) = Future
    (liftLazy $ futureTime <$> lFuture)
    (DL.defer \_ -> case force lFuture of Future _ a' -> force a')
    where
    lFuture = DL.defer \_ ->
      case compare t1 t2 of
        LT -> Future t1 $ f <<< This <$> a
        GT -> Future t2 $ f <<< That <$> b
        EQ -> Future t1 $ map f $ Both <$> a <*> b

instance Alignable Future where
  nil = empty

instance Bind Future where
  bind (Future t fa) k = Future
    (liftLazy $ futureTime <$> lFuture)
    (DL.defer \_ -> case force lFuture of Future _ a -> force a)
    where
    lFuture = DL.defer \_ ->
      case force fa of
        a -> case k a of
          Future t' b -> Future (t <> t') b

instance MonadFix Future where
  mfix f =
    let
      lFuture = fst <$> mfixTuple \(Tuple _ a) -> DL.defer \_ -> case f a of
        Future t a' -> Tuple (Future t a') $ DL.force a'
    in
      Future
        (liftLazy $ futureTime <$> lFuture)
        (DL.defer \_ -> case force lFuture of Future _ a -> force a)

instance MonadMoment Time Future where
  withMoment (Future time a) = Future time (Tuple time <$> a)

instance MonadAdjustMoment Time Future where
  adjustMoment f (Future time a) = Future (f time) a

instance Monad Future

instance Semigroup a => Semigroup (Future a) where
  append = lift2 append

instance Monoid a => Monoid (Future a) where
  mempty = pure mempty

interpret
  :: forall a b
   . (Event a -> Event b)
  -> List (Maybe a)
  -> List (Maybe b)
interpret build occs = eventToList $ build $ listToEvent bottom occs
  where
  eventToList =
    unfoldr (uncurry (map uncurry unforlEvent)) <<< Tuple occs <<< Tuple 0
  unforlEvent Nil _ _ = Nothing
  unforlEvent (_ : as) elapsed e@(Event (Future time step)) = Just
    if fromEnum time == elapsed then
      let Step b event = force step in mkNext (Just b) event
    else
      mkNext Nothing e
    where
    mkNext ma event = Tuple ma $ Tuple as $ Tuple (elapsed + 1) event
  listToEvent time = case _ of
    Nil -> nil
    Nothing : as -> next as
    Just a : as -> Event $ Future time $ DL.defer \_ -> Step a $ next as
    where
    next = maybe (const nil) listToEvent $ succ time
