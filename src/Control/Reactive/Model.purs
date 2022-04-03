module Control.Reactive.Model where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1)
import Control.Monad.Fix (class MonadFix, liftLazy, mfixTuple)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Control.Reactive.Moment (class MonadAdjustMoment, class MonadMoment)
import Data.Align (class Align, class Alignable, align)
import Data.Array as Array
import Data.Enum
  ( class BoundedEnum
  , class Enum
  , Cardinality(..)
  , pred
  , succ
  , toEnum
  )
import Data.Filterable (filter)
import Data.Lazy (force)
import Data.Lazy as DL
import Data.List as L
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LLN
import Data.List.NonEmpty as LN
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Ord.Down (Down(..))
import Data.These (These(..), theseLeft, theseRight)
import Data.Tuple (Tuple(..), fst)
import Effect.Exception.Unsafe (unsafeThrow)
import Safe.Coerce (coerce)
import Test.Data.Observe (class Observable, observe)

newtype Time = Time (DL.Lazy Int)

instance Observable Unit Int Time where
  observe _ (Time t) = force t

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

data Future a = Future (LLN.NonEmptyList Time) (DL.Lazy a)

instance Observable t o a => Observable t (Tuple Int (Maybe o)) (Future a) where
  observe t (Future time a) = Tuple (observe unit time') o
    where
    time' = LLN.head time
    o
      | time' == top = Nothing
      | otherwise = Just $ observe t a

instance Eq a => Eq (Future a) where
  eq (Future t1 a) (Future t2 b)
    | (LLN.head t1) == top || (LLN.head t2) == top = LLN.head t1 == LLN.head t2
    | otherwise = (LLN.head t1) == (LLN.head t2) && a == b

instance Show a => Show (Future a) where
  show (Future t a)
    | (LLN.head t) == top = "empty"
    | otherwise = "(Future " <> show t <> " " <> show a <> ")"

futureTime :: forall a. Future a -> LLN.NonEmptyList Time
futureTime (Future t _) = t

derive instance Functor Future

instance Lazy (Future a) where
  defer f = Future
    (LLN.NonEmptyList $ defer $ f >>> futureTime >>> coerce)
    (DL.defer \_ -> case f unit of Future _ a -> force a)

instance Lazy1 Future where
  defer1 = defer

instance Apply Future where
  apply
    (Future (LLN.NonEmptyList lts1) f)
    (Future (LLN.NonEmptyList lts2) a) =
    Future (unwrap <$> mergedTimes) $ apply f a
    where
    mergedTimes = LLN.NonEmptyList $ DL.defer \_ ->
      let
        t1 :| ts1 = force lts1
        t2 :| ts2 = force lts2
        ts1' = LN.NonEmptyList $ t1 :| L.fromFoldable ts1
        ts2' = LN.NonEmptyList $ t2 :| L.fromFoldable ts2
        LN.NonEmptyList (t :| ts) = LN.sort $ map Down $ ts1' <> ts2'
      in
        t :| LL.fromFoldable ts

instance Applicative Future where
  pure = future mempty

instance Alt Future where
  alt (Future t1 a) (Future t2 b) = Future
    (LLN.NonEmptyList $ liftLazy $ coerce $ futureTime <$> lFuture)
    (DL.defer \_ -> case force lFuture of Future _ a' -> force a')
    where
    lFuture = DL.defer \_ ->
      if t1' <= t2' then Future t1 a else Future t2 b
    t1a = Array.fromFoldable t1
    t2a = Array.fromFoldable t2
    t1' = align (fromMaybe top <<< theseLeft) t1a t2a
    t2' = align (fromMaybe top <<< theseRight) t1a t2a

instance Plus Future where
  empty = Future (pure top) $ DL.defer \_ -> unsafeThrow
    "***Control.Reactive.Model.empty (Future): evaluated empty future"

instance Alternative Future

instance Align Future where
  align f (Future t1 a) (Future t2 b) = Future
    (LLN.NonEmptyList $ liftLazy $ coerce $ futureTime <$> lFuture)
    (DL.defer \_ -> case force lFuture of Future _ a' -> force a')
    where
    lFuture = DL.defer \_ ->
      case compare (LLN.head t1) (LLN.head t2) of
        LT -> Future t1 $ f <<< This <$> a
        GT -> Future t2 $ f <<< That <$> b
        EQ -> Future t1 $ map f $ Both <$> a <*> b

instance Alignable Future where
  nil = empty

instance Bind Future where
  bind (Future time@(LLN.NonEmptyList t) fa) k = Future
    (LLN.NonEmptyList $ liftLazy $ coerce $ futureTime <$> lFuture)
    (DL.defer \_ -> case force lFuture of Future _ a -> force a)
    where
    lFuture = DL.defer \_ -> mkLFuture
    mkLFuture
      | LLN.head time == top = empty
      | otherwise = case force fa of
          a -> case k a of
            Future (LLN.NonEmptyList t') b ->
              let
                t1 :| ts1 = force t
                t2 :| ts2 = force t'
                ts1' = LN.NonEmptyList $ t1 :| L.fromFoldable ts1
                ts2' = LN.NonEmptyList $ t2 :| L.fromFoldable ts2
                LN.NonEmptyList (t :| ts) = LN.sort $ map Down $ ts1' <> ts2'
                mergedTimes =
                  LLN.NonEmptyList $ DL.defer \_ -> t :| LL.fromFoldable ts
              in
                Future (unwrap <$> mergedTimes) b

instance MonadFix Future where
  mfix f =
    let
      lFuture = fst <$> mfixTuple \(Tuple _ a) -> DL.defer \_ -> case f a of
        Future t a' -> Tuple (Future t a') $ DL.force a'
    in
      Future
        (LLN.NonEmptyList $ liftLazy $ coerce $ futureTime <$> lFuture)
        (DL.defer \_ -> case force lFuture of Future _ a -> force a)

instance MonadMoment Time Future where
  withMoment (Future time a) = Future time (Tuple (LLN.head time) <$> a)

instance MonadAdjustMoment Time Future where
  adjustMoment f (Future time a) = Future (f <$> time) a

instance Monad Future
instance MonadPlus Future

instance Semigroup a => Semigroup (Future a) where
  append = lift2 append

instance Monoid a => Monoid (Future a) where
  mempty = pure mempty

future :: forall a. Time -> a -> Future a
future time = Future (pure time) <<< pure
