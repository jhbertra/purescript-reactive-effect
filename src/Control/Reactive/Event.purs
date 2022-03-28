module Control.Reactive.Event where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, class Plus)
import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1, defer1)
import Control.Plus (empty)
import Control.Reactive.Prim.Frame.Future (FutureFrame)
import Data.Align (class Align, class Alignable, align, nil)
import Data.Compactable (class Compactable, compact, separate)
import Data.Either (Either(..))
import Data.Filterable (class Filterable, filter, filterMap, partitionMap)
import Data.Foldable (class Foldable, foldr)
import Data.Lazy as DL
import Data.Maybe (Maybe(..))
import Data.Ord.Max (Max(..))
import Data.These (These(..))
import Safe.Coerce (coerce)

-------------------------------------------------------------------------------
-- Events: semantically, List (Tuple time value)
--
-- The implementation of events in this library is very close to the one
-- proposed in http://conal.net/papers/push-pull-frp/push-pull-frp.pdf. We model
-- events as a `Step` that will occur at some point in the future. The
-- definitions of events and steps are mutually recursive, and are tightly
-- related to one another.
--
-- Unlike in the original push-pull FRP paper however, the basic types are
-- parametric in the `future` type constructor. As a result, all combinators for
-- behaviours and events are written against an abstract notion of future
-- values. This means that all an implementation need provide (though this is
-- by no means a trivial task), is a concrete `Future` implementation with
-- requisite typeclass instances, along with the necessary IO layer for this
-- type.
--
-- This library provides two such implementations: a semantic model, which
-- implements `futures` as a lazily evaluated (time, value) pair, and a
-- push-based production implementation, which implements `futures` using
-- continuation-passing style.
-------------------------------------------------------------------------------

-- | An event parameterized by its `future` type.
newtype AnEvent future time a = Event (future (AStep future time a))

-- | An event that uses a push-based `Future` implementation.
type Event = AnEvent FutureFrame

instance Lazy1 future => Lazy (AnEvent future time a) where
  defer f = Event $ defer1 $ coerce f

instance Lazy1 future => Lazy1 (AnEvent future time) where
  defer1 = defer

derive instance Functor future => Functor (AnEvent future time)
instance Alt future => Alt (AnEvent future time) where
  alt (Event fa) (Event fb) = Event $ fa <|> fb

instance Plus future => Plus (AnEvent future time) where
  empty = Event empty

instance (Ord time, Align future) => Align (AnEvent future time) where
  align f ea@(Event fa) eb@(Event (fb)) =
    Event $ align alignStep fa fb
    where
    alignStep (This (Step t a ea')) = Step t (f $ This a) $ align f ea' eb
    alignStep (That (Step t b eb')) = Step t (f $ That b) $ align f ea eb'
    alignStep (Both (Step t1 a ea') (Step t2 b eb')) =
      Step (t1 <> t2) (f $ Both a b) $ align f ea' eb'

instance (Ord time, Alignable future) => Alignable (AnEvent future time) where
  nil = Event nil

instance (Monad future, Ord time) => Compactable (AnEvent future time) where
  compact (Event fma) = Event $ fma >>= compactFutureStep
  separate (Event fma) =
    { left: Event $ fma >>= leftFutureStep
    , right: Event $ fma >>= rightFutureStep
    }

instance (Monad future, Ord time) => Filterable (AnEvent future time) where
  filter p (Event fa) = Event $ fa >>= filterFutureStep p
  filterMap f (Event fa) = Event $ fa >>= filterMapFutureStep f
  partition p e =
    { no: filter (not <<< p) e
    , yes: filter p e
    }
  partitionMap f (Event fa) =
    { left: Event $ fa >>= leftMapFutureStep f
    , right: Event $ fa >>= rightMapFutureStep f
    }

compactFutureStep
  :: forall future time a
   . Ord time
  => Monad future
  => AStep future time (Maybe a)
  -> future (AStep future time a)
compactFutureStep (Step _ Nothing ema) = coerce $ compact ema
compactFutureStep (Step t (Just a) ema) = pure $ Step t a $ compact ema

filterFutureStep
  :: forall future time a
   . Ord time
  => Monad future
  => (a -> Boolean)
  -> AStep future time a
  -> future (AStep future time a)
filterFutureStep p (Step t a ea)
  | p a = coerce $ filter p ea
  | otherwise = pure $ Step t a ea

filterMapFutureStep
  :: forall future time a b
   . Ord time
  => Monad future
  => (a -> Maybe b)
  -> AStep future time a
  -> future (AStep future time b)
filterMapFutureStep f (Step t a ea) = case f a of
  Nothing -> coerce $ filterMap f ea
  Just b -> pure $ Step t b $ filterMap f ea

leftFutureStep
  :: forall future time a b
   . Ord time
  => Monad future
  => AStep future time (Either a b)
  -> future (AStep future time a)
leftFutureStep (Step t eitherAB eEitherAB) = case eitherAB of
  Left a -> pure $ Step t a separatedEvent.left
  _ -> coerce separatedEvent.left
  where
  separatedEvent = separate eEitherAB

leftMapFutureStep
  :: forall future time a b c
   . Ord time
  => Monad future
  => (a -> Either b c)
  -> AStep future time a
  -> future (AStep future time b)
leftMapFutureStep f (Step t a ea) = case f a of
  Left b -> pure $ Step t b partitionedEvent.left
  _ -> coerce partitionedEvent.left
  where
  partitionedEvent = partitionMap f ea

rightFutureStep
  :: forall future time a b
   . Ord time
  => Monad future
  => AStep future time (Either a b)
  -> future (AStep future time b)
rightFutureStep (Step t eitherAB eEitherAB) = case eitherAB of
  Right b -> pure $ Step t b separatedEvent.right
  _ -> coerce separatedEvent.right
  where
  separatedEvent = separate eEitherAB

rightMapFutureStep
  :: forall future time a b c
   . Ord time
  => Monad future
  => (a -> Either b c)
  -> AStep future time a
  -> future (AStep future time c)
rightMapFutureStep f (Step t a ea) = case f a of
  Right c -> pure $ Step t c partitionedEvent.right
  _ -> coerce partitionedEvent.right
  where
  partitionedEvent = partitionMap f ea

unionWith :: forall f a. Align f => (a -> a -> a) -> f a -> f a -> f a
unionWith f = align case _ of
  This a -> a
  That a -> a
  Both a b -> f a b

unions
  :: forall t f a. Alignable f => Foldable t => t (f (a -> a)) -> f (a -> a)
unions = foldr (unionWith (<<<)) nil

-------------------------------------------------------------------------------
-- Step - an intermediate type that is used to represent both events and
-- behaviours. It is a direct data analogy of the `stepper` combinator to
-- construct a behaviour from an initial value and an event. A `Step` consists
-- of a current value, along with an event that will change this value when it
-- fires.
-------------------------------------------------------------------------------

-- | A Step parameterized by its `future` type.
data AStep future time a = Step (Max time) a (AnEvent future time a)

-- | A Step that uses a push-based `Future` implementation.
type Step = AStep FutureFrame

instance Functor future => Functor (AStep future time) where
  map f (Step t a e) = Step t (f a) $ map f e

instance (Ord time, Alt future, Apply future) => Apply (AStep future time) where
  apply sf@(Step t1 f ef) sa@(Step t2 a ea) =
    Step (t1 <> t2) (f a) (applyEventStep ef sa <|> applyStepEvent sf ea)

applyStepEvent
  :: forall time future a b
   . Functor future
  => Ord time
  => Alt future
  => Apply future
  => AStep future time (a -> b)
  -> AnEvent future time a
  -> AnEvent future time b
applyStepEvent sf (Event fa) = Event $ (sf <*> _) <$> fa

applyEventStep
  :: forall time future a b
   . Functor future
  => Ord time
  => Alt future
  => Apply future
  => AnEvent future time (a -> b)
  -> AStep future time a
  -> AnEvent future time b
applyEventStep (Event ff) stepA = Event $ (_ <*> stepA) <$> ff

instance (Bounded time, Alternative future) => Applicative (AStep future time) where
  pure a = Step mempty a empty

instance (Ord time, Alternative future) => Bind (AStep future time) where
  bind (Step t a ea) k = case k a of
    Step t' b eb -> Step (t <> t') b $ eb <|> bindE ea k

bindE
  :: forall future time a b
   . Functor future
  => Ord time
  => Alternative future
  => AnEvent future time a
  -> (a -> AStep future time b)
  -> AnEvent future time b
bindE (Event fa) k = Event $ map (join <<< (map k)) fa

instance (Bounded time, Alternative future) => Monad (AStep future time)

instance (Lazy time, Lazy a, Lazy1 future) => Lazy (AStep future time a) where
  defer f = Step
    (Max $ defer (\_ -> DL.force dlT))
    (defer (\_ -> DL.force dlA))
    (defer (\_ -> DL.force dlE))
    where
    dlStep = DL.defer f
    dlT = dlStep <#> case _ of Step (Max t) _ _ -> t
    dlA = dlStep <#> case _ of Step _ a _ -> a
    dlE = dlStep <#> case _ of Step _ _ e -> e
