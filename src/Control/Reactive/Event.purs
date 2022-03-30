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
newtype AnEvent future a = Event (future (AStep future a))

-- | An event that uses a push-based `Future` implementation.
type Event = AnEvent FutureFrame

instance Lazy1 future => Lazy (AnEvent future a) where
  defer f = Event $ defer1 $ coerce f

instance Lazy1 future => Lazy1 (AnEvent future) where
  defer1 = defer

derive instance Functor future => Functor (AnEvent future)
instance Alt future => Alt (AnEvent future) where
  alt (Event fa) (Event fb) = Event $ fa <|> fb

instance Plus future => Plus (AnEvent future) where
  empty = Event empty

instance Align future => Align (AnEvent future) where
  align f ea@(Event fa) eb@(Event (fb)) =
    Event $ align alignStep fa fb
    where
    alignStep (This (Step a ea')) = Step (f $ This a) $ align f ea' eb
    alignStep (That (Step b eb')) = Step (f $ That b) $ align f ea eb'
    alignStep (Both (Step a ea') (Step b eb')) =
      Step (f $ Both a b) $ align f ea' eb'

instance Alignable future => Alignable (AnEvent future) where
  nil = Event nil

instance Monad future => Compactable (AnEvent future) where
  compact (Event fma) = Event $ fma >>= compactFutureStep
  separate (Event fma) =
    { left: Event $ fma >>= leftFutureStep
    , right: Event $ fma >>= rightFutureStep
    }

instance Monad future => Filterable (AnEvent future) where
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
  :: forall future a
   . Monad future
  => AStep future (Maybe a)
  -> future (AStep future a)
compactFutureStep (Step Nothing ema) = coerce $ compact ema
compactFutureStep (Step (Just a) ema) = pure $ Step a $ compact ema

filterFutureStep
  :: forall future a
   . Monad future
  => (a -> Boolean)
  -> AStep future a
  -> future (AStep future a)
filterFutureStep p (Step a ea)
  | p a = coerce $ filter p ea
  | otherwise = pure $ Step a ea

filterMapFutureStep
  :: forall future a b
   . Monad future
  => (a -> Maybe b)
  -> AStep future a
  -> future (AStep future b)
filterMapFutureStep f (Step a ea) = case f a of
  Nothing -> coerce $ filterMap f ea
  Just b -> pure $ Step b $ filterMap f ea

leftFutureStep
  :: forall future a b
   . Monad future
  => AStep future (Either a b)
  -> future (AStep future a)
leftFutureStep (Step eitherAB eEitherAB) = case eitherAB of
  Left a -> pure $ Step a separatedEvent.left
  _ -> coerce separatedEvent.left
  where
  separatedEvent = separate eEitherAB

leftMapFutureStep
  :: forall future a b c
   . Monad future
  => (a -> Either b c)
  -> AStep future a
  -> future (AStep future b)
leftMapFutureStep f (Step a ea) = case f a of
  Left b -> pure $ Step b partitionedEvent.left
  _ -> coerce partitionedEvent.left
  where
  partitionedEvent = partitionMap f ea

rightFutureStep
  :: forall future a b
   . Monad future
  => AStep future (Either a b)
  -> future (AStep future b)
rightFutureStep (Step eitherAB eEitherAB) = case eitherAB of
  Right b -> pure $ Step b separatedEvent.right
  _ -> coerce separatedEvent.right
  where
  separatedEvent = separate eEitherAB

rightMapFutureStep
  :: forall future a b c
   . Monad future
  => (a -> Either b c)
  -> AStep future a
  -> future (AStep future c)
rightMapFutureStep f (Step a ea) = case f a of
  Right c -> pure $ Step c partitionedEvent.right
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
data AStep future a = Step a (AnEvent future a)

-- | A Step that uses a push-based `Future` implementation.
type Step = AStep FutureFrame

instance Functor future => Functor (AStep future) where
  map f (Step a e) = Step (f a) $ map f e

instance (Alt future, Apply future) => Apply (AStep future) where
  apply sf@(Step f ef) sa@(Step a ea) =
    Step (f a) (applyEventStep ef sa <|> applyStepEvent sf ea)

applyStepEvent
  :: forall future a b
   . Functor future
  => Alt future
  => Apply future
  => AStep future (a -> b)
  -> AnEvent future a
  -> AnEvent future b
applyStepEvent sf (Event fa) = Event $ (sf <*> _) <$> fa

applyEventStep
  :: forall future a b
   . Functor future
  => Alt future
  => Apply future
  => AnEvent future (a -> b)
  -> AStep future a
  -> AnEvent future b
applyEventStep (Event ff) stepA = Event $ (_ <*> stepA) <$> ff

instance Alternative future => Applicative (AStep future) where
  pure a = Step a empty

instance Alternative future => Bind (AStep future) where
  bind (Step a ea) k = case k a of
    Step b eb -> Step b $ eb <|> bindE ea k

bindE
  :: forall future a b
   . Functor future
  => Alternative future
  => AnEvent future a
  -> (a -> AStep future b)
  -> AnEvent future b
bindE (Event fa) k = Event $ map (join <<< (map k)) fa

instance Alternative future => Monad (AStep future)

instance (Lazy a, Lazy1 future) => Lazy (AStep future a) where
  defer f = Step (defer (\_ -> DL.force dlA)) (defer (\_ -> DL.force dlE))
    where
    dlStep = DL.defer f
    dlA = dlStep <#> case _ of Step a _ -> a
    dlE = dlStep <#> case _ of Step _ e -> e
