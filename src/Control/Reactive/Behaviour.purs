module Control.Reactive.Behaviour where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer, fix)
import Control.Lazy.Lazy1 (class Lazy1)
import Control.Monad.Fix (LazyRecord(..))
import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.Reactive.Event (AStep(..), AnEvent(..))
import Control.Reactive.Model (Future(..), Time(..))
import Control.Reactive.Moment (class MonadMoment, withMoment)
import Control.Reactive.Prim.Frame.Future (FutureFrame)
import Control.Reactive.TimeFn (TimeFn(..), evalTimeFn)
import Data.Compactable (compact, separate)
import Data.Either (Either(..))
import Data.Lazy (force)
import Data.List.Lazy.NonEmpty as LLN
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Safe.Coerce (coerce)

-------------------------------------------------------------------------------
-- Behaviours - semantically, `time -> a`.
--
-- The choice of representation for behaviours is based on a practical
-- observation: most behaviours are really stepwise functions of discretely
-- changing values rather than continuous functions of time. We can represent
-- both cases by using a stepwise function of continuous functions that change
-- over time (i.e. a peicewise function).
-- 
-- The resulting representation is practical both for representing "discrete"
-- behaviours, such as the time-varying state of a GUI widget, for which
-- push-based semantics are desirable, as well as "continuous" behaviours, such
-- as simulations, parametric animations, and signals, for which pull-based
-- semantics are preferable, as they allow sampling at arbitrary temporal
-- resolution.
--
-- Additionally, this representation makes it quite convenient to create
-- continuous functions from discrete functions, and vice-versa. For example,
-- given a discrete behaviour, one can create a continuous behaviour via
-- e.g. linear, quadratic, or cubic interpolation. One can also conveniently
-- perform calculus operations such as differentiation or integration using
-- numerical methods, as well as solving differential equations. Finally, one
-- could quantize a continuous behaviour by sampling it with an event.
-------------------------------------------------------------------------------

newtype ABehaviour future time a = Behaviour (AStep future (TimeFn time a))

type Behaviour = ABehaviour FutureFrame
type BehaviourModel = ABehaviour Future Time

derive newtype instance Lazy1 future => Lazy (ABehaviour future time a)

instance Lazy1 future => Lazy1 (ABehaviour future time) where
  defer1 = defer

derive instance Functor future => Functor (ABehaviour future time)

instance (Alt future, Apply future) => Apply (ABehaviour future time) where
  apply (Behaviour f) (Behaviour a) = Behaviour $ lift2 apply f a

instance Alternative future => Applicative (ABehaviour future time) where
  pure = Behaviour <<< pure <<< pure

instance Alternative future => Bind (ABehaviour future time) where
  bind (Behaviour step) k = Behaviour $ bindStep step
    where
    bindStep (Step tfa (Event ftfa)) = Step tfb etfb
      where
      tfb = tfa >>= k >>> case _ of (Behaviour (Step tfb' _)) -> tfb'
      etfb = Event $ bindStep <$> ftfa

applyE
  :: forall future time a b
   . Bounded time
  => MonadMoment time future
  => Alternative future
  => ABehaviour future time (a -> b)
  -> AnEvent future a
  -> AnEvent future b
applyE (Behaviour sf) (Event ea) = lift2EStep (uncurry <<< evalTimeFn) sf e'
  where
  e' = Event $ withMoment' bottom ea
  withMoment' lastTime fStep = do
    Tuple currentTime (Step a (Event ea')) <- withMoment fStep
    let t = max lastTime currentTime
    pure $ Step (Tuple t a) $ Event $ withMoment' t ea'

lift2EStep
  :: forall future a b c
   . Alternative future
  => (a -> b -> c)
  -> AStep future a
  -> AnEvent future b
  -> AnEvent future c
lift2EStep f (Step a (Event fa)) (Event fb) =
  Event $ lift2 (lift2 f) sa' fb
  where
  sa' = fa <|> pure a <$ fb

applyFirstE
  :: forall future time a b
   . Bounded time
  => MonadMoment time future
  => Alternative future
  => ABehaviour future time a
  -> AnEvent future b
  -> AnEvent future a
applyFirstE = applyE <<< map const

infixl 4 applyE as <&>
infixl 4 applyFirstE as <&

filterApply
  :: forall future time a
   . Bounded time
  => MonadMoment time future
  => Alternative future
  => Monad future
  => ABehaviour future time (a -> Boolean)
  -> AnEvent future a
  -> AnEvent future a
filterApply bp =
  compact <<< applyE ((\p a -> if p a then Just a else Nothing) <$> bp)

filterMapApply
  :: forall future time a b
   . Bounded time
  => MonadMoment time future
  => Alternative future
  => Monad future
  => ABehaviour future time (a -> Maybe b)
  -> AnEvent future a
  -> AnEvent future b
filterMapApply bp = compact <<< applyE (($) <$> bp)

partitionApply
  :: forall future time a
   . Bounded time
  => MonadMoment time future
  => Alternative future
  => Monad future
  => ABehaviour future time (a -> Boolean)
  -> AnEvent future a
  -> { yes :: AnEvent future a, no :: AnEvent future a }
partitionApply bp =
  relabel <<< separate <<< applyE
    ((\p a -> if p a then Right a else Left a) <$> bp)
  where
  relabel { left, right } = { yes: right, no: left }

partitionMapApply
  :: forall future time a b c
   . Bounded time
  => MonadMoment time future
  => Alternative future
  => Monad future
  => ABehaviour future time (a -> Either b c)
  -> AnEvent future a
  -> { left :: AnEvent future b, right :: AnEvent future c }
partitionMapApply bp = separate <<< applyE (($) <$> bp)

stepper
  :: forall future time a
   . Functor future
  => a
  -> AnEvent future a
  -> ABehaviour future time a
stepper a e = Behaviour $ Step (K a) (K <$> e)

switcher
  :: forall future time a
   . Alternative future
  => ABehaviour future time a
  -> AnEvent future (ABehaviour future time a)
  -> ABehaviour future time a
switcher a = join <<< stepper a

accumE
  :: forall time future a
   . Bounded time
  => MonadMoment time future
  => Alternative future
  => a
  -> AnEvent future (a -> a)
  -> AnEvent future a
accumE a e1 =
  let
    LazyRecord { e2 } = fix \(LazyRecord out) ->
      let
        e2 = ((#) <$> out.b) <&> e1
        b = stepper a e2 :: ABehaviour future time _
      in
        LazyRecord { e2, b }
  in
    e2

accumB
  :: forall time future a
   . Bounded time
  => MonadMoment time future
  => Alternative future
  => a
  -> AnEvent future (a -> a)
  -> ABehaviour future time a
accumB a = stepper a <<< accumE a

time :: forall time future. Plus future => ABehaviour future time time
time = Behaviour $ Step identity empty

denotationB :: forall a. BehaviourModel a -> Int -> a
denotationB (Behaviour step) t = evalTimeFn fn $ Time $ pure t
  where
  fn = tailRec iterStep step
  iterStep (Step fn' (Event (Future time' step')))
    | force (coerce (LLN.head time')) > t = Done fn'
    | LLN.head time' == top = Done fn'
    | otherwise = Loop $ force step'
