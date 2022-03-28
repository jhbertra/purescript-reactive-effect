module Control.Reactive.Behaviour where

import Prelude hiding ((<@>))

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1)
import Control.Reactive.Event (AStep(..), AnEvent(..))
import Control.Reactive.Prim.Frame.Future (FutureFrame)
import Control.Reactive.TimeFn (TimeFn, evalTimeFn)
import Data.Compactable (compact, separate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Ord.Max (Max(..))
import Data.Tuple (Tuple(..), uncurry)

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

newtype ABehaviour future time a = Behaviour (AStep future time (TimeFn time a))

type Behaviour = ABehaviour FutureFrame

derive newtype instance
  ( Lazy time
  , Lazy1 future
  ) =>
  Lazy (ABehaviour future time a)

instance (Lazy time, Lazy1 future) => Lazy1 (ABehaviour future time) where
  defer1 = defer

derive instance Functor future => Functor (ABehaviour future time)

instance (Ord time, Alt future, Apply future) => Apply (ABehaviour future time) where
  apply (Behaviour f) (Behaviour a) = Behaviour $ lift2 apply f a

instance
  ( Bounded time
  , Alternative future
  ) =>
  Applicative (ABehaviour future time) where
  pure = Behaviour <<< pure <<< pure

applyE
  :: forall future time a b
   . Bounded time
  => Alternative future
  => ABehaviour future time (a -> b)
  -> AnEvent future time a
  -> AnEvent future time b
applyE (Behaviour sf) (Event fa) = lift2EStep (uncurry <<< evalTimeFn) sf e'
  where
  e' = Event $ map withTime fa
  withTime (Step (Max t) a (Event f)) =
    Step (Max t) (Tuple t a) $ Event $ withTime <$> f

lift2EStep
  :: forall future time a b c
   . Bounded time
  => Alternative future
  => (a -> b -> c)
  -> AStep future time a
  -> AnEvent future time b
  -> AnEvent future time c
lift2EStep f (Step _ a (Event fa)) (Event fb) =
  Event $ lift2 (lift2 f) sa' fb
  where
  sa' = fa <|> pure a <$ fb

applyFirstE
  :: forall future time a b
   . Bounded time
  => Alternative future
  => ABehaviour future time a
  -> AnEvent future time b
  -> AnEvent future time a
applyFirstE = applyE <<< map const

infixl 4 applyE as <@>
infixl 4 applyFirstE as <@

filterApply
  :: forall future time a
   . Bounded time
  => Alternative future
  => Monad future
  => ABehaviour future time (a -> Boolean)
  -> AnEvent future time a
  -> AnEvent future time a
filterApply bp =
  compact <<< applyE ((\p a -> if p a then Just a else Nothing) <$> bp)

filterMapApply
  :: forall future time a b
   . Bounded time
  => Alternative future
  => Monad future
  => ABehaviour future time (a -> Maybe b)
  -> AnEvent future time a
  -> AnEvent future time b
filterMapApply bp = compact <<< applyE (($) <$> bp)

partitionApply
  :: forall future time a
   . Bounded time
  => Alternative future
  => Monad future
  => ABehaviour future time (a -> Boolean)
  -> AnEvent future time a
  -> { yes :: AnEvent future time a, no :: AnEvent future time a }
partitionApply bp =
  relabel <<< separate <<< applyE
    ((\p a -> if p a then Right a else Left a) <$> bp)
  where
  relabel { left, right } = { yes: right, no: left }

partitionMapApply
  :: forall future time a b c
   . Bounded time
  => Alternative future
  => Monad future
  => ABehaviour future time (a -> Either b c)
  -> AnEvent future time a
  -> { left :: AnEvent future time b, right :: AnEvent future time c }
partitionMapApply bp = separate <<< applyE (($) <$> bp)
