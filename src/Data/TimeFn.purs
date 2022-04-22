module Data.TimeFn where

import Prelude

import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, left, right)
import Data.Profunctor.Strong (class Strong, first, second)

-- | A function over time represented as a data structure that has special
-- | cases for `const` and `lazy` functions. It is useful to be able to
-- | distinguish constant functions from other ones, because in practice, most
-- | behaviours are actually stepwise functions created from discrete events,
-- | and not continuous functions of time. In such cases, we want to avoid
-- | unnecessarily resampling a value we know will not change.
-- |
-- | The instances for `TimeFn` endeavour to preserve and create `K` functions
-- | as much as possible for this reason. This is also the reason why `TimeFn`
-- | values that are lazily evaluated have their own constructor rather than
-- | being defined using `defer f = F \time -> evalTimeFn (f unit) time`,
-- | because such a definition would make all lazy constant functions
-- | indistinguishable from continuous functions.
data TimeFn time a
  = K a
  | F (time -> a)

instance Semigroupoid TimeFn where
  compose (K c) _ = K c
  compose (F f) (K a) = K $ f a
  compose (F f) (F g) = F $ f <<< g

instance Category TimeFn where
  identity = F identity

derive instance Functor (TimeFn time)

instance Apply (TimeFn time) where
  apply (K f) (K a) = K $ f a
  apply (K f) (F a) = F $ f <$> a
  apply (F f) (K a) = F $ f <@> a
  apply (F f) (F a) = F $ f <*> a

instance Applicative (TimeFn time) where
  pure = K

instance Bind (TimeFn time) where
  bind (F f) k = F $ f >>= k >>> evalTimeFn
  bind (K a) k = k a

instance Monad (TimeFn time)

instance Profunctor TimeFn where
  dimap f g (F tf) = F $ dimap f g tf
  dimap _ g (K a) = K $ g a

instance Strong TimeFn where
  first (F f) = F $ first f
  first (K a) = F $ first $ const a
  second (F f) = F $ second f
  second (K a) = F $ second $ const a

instance Choice TimeFn where
  left (F f) = F $ left f
  left (K a) = F $ left $ const a
  right (F f) = F $ right f
  right (K a) = F $ right $ const a

-- | Evaluate a TimeFn at a specific time. Along with the `F` constructor,
-- | witnesses the isomorphism between `time -> a` and `TimeFn time a`.
evalTimeFn :: forall time a. TimeFn time a -> time -> a
evalTimeFn (F f) time = f time
evalTimeFn (K a) _ = a
