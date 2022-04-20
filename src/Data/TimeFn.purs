module Data.TimeFn where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1)
import Data.Lazy (force)
import Data.Lazy as DL
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
  = L (DL.Lazy (TimeFn time a))
  | K a
  | F (time -> a)

instance Lazy (TimeFn time a) where
  defer = L <<< DL.defer

instance Lazy1 (TimeFn time) where
  defer1 = defer

instance Semigroupoid TimeFn where
  compose (L f) (L g) = L $ DL.defer \_ -> force f <<< force g
  compose (L f) g = L $ DL.defer \_ -> force f <<< g
  compose f (L g) = L $ DL.defer \_ -> f <<< force g
  compose (K c) _ = K c
  compose (F f) (K a) = K $ f a
  compose (F f) (F g) = F $ f <<< g

instance Category TimeFn where
  identity = F identity

derive instance Functor (TimeFn time)

instance Apply (TimeFn time) where
  apply (L f) (L a) = L $ DL.defer \_ -> force f <*> force a
  apply (L f) a = L $ DL.defer \_ -> force f <*> a
  apply f (L a) = L $ DL.defer \_ -> f <*> force a
  apply (K f) (K a) = K $ f a
  apply (K f) (F a) = F $ f <$> a
  apply (F f) (K a) = F $ f `flap` a
  apply (F f) (F a) = F $ f <*> a

instance Applicative (TimeFn time) where
  pure = K

instance Bind (TimeFn time) where
  bind (L tf) k = L $ DL.defer \_ -> force tf >>= k
  bind (F f) k = F $ f >>= k >>> evalTimeFn
  bind (K a) k = k a

instance Monad (TimeFn time)

instance Profunctor TimeFn where
  dimap f g (L tf) = L $ DL.defer \_ -> dimap f g $ force tf
  dimap f g (F tf) = F $ dimap f g tf
  dimap _ g (K a) = K $ g a

instance Strong TimeFn where
  first (L tf) = L $ DL.defer \_ -> first $ force tf
  first (F f) = F $ first f
  first (K a) = F $ first $ const a
  second (L tf) = L $ DL.defer \_ -> second $ force tf
  second (F f) = F $ second f
  second (K a) = F $ second $ const a

instance Choice TimeFn where
  left (L tf) = L $ DL.defer \_ -> left $ force tf
  left (F f) = F $ left f
  left (K a) = F $ left $ const a
  right (L tf) = L $ DL.defer \_ -> right $ force tf
  right (F f) = F $ right f
  right (K a) = F $ right $ const a

-- | Evaluate a TimeFn at a specific time. Along with the `F` constructor,
-- | witnesses the isomorphism between `time -> a` and `TimeFn time a`.
evalTimeFn :: forall time a. TimeFn time a -> time -> a
evalTimeFn (L tf) time = evalTimeFn (DL.force tf) time
evalTimeFn (F f) time = f time
evalTimeFn (K a) _ = a

-- | Evaluate a TimeFn at a specific time lazily. The function will not be
-- | evaluated until the return value is "forced".
evalTimeFnLazy :: forall time a. Lazy a => TimeFn time a -> time -> a
evalTimeFnLazy tf time = defer \_ -> evalTimeFn tf time
