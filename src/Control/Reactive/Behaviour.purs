module Control.Reactive.Behaviour where

import Prelude hiding ((<@>))

import Control.Lazy.Lazy1 (class Lazy1)
import Control.Reactive.Event (class Event)
import Data.Compactable (compact, separate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

class (Event e, Applicative f, Lazy1 f) <= Behaviour e f where
  applyE :: forall a b. f (a -> b) -> e a -> e b

applyFirstE
  :: forall event behaviour b a
   . Behaviour event behaviour
  => behaviour a
  -> event b
  -> event a
applyFirstE = applyE <<< map const

infixl 4 applyE as <@>
infixl 4 applyFirstE as <@

filterApply
  :: forall event behaviour a
   . Behaviour event behaviour
  => behaviour (a -> Boolean)
  -> event a
  -> event a
filterApply bp =
  compact <<< applyE ((\p a -> if p a then Just a else Nothing) <$> bp)

filterMapApply
  :: forall event behaviour a b
   . Behaviour event behaviour
  => behaviour (a -> Maybe b)
  -> event a
  -> event b
filterMapApply bp = compact <<< applyE (($) <$> bp)

partitionApply
  :: forall event behaviour a
   . Behaviour event behaviour
  => behaviour (a -> Boolean)
  -> event a
  -> { yes :: event a, no :: event a }
partitionApply bp =
  relabel <<< separate <<< applyE
    ((\p a -> if p a then Right a else Left a) <$> bp)
  where
  relabel { left, right } = { yes: right, no: left }

partitionMapApply
  :: forall event behaviour a b c
   . Behaviour event behaviour
  => behaviour (a -> Either b c)
  -> event a
  -> { left :: event b, right :: event c }
partitionMapApply bp = separate <<< applyE (($) <$> bp)
