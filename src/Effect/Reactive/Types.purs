module Effect.Reactive.Types where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)

foreign import data Timeline :: Type

newtype Time (t :: Timeline) = Time Number

derive instance Newtype (Time t) _
derive newtype instance Show (Time t)
derive newtype instance Eq (Time t)
derive newtype instance Ord (Time t)
derive newtype instance Bounded (Time t)

instance Semigroup (Time t) where
  append (Time a) (Time b) = Time $ max a b

instance Monoid (Time t) where
  mempty = Time bottom

type Canceller = Effect Unit
