module Effect.Reactive.Model where

import Prelude

import Control.Monad.Reader (Reader, runReader)
import Data.List as Strict
import Data.List.Lazy (List, repeat)
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong ((&&&))
import Data.Unfoldable (unfoldr)

type Time = Int
newtype Raff a = Raff (Reader Time a)

derive instance Newtype (Raff a) _
derive instance Functor Raff
derive newtype instance Apply Raff
derive newtype instance Applicative Raff
derive newtype instance Bind Raff
derive newtype instance Monad Raff

newtype Event a = Event (List (Maybe a))
newtype Behaviour a = Behaviour (List a)

derive instance Newtype (Event a) _
derive instance Functor Event

derive instance Newtype (Behaviour a) _
derive instance Functor Behaviour

interpretE
  :: forall a b
   . (Event a -> Raff (Event b))
  -> Strict.List (Maybe a)
  -> Strict.List (Maybe b)
interpretE f as = Strict.fromFoldable
  $ Lazy.take (Strict.length as)
  $ unwrap
  $ flip runReader 0
  $ unwrap
  $ f
  $ Event
  $ Strict.toUnfoldable as <> repeat Nothing

interpretB
  :: forall a b
   . (Event a -> Raff (Behaviour b))
  -> Strict.List (Maybe a)
  -> Strict.List b
interpretB f as = Strict.fromFoldable
  $ Lazy.take (Strict.length as)
  $ unwrap
  $ flip runReader 0
  $ unwrap
  $ f
  $ Event
  $ Strict.toUnfoldable as <> repeat Nothing

timeB :: Behaviour Time
timeB = Behaviour $ unfoldr (Just <<< (identity &&& (_ + 1))) 0
