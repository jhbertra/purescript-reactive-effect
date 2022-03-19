module Control.Reactive.Event where

import Prelude

import Control.Lazy.Lazy1 (class Lazy1)
import Data.Align (class Align, class Alignable, align, nil)
import Data.Filterable (class Filterable)
import Data.Foldable (class Foldable, foldr)
import Data.These (These(..))

class (Filterable f, Alignable f, Lazy1 f) <= Event f

unionWith :: forall f a. Align f => (a -> a -> a) -> f a -> f a -> f a
unionWith f = align case _ of
  This a -> a
  That a -> a
  Both a b -> f a b

unions
  :: forall t f a. Alignable f => Foldable t => t (f (a -> a)) -> f (a -> a)
unions = foldr (unionWith (<<<)) nil
