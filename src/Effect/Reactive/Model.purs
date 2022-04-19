module Effect.Reactive.Model where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Plus)
import Control.Monad.Reader (Reader, ReaderT(..), runReader)
import Control.Plus (empty)
import Data.Align (class Align, class Alignable, align)
import Data.Compactable (class Compactable, separateDefault)
import Data.Filterable
  ( class Filterable
  , filterDefault
  , filterMapDefault
  , partitionDefault
  , partitionMapDefault
  )
import Data.List as Strict
import Data.List.Lazy (List, repeat, (!!))
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.These (these)
import Safe.Coerce (coerce)

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
instance Compactable Event where
  compact = over Event $ map join
  separate e = separateDefault e

instance Filterable Event where
  filter p = filterDefault p
  filterMap f = filterMapDefault f
  partition p = partitionDefault p
  partitionMap f = partitionMapDefault f

instance Alt Event where
  alt (Event a) (Event b) = Event $ Lazy.zipWith (<|>) a b

instance Apply Event where
  apply (Event f) (Event a) = Event $ Lazy.zipWith apply f a

instance Plus Event where
  empty = Event $ Lazy.repeat Nothing

instance Align Event where
  align f (Event a) (Event b) = Event $ Lazy.zipWith (align f) a b

instance Alignable Event where
  nil = empty

instance Semigroup a => Semigroup (Event a) where
  append = align $ these identity identity append

instance Semigroup a => Monoid (Event a) where
  mempty = empty

derive instance Newtype (Behaviour a) _
derive instance Functor Behaviour
instance Apply Behaviour where
  apply (Behaviour f) (Behaviour a) = Behaviour $ Lazy.zipWith ($) f a

instance Applicative Behaviour where
  pure = Behaviour <<< Lazy.repeat

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

interpretE2
  :: forall a b c
   . (Event a -> Event b -> Raff (Event c))
  -> Strict.List (Maybe a)
  -> Strict.List (Maybe b)
  -> Strict.List (Maybe c)
interpretE2 f as bs = Strict.fromFoldable
  $ Lazy.take (max (Strict.length as) (Strict.length bs))
  $ unwrap
  $ flip runReader 0
  $ unwrap
  $ f
      (Event $ Strict.toUnfoldable as <> repeat Nothing)
      (Event $ Strict.toUnfoldable bs <> repeat Nothing)

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

interpretB2
  :: forall a b c
   . (Event a -> Event b -> Raff (Behaviour c))
  -> Strict.List (Maybe a)
  -> Strict.List (Maybe b)
  -> Strict.List c
interpretB2 f as bs = Strict.fromFoldable
  $ Lazy.take (max (Strict.length as) (Strict.length bs))
  $ unwrap
  $ flip runReader 0
  $ unwrap
  $ f
      (Event $ Strict.toUnfoldable as <> repeat Nothing)
      (Event $ Strict.toUnfoldable bs <> repeat Nothing)

timeB :: Behaviour Time
timeB = Behaviour $ Lazy.iterate (_ + 1) 0

forgetE :: forall a. Time -> Event a -> List (Maybe a)
forgetE time = Lazy.drop time <<< coerce

stepper :: forall a. a -> Event a -> Raff (Behaviour a)
stepper i e = Raff $ ReaderT \time ->
  pure $ Behaviour $ Lazy.replicate time i <> step i (forgetE time e)
  where
  step a (Lazy.List lstep) = lstep
    # map case _ of
        Lazy.Nil -> Lazy.Nil
        Lazy.Cons ma mas ->
          let
            next = fromMaybe a ma
          in
            Lazy.Cons next $ step next mas
    # Lazy.List

switchB :: forall a. Behaviour a -> Event (Behaviour a) -> Raff (Behaviour a)
switchB b e = diagonalB <$> stepper b e

diagonalB :: forall a. Behaviour (Behaviour a) -> Behaviour a
diagonalB = over Behaviour
  $ Lazy.catMaybes
      <<< Lazy.zipWith (\time xs -> xs !! time) (Lazy.iterate (_ + 1) 0)
      <<< map unwrap

applyE :: forall a b. Behaviour (a -> b) -> Event a -> Event b
applyE = over Event <<< Lazy.zipWith map <<< unwrap
