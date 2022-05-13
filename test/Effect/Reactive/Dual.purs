module Test.Effect.Reactive.Dual where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (empty)
import Control.Lazy (defer)
import Control.Monad.Fix (class MonadFix, mfix)
import Control.Plus (class Plus)
import Data.Align (class Align, class Alignable, align, nil)
import Data.Array as Array
import Data.Compactable (class Compactable, compact, separate)
import Data.Filterable
  ( class Filterable
  , filter
  , filterMap
  , partition
  , partitionMap
  )
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Reactive as R
import Test.Effect.Reactive.Model as M

data Event (t :: R.Timeline) a = E (M.Event a) (R.Event t a)
data Behaviour (t :: R.Timeline) a = B (M.Behaviour a) (R.Behaviour t a)
data Raff (t :: R.Timeline) a = R (M.Raff a) (R.Raff t a)

derive instance Functor (Raff t)
instance Apply (Raff t) where
  apply (R mf rf) (R ma ra) = R (mf <*> ma) (rf <*> ra)

instance Applicative (Raff t) where
  pure a = R (pure a) (pure a)

instance Bind (Raff t) where
  bind (R m r) k = R
    (m >>= k >>> case _ of R m' _ -> m')
    (r >>= k >>> case _ of R _ r' -> r')

instance Monad (Raff t)

instance MonadFix (Raff t) where
  mfix f = R (mfix fm) (mfix fr)
    where
    fm a = let R m _ = f a in m
    fr a = let R _ r = f a in r

instance Functor (Event t) where
  map f (E m e) = E (map f m) (map f e)

instance Compactable (Event t) where
  compact (E m e) = E (compact m) (compact e)
  separate (E m e) =
    let
      msep = separate m
      esep = separate e
    in
      { left: E msep.left esep.left, right: E msep.right esep.right }

instance Filterable (Event t) where
  filter p (E m e) = E (filter p m) (filter p e)
  filterMap f (E m e) = E (filterMap f m) (filterMap f e)
  partition p (E m e) =
    let
      mpart = partition p m
      epart = partition p e
    in
      { no: E mpart.no epart.no, yes: E mpart.yes epart.yes }
  partitionMap f (E m e) =
    let
      mpart = partitionMap f m
      epart = partitionMap f e
    in
      { left: E mpart.left epart.left, right: E mpart.right epart.right }

instance Alt (Event t) where
  alt (E m1 e1) (E m2 e2) = E (m1 <|> m2) (e1 <|> e2)

instance Plus (Event t) where
  empty = E empty empty

instance Apply (Event t) where
  apply (E m1 e1) (E m2 e2) = E (m1 <*> m2) (e1 <*> e2)

instance Align (Event t) where
  align f (E m1 e1) (E m2 e2) = E (align f m1 m2) (align f e1 e2)

instance Alignable (Event t) where
  nil = E nil nil

instance Semigroup a => Semigroup (Event t a) where
  append (E m1 e1) (E m2 e2) = E (append m1 m2) (append e1 e2)

instance Semigroup a => Monoid (Event t a) where
  mempty = E mempty mempty

interpretModel
  :: forall t a b
   . (Event t a -> Raff t (Event t b))
  -> Array (Maybe a)
  -> Array (Maybe b)
interpretModel f = interpretModel2 (\_ ea -> f ea) []

interpretModel2
  :: forall t a b c
   . (Event t a -> Event t b -> Raff t (Event t c))
  -> Array (Maybe a)
  -> Array (Maybe b)
  -> Array (Maybe c)
interpretModel2 f as bs = Array.fromFoldable $
  M.interpretE2
    ( \e1 e2 -> map (\(E m _) -> m)
        $ (\(R m _) -> m)
        $ f
            (E e1 (defer \_ -> unsafeThrow "undefined"))
            (E e2 (defer \_ -> unsafeThrow "undefined"))
    )
    (Array.toUnfoldable as)
    (Array.toUnfoldable bs)

interpretEffect
  :: forall a b
   . (forall t. Event t a -> Raff t (Event t b))
  -> Array (Maybe a)
  -> Effect (Array (Maybe b))
interpretEffect f = interpretEffect2 (\_ ea -> f ea) []

interpretEffect2
  :: forall a b c
   . (forall t. Event t a -> Event t b -> Raff t (Event t c))
  -> Array (Maybe a)
  -> Array (Maybe b)
  -> Effect (Array (Maybe c))
interpretEffect2 f = R.interpret2
  ( \e1 e2 -> map (\(E _ e) -> e)
      $ (\(R _ m) -> m)
      $ f
          (E (defer \_ -> unsafeThrow "undefined") e1)
          (E (defer \_ -> unsafeThrow "undefined") e2)
  )
