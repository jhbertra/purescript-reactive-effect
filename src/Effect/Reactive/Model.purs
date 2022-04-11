module Effect.Reactive.Model where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Plus (empty)
import Data.Align (class Align, class Alignable, align)
import Data.Foldable (find)
import Data.List (List(..), dropWhile, foldl, takeWhile, (:))
import Data.Maybe (Maybe, fromMaybe)
import Data.These (These(..), these)
import Data.Tuple (Tuple(..), fst, snd)
import Safe.Coerce (coerce)
import Test.Data.Observe (class Observable, observe)

type Time = Int
newtype Raff a = Raff (Reader Time a)
newtype RVar a = RVar (List (Tuple Time a))

derive instance Functor RVar

instance Observable t o a => Observable t (List (Tuple Time o)) (RVar a) where
  observe t (RVar occs) = map (observe t) <$> occs

instance Apply RVar where
  apply (RVar rf) (RVar ra) = RVar $ go rf ra Nil Nil
    where
    go Nil Nil _ _ = Nil
    go Nil _ Nil _ = Nil
    go _ Nil _ Nil = Nil
    go Nil (Tuple t a : as) (f : _) _ =
      Tuple t (f a) : go Nil as (pure f) (pure a)
    go (Tuple t f : fs) Nil _ (a : _) =
      Tuple t (f a) : go fs Nil (pure f) (pure a)
    go fs@(Tuple t1 f : fs') as@(Tuple t2 a : as') mf ma = case compare t1 t2 of
      LT -> (Tuple t1 <$> (f <$> ma)) <> go fs' as (pure f) ma
      GT -> (Tuple t2 <$> (mf <@> a)) <> go fs as' mf (pure a)
      EQ -> Tuple t1 (f a) : go fs' as' (pure f) (pure a)

instance Align RVar where
  align f (RVar as) (RVar Nil) = RVar $ map (f <<< This) <$> as
  align f (RVar Nil) (RVar bs) = RVar $ map (f <<< That) <$> bs
  align f ra@(RVar (Tuple t1 a : as)) rb@(RVar (Tuple t2 b : bs)) = RVar
    case compare t1 t2 of
      GT -> Tuple t2 (f $ That b) : coerce (align f ra (RVar bs))
      LT -> Tuple t1 (f $ This a) : coerce (align f (RVar as) rb)
      EQ -> Tuple t1 (f $ Both a b) : coerce (align f (RVar as) (RVar bs))

instance Applicative RVar where
  pure = RVar <<< pure <<< Tuple bottom

instance Alignable RVar where
  nil = RVar empty

instance Semigroup a => Semigroup (RVar a) where
  append = lift2 append

instance Monoid a => Monoid (RVar a) where
  mempty = pure mempty

read :: forall a. RVar a -> Raff (Maybe a)
read (RVar as) = Raff do
  t <- ask
  pure $ snd <$> find ((_ <= t) <<< fst) as

lastUpdate :: forall a. RVar a -> Raff Time
lastUpdate (RVar as) = Raff do
  t <- ask
  pure $ fromMaybe top $ fst <$> find ((_ <= t) <<< fst) as

execute :: forall a. RVar (Raff a) -> RVar a
execute = executeMap identity

executeMap :: forall a b. (a -> Raff b) -> RVar a -> RVar b
executeMap f (RVar as) =
  RVar $ as <#> \(Tuple t a) -> Tuple t $ runReader (coerce f a) t

switch :: forall a. RVar (RVar a) -> RVar a
switch = switchMap identity

switchMap :: forall a b. (a -> RVar b) -> RVar a -> RVar b
switchMap k (RVar as) = RVar $ foldl switch' Nil as
  where
  switch' bs (Tuple t a) = case k a of
    RVar bs' ->
      takeWhile ((_ < t) <<< fst) bs <> dropWhile ((_ < t) <<< fst) bs'

unionWith :: forall a. (a -> a -> a) -> RVar a -> RVar a -> RVar a
unionWith = align <<< these identity identity
