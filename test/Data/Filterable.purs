module Test.Data.Filterable where

import Prelude

import Control.Monad.Gen (elements)
import Data.Compactable (compact, separate)
import Data.Either (Either(..), isRight)
import Data.Filterable
  ( class Filterable
  , eitherBool
  , filter
  , filterMap
  , maybeBool
  , partition
  , partitionMap
  )
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen, repeatable)
import Test.QuickCheck.Laws (A, B, C)
import Test.Spec (Spec, describe, it)

filterableSpec
  :: forall t1 t2 t3 o1 o2 o3 f
   . Observable t1 o1 (f A)
  => Observable t2 o2 (f B)
  => Observable t3 o3 (f C)
  => Filterable f
  => (forall a. Arbitrary a => Gen (f a))
  -> Spec Unit
filterableSpec gen = describe "Filterable instance" do
  let
    toTuple :: forall a b. { left :: a, right :: b } -> Tuple a b
    toTuple { left, right } = Tuple left right

    toTuple2 :: forall a b. { no :: a, yes :: b } -> Tuple a b
    toTuple2 { no, yes } = Tuple no yes
  it "obeys law: filterMap functor relation" do
    quickCheckGen' 1000 do
      fa :: f (Maybe A) <- gen
      pure $ filterMap identity fa =-= compact fa
  it "obeys law: filterMap functor identity" do
    quickCheckGen' 1000 do
      fa :: f A <- gen
      pure $ filterMap Just fa =-= fa
  it "obeys law: derive filter" do
    quickCheckGen' 1000 do
      fa :: f A <- gen
      f :: (A -> Boolean) <- arbitrary
      pure $ filter f fa =-= filterMap (maybeBool f) fa
  it "obeys law: derive filterMap" do
    quickCheckGen' 1000 do
      fa :: f A <- gen
      p <- repeatable \(a :: A) -> elements $ Just a :| [ Nothing ]
      pure $ filterMap p fa =-= filter (isJust <<< p) fa
  it "obeys law: kleisli composition" do
    quickCheckGen' 1000 do
      fa :: f A <- gen
      f :: (B -> Maybe C) <- arbitrary
      g :: (A -> Maybe B) <- arbitrary
      pure $ filterMap (f <=< g) fa =-= filterMap f (filterMap g fa)
  it "obeys law: partitionMap functor relation" do
    quickCheckGen' 1000 do
      fab :: f (Either A B) <- gen
      pure $ toTuple (partitionMap identity fab) =-= toTuple (separate fab)
  it "obeys law: partitionMap functor identity left" do
    quickCheckGen' 1000 do
      fa :: f A <- gen
      pure $ (partitionMap Left fa).left =-= fa
  it "obeys law: partitionMap functor identity right" do
    quickCheckGen' 1000 do
      fa :: f A <- gen
      pure $ (partitionMap Right fa).right =-= fa
  it "obeys law: derive partition" do
    quickCheckGen' 1000 do
      fa :: f A <- gen
      f :: (A -> Boolean) <- arbitrary
      pure $
        toTuple2 (partition f fa) =-= toTuple (partitionMap (eitherBool f) fa)
  it "obeys law: derive partitionMap" do
    quickCheckGen' 1000 do
      fa :: f A <- gen
      p <- repeatable \(a :: A) -> elements $ Left a :| [ Right a ]
      pure $
        toTuple (partitionMap p fa) =-= toTuple2 (partition (isRight <<< p) fa)
