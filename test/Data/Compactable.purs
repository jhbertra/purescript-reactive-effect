module Test.Data.Compactable where

import Prelude

import Control.Plus (class Plus, empty)
import Data.Compactable (class Compactable, compact, separate, separateDefault)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck')

compactableFunctorSpec
  :: forall t1 t2 o1 o2 f
   . Observable t1 o1 (f A)
  => Observable t2 o2 (f B)
  => Functor f
  => Compactable f
  => (forall a. Gen a -> Gen (f a))
  -> Spec Unit
compactableFunctorSpec gen = describe "Compactable + Functor instance" do
  it "obeys law: functor identity" do
    quickCheckGen' 1000 do
      fa :: f A <- gen arbitrary
      pure $ compact (map Just fa) =-= fa
  it "obeys law: derive separate" do
    quickCheckGen' 1000 do
      fab :: f (Either A B) <- gen arbitrary
      let toTuple { left, right } = Tuple left right
      pure $ (toTuple $ separate fab) =-= (toTuple $ separateDefault fab)

compactablePlusSpec
  :: forall t o f
   . Observable t o (f A)
  => Plus f
  => Compactable f
  => (forall a. Gen a -> Gen (f a))
  -> Spec Unit
compactablePlusSpec gen = describe "Compactable + Plus instance" do
  it "obeys law: empty identity" do
    quickCheck' 1 $ compact empty =-= (empty :: f A)
  it "obeys law: compact empty" do
    quickCheckGen' 1000 do
      fa :: f B <- gen arbitrary
      pure $ compact (Nothing <$ fa) =-= (empty :: f A)
