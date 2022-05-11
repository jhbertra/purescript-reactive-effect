module Test.Control.Apply where

import Prelude

import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C)
import Test.Spec (Spec, describe, it)

applySpec
  :: forall t o f
   . Observable t o (f C)
  => Apply f
  => (forall a. Arbitrary a => Gen (f a))
  -> Spec Unit
applySpec gen = describe "Apply instance" do
  it "obeys law: composition" do
    quickCheckGen' 1000 do
      f :: f (B -> C) <- gen
      g :: f (A -> B) <- gen
      x :: f A <- gen
      pure $ ((<<<) <$> f <*> g <*> x) =-= (f <*> (g <*> x))
