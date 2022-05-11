module Test.Control.Applicative where

import Prelude

import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck')

applicativeSpec
  :: forall t1 t2 t3 o1 o2 o3 f
   . Observable t1 o1 (f A)
  => Observable t2 o2 (f B)
  => Observable t3 o3 (f C)
  => Applicative f
  => (forall a. Arbitrary a => Gen (f a))
  -> Spec Unit
applicativeSpec gen = describe "Applicative instance" do
  it "obeys law: identity" do
    quickCheckGen' 1000 do
      a :: f A <- gen
      pure $ (pure identity <*> a) =-= a
  it "obeys law: composition" do
    quickCheckGen' 1000 do
      f :: f (B -> C) <- gen
      g :: f (A -> B) <- gen
      x :: f A <- gen
      pure $ (pure (<<<) <*> f <*> g <*> x) =-= (f <*> (g <*> x))
  it "obeys law: homomorphism" do
    quickCheck' 1000 \f (x :: A) -> (pure f <*> pure x) =-= (pure (f x) :: f B)
  it "obeys law: interchange" do
    quickCheckGen' 1000 do
      f :: f (A -> B) <- gen
      pure \a -> (f <*> pure a) =-= (pure (_ $ a) <*> f)
