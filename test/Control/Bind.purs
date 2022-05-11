module Test.Control.Bind where

import Prelude

import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck (class Arbitrary, coarbitrary)
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen, repeatable)
import Test.QuickCheck.Laws (A, B, C)
import Test.Spec (Spec, describe, it)

bindSpec
  :: forall t1 t2 t3 o1 o2 o3 f
   . Observable t1 o1 (f A)
  => Observable t2 o2 (f B)
  => Observable t3 o3 (f C)
  => Bind f
  => (forall a. Arbitrary a => Gen (f a))
  -> Spec Unit
bindSpec gen = describe "Bind instance" do
  it "obeys law: associativity" do
    quickCheckGen' 1000 do
      m :: f A <- gen
      f :: (A -> f B) <- repeatable \a -> coarbitrary a gen
      g :: (B -> f C) <- repeatable \b -> coarbitrary b gen
      pure $ ((m >>= f) >>= g) =-= (m >>= (f >=> g))
