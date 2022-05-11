module Test.Control.Monad where

import Prelude

import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck (class Arbitrary, coarbitrary)
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen, repeatable)
import Test.QuickCheck.Laws (A, B)
import Test.Spec (Spec, describe, it)

monadSpec
  :: forall t1 t2 o1 o2 f
   . Observable t1 o1 (f A)
  => Observable t2 o2 (f B)
  => Monad f
  => (forall a. Arbitrary a => Gen (f a))
  -> Spec Unit
monadSpec gen = describe "Monad instance" do
  it "obeys law: left identity" do
    quickCheckGen' 1000 do
      f :: (A -> f B) <- repeatable \a -> coarbitrary a gen
      pure \x -> (pure x >>= f) =-= f x
  it "obeys law: right identity" do
    quickCheckGen' 1000 do
      m :: f A <- gen
      pure $ (m >>= pure) =-= m
