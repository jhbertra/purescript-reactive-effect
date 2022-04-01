module Test.Data.Semigroup where

import Prelude

import Test.QuickCheck ((===))
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)

semigroupSpec :: forall a. Eq a => Show a => Semigroup a => Gen a -> Spec Unit
semigroupSpec genA = describe "Semigroup instance" do
  it "obeys law: associativity" do
    quickCheckGen' 1000 do
      a <- genA
      b <- genA
      c <- genA
      pure $ (a <> b) <> c === a <> (b <> c)
