module Test.Data.Monoid where

import Prelude

import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)

monoidSpec :: forall t o a. Observable t o a => Monoid a => Gen a -> Spec Unit
monoidSpec genA = describe "Monoid instance" do
  it "obeys law: left identity" do
    quickCheckGen' 1000 do
      a <- genA
      pure $ mempty <> a =-= a
  it "obeys law: right identity" do
    quickCheckGen' 1000 do
      a <- genA
      pure $ a <> mempty =-= a
