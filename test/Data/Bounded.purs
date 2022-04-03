module Test.Data.Bounded where

import Prelude

import Test.Data.Observe (class Observable, (<=-?))
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)

boundedSpec :: forall t o a. Observable t o a => Bounded a => Gen a -> Spec Unit
boundedSpec genA = describe "Bounded instance" do
  it "obeys law: ordering lower bound" do
    quickCheckGen' 1000 do
      a <- genA
      pure $ bottom <=-? a
  it "obeys law: ordering upper bound" do
    quickCheckGen' 1000 do
      a <- genA
      pure $ a <=-? top
