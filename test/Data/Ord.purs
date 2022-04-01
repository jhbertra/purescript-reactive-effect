module Test.Data.Ord where

import Prelude

import Test.QuickCheck ((<=?), (===))
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen, suchThat)
import Test.Spec (Spec, describe, it, parallel)

ordSpec :: forall a. Show a => Ord a => Gen a -> Spec Unit
ordSpec genA = describe "Ord instance" do
  it "obeys law: reflexivity" do
    quickCheckGen' 1000 do
      a <- genA
      pure $ a <=? a
  it "obeys law: antisymmetry" do
    quickCheckGen' 1000 do
      a <- genA
      b <- genA `suchThat` \b -> b <= a && a <= b
      pure $ a === b
  it "obeys law: transitivity" do
    quickCheckGen' 1000 do
      a <- genA
      b <- genA `suchThat` (a <= _)
      c <- genA `suchThat` (b <= _)
      pure $ a <=? c
