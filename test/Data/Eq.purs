module Test.Data.Eq where

import Prelude

import Test.QuickCheck ((===))
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen, suchThat)
import Test.Spec (Spec, describe, it)

eqSpec :: forall a. Show a => Eq a => Gen a -> Spec Unit
eqSpec genA = describe "Eq instance" do
  it "obeys law: reflexivity" do
    quickCheckGen' 1000 do
      a <- genA
      pure $ a === a
  it "obeys law: symmetry" do
    quickCheckGen' 1000 do
      a <- genA
      b <- genA
      pure $ (a == b) === (b == a)
  it "obeys law: transitivity" do
    quickCheckGen' 1000 do
      a <- genA
      b <- genA `suchThat` eq a
      c <- genA `suchThat` eq b
      pure $ a == c
  it "obeys law: negation" do
    quickCheckGen' 1000 do
      a <- genA
      b <- genA
      pure $ (a /= b) === not (a == b)
