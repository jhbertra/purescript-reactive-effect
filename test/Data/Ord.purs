module Test.Data.Ord where

import Prelude

import Data.Tuple (Tuple(..))
import Test.QuickCheck ((/==), (<=?), (===))
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)

ordSpec :: forall a. Show a => Ord a => Gen a -> Spec Unit
ordSpec gen = describe "Ord instance" do
  it "obeys law: reflexivity" do
    quickCheckGen' 1000 do
      a <- gen
      pure $ a <=? a
  it "obeys law: antisymmetry" do
    quickCheckGen' 1000 do
      Tuple a b <- Tuple <$> gen <*> gen
      pure
        if b <= a && a <= b then
          a === b
        else
          a /== b
  it "obeys law: transitivity" do
    quickCheckGen' 1000 do
      { a, b, c } <- { a: _, b: _, c: _ } <$> gen <*> gen <*> gen
      pure
        if a <= b && b <= c then
          a <=? c
        else
          true === true
