module Test.Data.Eq where

import Prelude

import Data.Tuple (Tuple(..))
import Test.QuickCheck ((===))
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)

eqSpec :: forall a. Show a => Eq a => Gen a -> Spec Unit
eqSpec gen = describe "Eq instance" do
  it "obeys law: reflexivity" do
    quickCheckGen' 1000 do
      a <- gen
      pure $ a === a
  it "obeys law: symmetry" do
    quickCheckGen' 1000 do
      Tuple a b <- Tuple <$> gen <*> gen
      pure $ (a == b) === (b == a)
  it "obeys law: transitivity" do
    quickCheckGen' 1000 do
      { a, b, c } <- { a: _, b: _, c: _ } <$> gen <*> gen <*> gen
      pure
        if a == b && b == c then
          a === c
        else
          true === true
  it "obeys law: negation" do
    quickCheckGen' 1000 do
      Tuple a b <- Tuple <$> gen <*> gen
      pure $ (a /= b) === not (a == b)
