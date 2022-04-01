module Test.Data.Enum where

import Prelude

import Data.Enum (class Enum, pred, succ)
import Data.Maybe (Maybe(..))
import Test.QuickCheck ((<=?), (<?), (===))
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)

enumSpec :: forall a. Show a => Enum a => Gen a -> Spec Unit
enumSpec genA = describe "Enum instance" do
  it "obeys law: successor" do
    quickCheckGen' 1000 do
      a <- genA
      pure case succ a of
        Nothing -> true === true
        Just a' -> a <? a'
  it "obeys law: predecessor" do
    quickCheckGen' 1000 do
      a <- genA
      pure case pred a of
        Nothing -> true === true
        Just a' -> a' <? a
  it "obeys law: succ retracts pred" do
    quickCheckGen' 1000 do
      a <- genA
      pure $ (pred =<< succ =<< pred a) === pred a
  it "obeys law: pred retracts succ" do
    quickCheckGen' 1000 do
      a <- genA
      pure $ (succ =<< pred =<< succ a) === succ a
  it "obeys law: non-skipping succ" do
    quickCheckGen' 1000 do
      a <- genA
      b <- genA
      pure
        if b <= a then
          true === true
        else
          case succ a of
            Nothing -> true === true
            Just a' -> a' <=? b
  it "obeys law: non-skipping pred" do
    quickCheckGen' 1000 do
      a <- genA
      b <- genA
      pure
        if a <= b then
          true === true
        else
          case pred a of
            Nothing -> true === true
            Just a' -> b <=? a'
