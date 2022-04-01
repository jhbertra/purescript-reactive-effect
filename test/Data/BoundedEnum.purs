module Test.Data.BoundedEnum where

import Prelude

import Data.Array (replicate)
import Data.Enum (class BoundedEnum, cardinality, pred, succ)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

boundedEnumSpec :: forall a. Show a => BoundedEnum a => Gen a -> Spec Unit
boundedEnumSpec genA = describe "BoundedEnum instance" do
  let
    c :: Int
    c = unwrap (cardinality :: _ a)
  it "obeys law: succ" do
    shouldEqual
      (Just top :: _ a)
      (foldl (>>=) (pure bottom) (replicate (c - 1) succ))
  it "obeys law: pred" do
    shouldEqual
      (Just bottom :: _ a)
      (foldl (>>=) (pure top) (replicate (c - 1) pred))
