module Test.Control.Alternative where

import Prelude

import Control.Alternative (class Alternative, empty, (<|>))
import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Test.Spec (Spec, describe, it)

alternativeSpec
  :: forall t1 t2 o1 o2 f
   . Observable t1 o1 (f A)
  => Observable t2 o2 (f B)
  => Alternative f
  => (forall a. Gen a -> Gen (f a))
  -> Spec Unit
alternativeSpec gen = describe "Alternative instance" do
  it "obeys law: distributivity" do
    quickCheckGen' 1000 do
      a :: f A <- gen arbitrary
      f :: f (A -> B) <- gen arbitrary
      g :: f (A -> B) <- gen arbitrary
      pure $ ((f <|> g) <*> a) =-= (f <*> a <|> g <*> a)
  it "obeys law: annihilation" do
    quickCheckGen' 1000 do
      a :: f B <- gen arbitrary
      pure $ (empty <*> a) =-= (empty :: f B)
