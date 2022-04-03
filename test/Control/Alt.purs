module Test.Control.Alt where

import Prelude

import Control.Alt (class Alt, (<|>))
import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Test.Spec (Spec, describe, it)

altSpec
  :: forall t1 t2 o1 o2 f
   . Observable t1 o1 (f A)
  => Observable t2 o2 (f B)
  => Alt f
  => (forall a. Gen a -> Gen (f a))
  -> Spec Unit
altSpec gen = describe "Alt instance" do
  it "obeys law: associativity" do
    quickCheckGen' 1000 do
      a :: f A <- gen arbitrary
      b :: f A <- gen arbitrary
      c :: f A <- gen arbitrary
      pure $ ((a <|> b) <|> c) =-= (a <|> (b <|> c))
  it "obeys law: distributivity" do
    quickCheckGen' 1000 do
      a :: f A <- gen arbitrary
      b :: f A <- gen arbitrary
      pure \(f :: A -> B) ->
        (f <$> (a <|> b)) =-= (f <$> a <|> f <$> b)
