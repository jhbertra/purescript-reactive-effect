module Test.Data.Functor where

import Prelude

import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C)
import Test.Spec (Spec, describe, it)

functorSpec
  :: forall t1 t2 o1 o2 f
   . Observable t1 o1 (f A)
  => Observable t2 o2 (f C)
  => Functor f
  => (forall a. Gen a -> Gen (f a))
  -> Spec Unit
functorSpec gen = describe "Functor instance" do
  it "obeys law: identity" do
    quickCheckGen' 1000 do
      fa :: f A <- gen arbitrary
      pure $ (identity <$> fa) =-= fa
  it "obeys law: composition" do
    quickCheckGen' 1000 do
      fa :: f A <- gen arbitrary
      pure \(f :: B -> C) (g :: A -> B) ->
        map (f <<< g) fa =-= map f (map g fa)
