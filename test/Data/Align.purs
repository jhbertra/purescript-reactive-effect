module Test.Data.Align where

import Prelude

import Data.Align (class Align, class Alignable, align, nil)
import Data.Bifunctor (bimap)
import Data.These (These(..), assoc, swap)
import Test.Data.Observe (class Observable, (=-=))
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Extra (quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C, D)
import Test.Spec (Spec, describe, it)

alignSpec
  :: forall t1 t2 t3 t4 o1 o2 o3 o4 f
   . Observable t1 o1 (f (These A A))
  => Observable t2 o2 (f (These A B))
  => Observable t3 o3 (f (These C D))
  => Observable t4 o4 (f (These A (These B C)))
  => Align f
  => (forall a. Gen a -> Gen (f a))
  -> Spec Unit
alignSpec gen = describe "Align instance" do
  it "obeys law: idempotency" do
    quickCheckGen' 1000 do
      fa :: f A <- gen arbitrary
      pure $ join (align identity) fa =-= map (join Both) fa
  it "obeys law: commutativity" do
    quickCheckGen' 1000 do
      fa :: f A <- gen arbitrary
      fb :: f B <- gen arbitrary
      pure $ align identity fa fb =-= swap <$> align identity fb fa
  it "obeys law: associativity" do
    quickCheckGen' 1000 do
      fa :: f A <- gen arbitrary
      fb :: f B <- gen arbitrary
      fc :: f C <- gen arbitrary
      pure $
        align identity fa (align identity fb fc) =-=
          assoc <$> align identity (align identity fa fb) fc
  it "obeys law: functoriality" do
    quickCheckGen' 1000 do
      fa :: f A <- gen arbitrary
      fb :: f B <- gen arbitrary
      f :: (A -> C) <- arbitrary
      g :: (B -> D) <- arbitrary
      pure $
        align identity (f <$> fa) (g <$> fb) =-=
          bimap f g <$> align identity fa fb

alignableSpec
  :: forall t o f
   . Observable t o (f (These A B))
  => Alignable f
  => (forall a. Gen a -> Gen (f a))
  -> Spec Unit
alignableSpec gen = describe "Alignable instance" do
  it "obeys law: left identity" do
    quickCheckGen' 1000 do
      fb :: f B <- gen arbitrary
      pure $ align identity (nil :: f A) fb =-= map That fb
  it "obeys law: right identity" do
    quickCheckGen' 1000 do
      fa :: f A <- gen arbitrary
      pure $ align identity fa (nil :: f B) =-= map This fa
