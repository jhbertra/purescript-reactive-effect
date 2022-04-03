module Test.Control.Reactive.Model where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Gen (chooseInt)
import Control.Reactive.Model (Event, Future(..), Time(..), interpret)
import Data.Lazy (Lazy)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Test.Control.Alt (altSpec)
import Test.Control.Alternative (alternativeSpec)
import Test.Control.Applicative (applicativeSpec)
import Test.Control.Apply (applySpec)
import Test.Control.Bind (bindSpec)
import Test.Control.Monad (monadSpec)
import Test.Data.Align (alignSpec, alignableSpec)
import Test.Data.Bounded (boundedSpec)
import Test.Data.Enum (enumSpec)
import Test.Data.Eq (eqSpec)
import Test.Data.Functor (functorSpec)
import Test.Data.Monoid (monoidSpec)
import Test.Data.Observe ((=-=))
import Test.Data.Ord (ordSpec)
import Test.Data.Semigroup (semigroupSpec)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Extra (quickCheckGen)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

modelSpec :: Spec Unit
modelSpec = describe "Control.Reactive.Model" do
  timeSpec
  futureSpec

genTime :: Gen Time
genTime = Time <<< pure <$> chooseInt 0 127

timeSpec :: Spec Unit
timeSpec = describe "Time" do
  eqSpec genTime
  ordSpec genTime
  boundedSpec genTime
  enumSpec genTime
  semigroupSpec genTime
  monoidSpec genTime

genFuture :: forall a. Gen a -> Gen (Future a)
genFuture genA = Future <$> (pure <$> genTime) <*> (pure <$> genA)

futureSpec :: Spec Unit
futureSpec = describe "Future" do
  eqSpec $ genFuture (arbitrary :: Gen A)
  functorSpec genFuture
  applySpec genFuture
  applicativeSpec genFuture
  altSpec genFuture
  alternativeSpec genFuture
  alignSpec genFuture
  alignableSpec genFuture
  bindSpec genFuture
  monadSpec genFuture
  describe "<|>" do
    it "Resolves earlier times" do
      quickCheckGen do
        t1 <- genTime
        t2 <- genTime
        f1 <- Future (pure (min t1 t2)) <$> (arbitrary :: Gen (Lazy A))
        f2 <- Future (pure (max t1 t2)) <$> (arbitrary :: Gen (Lazy A))
        pure $ f1 <|> f2 =-= f1
    it "Chooses left in case of ties" do
      quickCheckGen do
        t <- genTime
        f1 <- Future (pure t) <$> (arbitrary :: Gen (Lazy A))
        f2 <- Future (pure t) <$> (arbitrary :: Gen (Lazy A))
        pure $ f1 <|> f2 =-= f1
    it
      "Chooses the option with the earliest occurance in its history in case of ties"
      do
        quickCheckGen do
          t1 <- genTime
          t2 <- genTime
          f1 <- Future (pure (max t1 t2)) <$> (arbitrary :: Gen (Lazy A))
          f2 <- Future (pure (max t1 t2)) <$> (arbitrary :: Gen (Lazy A))
          f3 <- Future (pure (min t1 t2)) <$> (arbitrary :: Gen (Lazy B))
          pure $ f1 <|> f2 <* f3 =-= f2

  describe "interpret" do
    it "obeys law: identity" do
      quickCheck \(as :: List (Maybe A)) ->
        interpret identity as =-= as
    it "obeys law: annihilation" do
      quickCheck \(as :: List (Maybe A)) ->
        interpret (const (empty :: Event B)) as =-= Nothing <$ as
