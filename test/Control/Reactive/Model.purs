module Test.Control.Reactive.Model where

import Prelude

import Control.Monad.Gen (chooseInt)
import Control.Reactive.Model (Time(..))
import Data.Lazy (defer)
import Test.Data.Bounded (boundedSpec)
import Test.Data.Enum (enumSpec)
import Test.Data.Eq (eqSpec)
import Test.Data.Monoid (monoidSpec)
import Test.Data.Ord (ordSpec)
import Test.Data.Semigroup (semigroupSpec)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe)

modelSpec :: Spec Unit
modelSpec = describe "Control.Reactive.Model" do
  timeSpec

genTime :: Gen Time
genTime = Time <<< defer <<< const <$> chooseInt 0 100

timeSpec :: Spec Unit
timeSpec = describe "Time" do
  eqSpec genTime
  ordSpec genTime
  boundedSpec genTime
  enumSpec genTime
  semigroupSpec genTime
  monoidSpec genTime
