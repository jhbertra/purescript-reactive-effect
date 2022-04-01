module Test.Control.Reactive.Model where

import Prelude

import Control.Monad.Gen (chooseInt)
import Control.Reactive.Model (Time(..))
import Data.Lazy (defer)
import Test.Data.Eq (eqSpec)
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
