module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Control.Reactive.Model (modelSpec)
import Test.Spec (parallel)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ] $ parallel do
    modelSpec
