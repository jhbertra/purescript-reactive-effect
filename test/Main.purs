module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Effect.Reactive.Model (modelSpec)
import Test.Effect.Reactive.RVar (rvarSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ] do
    rvarSpec
    modelSpec
