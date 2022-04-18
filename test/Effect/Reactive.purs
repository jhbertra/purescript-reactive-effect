module Test.Effect.Reactive where

import Prelude

import Data.Int (toNumber)
import Data.Newtype (unwrap)
import Effect.Reactive.Event (Behaviour, Event, interpretB, timeB)
import Effect.Reactive.Internal (Raff)
import Effect.Reactive.Model as Model
import Test.QuickCheck (class Arbitrary, (===))
import Test.QuickCheck.Extra (quickCheckImpure)
import Test.QuickCheck.Laws (A)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = do
  behaviourSpec

behaviourSpec :: Spec Unit
behaviourSpec = do
  modelSpec
    "timeB"
    (const $ pure $ unwrap <$> timeB)
    (\(_ :: _ A) -> pure $ toNumber <$> Model.timeB)
  where
  modelSpec
    :: forall a b
     . Arbitrary a
    => Eq b
    => Show b
    => String
    -> (forall t. Event t a -> Raff t (Behaviour t b))
    -> (Model.Event a -> Model.Raff (Model.Behaviour b))
    -> Spec Unit
  modelSpec name real model = describe name do
    it "Matches the model implementation" do
      quickCheckImpure \as -> do
        bs <- interpretB real as
        let modelBs = Model.interpretB model as
        pure $ bs === modelBs
