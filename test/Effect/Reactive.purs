module Test.Effect.Reactive where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Align (aligned, nil)
import Data.Compactable (compact, separate)
import Data.Either (Either)
import Data.Filterable (filter, filterMap, partition, partitionMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect.Reactive.Event
  ( Behaviour
  , Event
  , applyE
  , interpretB
  , interpretB2
  , interpretE
  , interpretE2
  , stepper
  , timeB
  )
import Effect.Reactive.Internal (Raff)
import Effect.Reactive.Model as Model
import Test.QuickCheck (class Arbitrary, (===))
import Test.QuickCheck.Extra (quickCheckImpure)
import Test.QuickCheck.Laws (A)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = do
  eventSpec
  behaviourSpec

eventSpec :: Spec Unit
eventSpec = describe "Event" do
  modelSpec2
    "applyB"
    (\(f :: A -> Int) _ ef ea -> applyE <$> stepper f ef <@> ea)
    (\f _ ef ea -> Model.applyE <$> Model.stepper f ef <@> ea)
  modelSpec2
    "map"
    (\(f :: A -> Int) _ _ ea -> pure $ f <$> ea)
    (\f _ _ ea -> pure $ f <$> ea)
  modelSpec
    "compact"
    (\(_ :: _ Int) ea -> pure $ compact ea)
    (\_ ea -> pure $ compact ea)
  modelSpec
    "separate (left)"
    (\(_ :: _ String Int) ea -> pure $ _.left $ separate ea)
    (\_ ea -> pure $ _.left $ separate ea)
  modelSpec
    "separate (right)"
    (\(_ :: _ String Int) ea -> pure $ _.right $ separate ea)
    (\_ ea -> pure $ _.right $ separate ea)
  modelSpec2
    "filter"
    (\(f :: Int -> Boolean) _ _ ea -> pure $ filter f ea)
    (\f _ _ ea -> pure $ filter f ea)
  modelSpec2
    "partition (no)"
    (\(f :: Int -> Boolean) _ _ ea -> pure $ _.no $ partition f ea)
    (\f _ _ ea -> pure $ _.no $ partition f ea)
  modelSpec2
    "partition (yes)"
    (\(f :: Int -> Boolean) _ _ ea -> pure $ _.yes $ partition f ea)
    (\f _ _ ea -> pure $ _.yes $ partition f ea)
  modelSpec2
    "filterMap"
    (\(f :: A -> Maybe Int) _ _ ea -> pure $ filterMap f ea)
    (\f _ _ ea -> pure $ filterMap f ea)
  modelSpec2
    "partitionMap (left)"
    (\(f :: A -> Either String Int) _ _ ea -> pure $ _.left $ partitionMap f ea)
    (\f _ _ ea -> pure $ _.left $ partitionMap f ea)
  modelSpec2
    "partitionMap (right)"
    (\(f :: A -> Either String Int) _ _ e -> pure $ _.right $ partitionMap f e)
    (\f _ _ ea -> pure $ _.right $ partitionMap f ea)
  modelSpec2
    "alt"
    (\(_ :: Int) _ a b -> pure $ a <|> b)
    (\_ _ a b -> pure $ a <|> b)
  modelSpec2
    "apply"
    (\(_ :: A -> Int) _ a b -> pure $ a <*> b)
    (\_ _ a b -> pure $ a <*> b)
  modelSpec2
    "aligned"
    (\(_ :: Int) (_ :: String) a b -> pure $ aligned a b)
    (\_ _ a b -> pure $ aligned a b)
  modelSpec0 "nil" nil (nil :: _ Int)
  modelSpec0 "empty" empty (empty :: _ Int)
  modelSpec0 "mempty" mempty (mempty :: _ String)
  modelSpec2
    "append"
    (\(_ :: String) _ a b -> pure $ a <> b)
    (\_ _ a b -> pure $ a <> b)
  where
  modelSpec0
    :: forall a
     . Eq a
    => Show a
    => String
    -> (forall t. (Event t a))
    -> (Model.Event a)
    -> Spec Unit
  modelSpec0 name real model = describe name do
    it "Matches the model implementation" do
      quickCheckImpure \(as :: _ (_ A)) -> do
        bs <- interpretE (const $ pure real) as
        let modelBs = Model.interpretE (const $ pure model) as
        pure $ bs === modelBs

  modelSpec
    :: forall a b
     . Arbitrary a
    => Eq b
    => Show b
    => String
    -> (forall t. a -> Event t a -> Raff t (Event t b))
    -> (a -> Model.Event a -> Model.Raff (Model.Event b))
    -> Spec Unit
  modelSpec name real model = describe name do
    it "Matches the model implementation" do
      quickCheckImpure \a as -> do
        bs <- interpretE (real a) as
        let modelBs = Model.interpretE (model a) as
        pure $ bs === modelBs

  modelSpec2
    :: forall a b c
     . Arbitrary a
    => Arbitrary b
    => Eq c
    => Show c
    => String
    -> (forall t. a -> b -> Event t a -> Event t b -> Raff t (Event t c))
    -> (a -> b -> Model.Event a -> Model.Event b -> Model.Raff (Model.Event c))
    -> Spec Unit
  modelSpec2 name real model = describe name do
    it "Matches the model implementation" do
      quickCheckImpure \a b as bs -> do
        cs <- interpretE2 (real a b) as bs
        let modelCs = Model.interpretE2 (model a b) as bs
        pure $ cs === modelCs

behaviourSpec :: Spec Unit
behaviourSpec = describe "Behaviour" do
  modelSpec0 "timeB" (unwrap <$> timeB) (toNumber <$> Model.timeB)
  modelSpec "stepper" stepper (Model.stepper :: Int -> _ -> _)
  modelSpec "pure" (\(a :: Int) _ -> pure $ pure a) (\a _ -> pure $ pure a)
  modelSpec2
    "(<*>)"
    ( \(f :: A -> Int) a ef ea -> do
        bf <- stepper f ef
        ba <- stepper a ea
        pure $ bf <*> ba
    )
    ( \f a ef ea -> do
        bf <- Model.stepper f ef
        ba <- Model.stepper a ea
        pure $ bf <*> ba
    )
  where
  modelSpec0
    :: forall a
     . Eq a
    => Show a
    => String
    -> (forall t. (Behaviour t a))
    -> (Model.Behaviour a)
    -> Spec Unit
  modelSpec0 name real model = describe name do
    it "Matches the model implementation" do
      quickCheckImpure \(as :: _ (_ A)) -> do
        bs <- interpretB (const $ pure real) as
        let modelBs = Model.interpretB (const $ pure model) as
        pure $ bs === modelBs

  modelSpec
    :: forall a b
     . Arbitrary a
    => Eq b
    => Show b
    => String
    -> (forall t. a -> Event t a -> Raff t (Behaviour t b))
    -> (a -> Model.Event a -> Model.Raff (Model.Behaviour b))
    -> Spec Unit
  modelSpec name real model = describe name do
    it "Matches the model implementation" do
      quickCheckImpure \a as -> do
        bs <- interpretB (real a) as
        let modelBs = Model.interpretB (model a) as
        pure $ bs === modelBs

  modelSpec2
    :: forall a b c
     . Arbitrary a
    => Arbitrary b
    => Eq c
    => Show c
    => String
    -> (forall t. a -> b -> Event t a -> Event t b -> Raff t (Behaviour t c))
    -> ( a
         -> b
         -> Model.Event a
         -> Model.Event b
         -> Model.Raff (Model.Behaviour c)
       )
    -> Spec Unit
  modelSpec2 name real model = describe name do
    it "Matches the model implementation" do
      quickCheckImpure \a b as bs -> do
        cs <- interpretB2 (real a b) as bs
        let modelCs = Model.interpretB2 (model a b) as bs
        pure $ cs === modelCs
