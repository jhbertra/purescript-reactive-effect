module Test.Effect.Reactive (reactiveSpec) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Data.Align (aligned, nil)
import Data.Compactable (compact, separate)
import Data.Filterable
  ( eitherBool
  , filter
  , filterMap
  , maybeBool
  , partition
  , partitionMap
  )
import Data.Int (odd)
import Test.Data.Observe (class Observable, (=-=))
import Test.Effect.Reactive.Dual
  ( Event
  , Raff
  , interpretEffect
  , interpretEffect2
  , interpretModel
  , interpretModel2
  )
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Extra (quickCheckImpure)
import Test.Spec (Spec, describe, it)

reactiveSpec :: Spec Unit
reactiveSpec = describe "Effect.Reactive" do
  describe "Event" do
    matchesModel "map" (pure <<< map (add 1))
    matchesModel "compact" \(e :: _ _ (_ Int)) -> pure $ compact e
    matchesModel "separate.left" \(e :: _ _ (_ Int String)) -> pure
      (separate e).left
    matchesModel "separate.right" \(e :: _ _ (_ Int String)) -> pure
      (separate e).right
    matchesModel "filter" (pure <<< filter odd)
    matchesModel "filterMap" (pure <<< filterMap (maybeBool odd))
    matchesModel "partition.no" (pure <<< _.no <<< partition odd)
    matchesModel "partition.yes" (pure <<< _.yes <<< partition odd)
    matchesModel "partitionMap.left"
      (pure <<< _.left <<< partitionMap (eitherBool odd))
    matchesModel "partitionMap.right"
      (pure <<< _.right <<< partitionMap (eitherBool odd))
    matchesModel2 "alt" (\(e1 :: _ _ Int) e2 -> pure $ e1 <|> e2)
    matchesModel "empty" (\(_ :: _ _ Int) -> pure (empty :: _ _ Int))
    matchesModel "nil" (\(_ :: _ _ Int) -> pure (nil :: _ _ Int))
    matchesModel2 "apply" (\(e1 :: _ _ (Int -> String)) e2 -> pure $ e1 <*> e2)
    matchesModel2 "aligned"
      (\(e1 :: _ _ Int) (e2 :: _ _ String) -> pure $ aligned e1 e2)
    matchesModel2 "aligned"
      (\(e1 :: _ _ Int) (e2 :: _ _ String) -> pure $ aligned e1 e2)

matchesModel
  :: forall t o a b
   . Observable t o b
  => Arbitrary a
  => String
  -> (forall t'. Event t' a -> Raff t' (Event t' b))
  -> Spec Unit
matchesModel name f = describe name $ it "matches the model" do
  quickCheckImpure \as t -> do
    actual <- interpretEffect f as
    let expected = interpretModel f as
    pure $ (actual =-= expected) t

matchesModel2
  :: forall t o a b c
   . Observable t o c
  => Arbitrary a
  => Arbitrary b
  => String
  -> (forall t'. Event t' a -> Event t' b -> Raff t' (Event t' c))
  -> Spec Unit
matchesModel2 name f = describe name $ it "matches the model" do
  quickCheckImpure \as bs t -> do
    actual <- interpretEffect2 f as bs
    let expected = interpretModel2 f as bs
    pure $ (actual =-= expected) t
