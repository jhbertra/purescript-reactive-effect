module Test.QuickCheck.Extra
  ( class TestableImpure
  , testImpure
  , quickCheckGen
  , quickCheckGen'
  , quickCheckGenPure
  , quickCheckImpureWithSeed
  , quickCheckImpure'
  , quickCheckImpure
  ) where

import Prelude

import Control.Parallel (parSequence)
import Data.Filterable (filterMap)
import Data.Foldable (intercalate)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List, length, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First)
import Data.String (joinWith)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Random.LCG (Seed)
import Test.QuickCheck (class Arbitrary, Result(..), arbitrary, test)
import Test.QuickCheck as QC
import Test.QuickCheck.Gen (Gen, evalGen, stateful)

class TestableImpure prop where
  testImpure :: prop -> Gen (Aff Result)

instance TestableImpure Result where
  testImpure = map pure <<< test

instance TestableImpure Boolean where
  testImpure = map pure <<< test

instance TestableImpure (Effect Result) where
  testImpure = testImpure <<< (liftEffect :: _ ~> Aff)

instance TestableImpure (Effect Boolean) where
  testImpure = testImpure <<< (liftEffect :: _ ~> Aff)

instance TestableImpure (Aff Result) where
  testImpure = pure

instance TestableImpure (Aff Boolean) where
  testImpure = pure <<< map case _ of
    true -> Success
    false -> Failed "Test returned false"

instance (Arbitrary t, TestableImpure prop) => TestableImpure (t -> prop) where
  testImpure f = testImpure <<< f =<< arbitrary

instance TestableImpure prop => TestableImpure (Gen prop) where
  testImpure = flip bind testImpure

-- | Runs a Testable with a random seed and 100 inputs for a custom generator.
quickCheckGen
  :: forall prop
   . (QC.Testable prop)
  => Gen prop
  -> Aff Unit
quickCheckGen = quickCheckGen' 100

-- | Runs a Testable with a random seed and the given number of inputs for a
-- | custom generator.
quickCheckGen'
  :: forall prop
   . (QC.Testable prop)
  => Int
  -> Gen prop
  -> Aff Unit
quickCheckGen' n prop = do
  seed <- liftEffect QC.randomSeed
  quickCheckGenPure seed n prop

getErrorMessage :: QC.Result -> Maybe String
getErrorMessage (QC.Failed msg) = Just msg
getErrorMessage _ = Nothing

-- | Runs a Testable with a given seed and number of inputs for a custom
-- | generator.
quickCheckGenPure
  :: forall prop
   . (QC.Testable prop)
  => QC.Seed
  -> Int
  -> Gen prop
  -> Aff Unit
quickCheckGenPure seed n prop = do
  let results = QC.quickCheckGenPure seed n prop
  let msgs = mapMaybe getErrorMessage results

  if length msgs > 0 then throwError $ error $ intercalate "\n  " msgs
  else pure unit

type LoopState =
  { successes :: Int
  , firstFailure :: First { index :: Int, message :: String, seed :: Seed }
  , seed :: Seed
  , index :: Int
  }

-- | Runs n impure Testable with a random seed and 100 inputs.
quickCheckImpure :: forall prop. TestableImpure prop => prop -> Aff Unit
quickCheckImpure = quickCheckImpure' 100

-- | Runs an impure Testable with a random seed and the given number of inputs.
quickCheckImpure' :: forall prop. TestableImpure prop => Int -> prop -> Aff Unit
quickCheckImpure' n prop = do
  seed <- liftEffect QC.randomSeed
  quickCheckImpureWithSeed seed n prop

-- | A variant of the `quickCheckImpure'` function that accepts a specific seed as
-- | well as the number of impure tests that should be run.
quickCheckImpureWithSeed
  :: forall prop. TestableImpure prop => Seed -> Int -> prop -> Aff Unit
quickCheckImpureWithSeed seed n prop = do
  let
    tests :: List (Aff (Tuple Seed Result))
    tests =
      sequence <$> evalGen (replicateA n genTest) { newSeed: seed, size: 10 }
  results <- parSequence tests
  traverseWithIndex_ throwFailure $ filterMap (traverse getErrorMessage) results
  where
  genTest = stateful \gs -> Tuple gs.newSeed <$> testImpure prop
  throwFailure i (Tuple s msg) =
    throwError $ error $ joinWith "\n"
      [ "Property falsifiable after " <> show i <> " tests."
      , "Seed: " <> show s
      , "Failure: " <> msg
      ]
