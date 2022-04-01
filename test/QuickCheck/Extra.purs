module Test.QuickCheck.Extra
  ( quickCheckGen
  , quickCheckGen'
  , quickCheckGenPure
  ) where

import Prelude

import Data.Foldable (intercalate)
import Data.List (length, mapMaybe)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Test.QuickCheck as QC
import Test.QuickCheck.Gen (Gen)

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
