module Test.Effect.Reactive.Model (modelSpec) where

import Prelude

import Control.Alt (alt)
import Control.Alternative (empty)
import Control.Apply (lift2, lift3)
import Control.Monad.Gen (chooseInt, oneOf, resize, sized)
import Control.Monad.Rec.Class (Step(..), tailRecM3)
import Data.Align (align, nil)
import Data.List (reverse, (:))
import Data.NonEmpty ((:|))
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Effect.Reactive.Model (RVar(..))
import Test.Control.Alt (altSpec)
import Test.Control.Alternative (alternativeSpec)
import Test.Control.Applicative (applicativeSpec)
import Test.Control.Apply (applySpec)
import Test.Control.Bind (bindSpec)
import Test.Control.Monad (monadSpec)
import Test.Data.Align (alignSpec, alignableSpec)
import Test.Data.Functor (functorSpec)
import Test.QuickCheck (class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (Gen, repeatable)
import Test.QuickCheck.Laws (A, B)
import Test.Spec (Spec, describe, focus)

modelSpec :: Spec Unit
modelSpec = describe "Effect.Reactive.Model" do
  rvarSpec

genRVarOccs :: forall a. Gen a -> Gen (RVar a)
genRVarOccs ma = chooseInt 0 24 >>= tailRecM3
  ( \lower tail count -> case lower, count of
      _, 0 -> pure $ Done $ RVar $ reverse tail
      128, _ -> pure $ Done $ RVar $ reverse tail
      _, _ -> ado
        t <- chooseInt lower 127
        a <- ma
        in Loop { a: t + 1, b: Tuple t a : tail, c: count - 1 }
  )
  0
  empty

genRVar :: forall a. Gen a -> Gen (RVar a)
genRVar a = sized \size -> genRVarSized size a

genRVarSized :: forall a. Int -> Gen a -> Gen (RVar a)
genRVarSized size a
  | size == 0 = oneOf $ pure nil :|
      [ pure nil
      , pure <$> a
      , genRVarOccs a
      ]
  | otherwise =
      let
        subRVar :: forall b. Gen b -> Gen (RVar b)
        subRVar = resize (_ / 2) <<< genRVar

        fa :: forall b. Coarbitrary b => Gen (b -> a)
        fa = repeatable \b -> coarbitrary b a
      in
        oneOf
          $ lift2 apply (subRVar fa) (subRVar (arbitrary :: Gen A))
              :|
                [ map <$> fa <*> subRVar (arbitrary :: Gen A)
                , lift3 align
                    ( repeatable case _ of
                        This (_A :: A) -> coarbitrary _A a
                        That (_B :: B) -> coarbitrary _B a
                        Both _A _B -> coarbitrary _A $ coarbitrary _B a
                    )
                    (subRVar arbitrary)
                    (subRVar arbitrary)
                , genRVarOccs a
                ]

rvarSpec :: Spec Unit
rvarSpec = describe "RVar" do
  functorSpec genRVar
  applySpec genRVar
  applicativeSpec genRVar
  alignSpec genRVar
  alignableSpec genRVar
