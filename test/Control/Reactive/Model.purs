module Test.Control.Reactive.Model where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Alternative (empty)
import Control.Apply (lift2, lift3)
import Control.Monad.Gen (chooseInt, oneOf, resize, sized)
import Control.Monad.Gen.Common (genEither, genMaybe)
import Control.Reactive.Event
  ( AStep(..)
  , AnEvent(..)
  , EventModel
  , StepModel
  , denotationE
  , interpret
  , switcherE
  , unionWith
  )
import Control.Reactive.Model (Future(..), Time(..), future)
import Data.Align (align, nil)
import Data.Compactable (compact, separate)
import Data.Lazy (Lazy)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.String (joinWith)
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Test.Control.Alt (altSpec)
import Test.Control.Alternative (alternativeSpec)
import Test.Control.Applicative (applicativeSpec)
import Test.Control.Apply (applySpec)
import Test.Control.Bind (bindSpec)
import Test.Control.Monad (monadSpec)
import Test.Data.Align (alignSpec, alignableSpec)
import Test.Data.Bounded (boundedSpec)
import Test.Data.Compactable (compactableFunctorSpec, compactablePlusSpec)
import Test.Data.Enum (enumSpec)
import Test.Data.Eq (eqSpec)
import Test.Data.Filterable (filterableSpec)
import Test.Data.Functor (functorSpec)
import Test.Data.Monoid (monoidSpec)
import Test.Data.Observe (observe, (=-=))
import Test.Data.Ord (ordSpec)
import Test.Data.Semigroup (semigroupSpec)
import Test.QuickCheck (Result(..), arbitrary, coarbitrary)
import Test.QuickCheck.Arbitrary (class Coarbitrary)
import Test.QuickCheck.Extra (quickCheckGen)
import Test.QuickCheck.Gen (Gen, repeatable)
import Test.QuickCheck.Laws (A, B)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck, quickCheck')

modelSpec :: Spec Unit
modelSpec = describe "Control.Reactive.Model" do
  timeSpec
  futureSpec
  eventSpec

genTime :: Gen Time
genTime = genTimeFrom 0

genTimeFrom :: Int -> Gen Time
genTimeFrom lower = Time <<< pure <$> chooseInt (min lower 127) 127

timeSpec :: Spec Unit
timeSpec = describe "Time" do
  eqSpec genTime
  ordSpec genTime
  boundedSpec genTime
  enumSpec genTime
  semigroupSpec genTime
  monoidSpec genTime

genFutureFrom :: forall a. Int -> Gen a -> Gen (Future a)
genFutureFrom lower a = sized \size -> genFutureSized lower size a

genFuture :: forall a. Gen a -> Gen (Future a)
genFuture a = genFutureFrom 0 a

genFutureSized :: forall a. Int -> Int -> Gen a -> Gen (Future a)
genFutureSized lower size a
  | size == 0 = oneOf $ pure empty :|
      [ pure nil
      , pure <$> a
      , future <$> genTimeFrom lower <*> a
      ]
  | otherwise =
      let
        subFuture :: forall b. Gen b -> Gen (Future b)
        subFuture = resize (_ / 2) <<< genFutureFrom lower

        fa :: forall b. Coarbitrary b => Gen (b -> a)
        fa = repeatable \b -> coarbitrary b a
      in
        oneOf $
          lift2 apply (subFuture fa) (subFuture (arbitrary :: Gen A)) :|
            [ map <$> fa <*> subFuture (arbitrary :: Gen A)
            , lift2 alt (subFuture a) (subFuture a)
            , lift3 align
                ( repeatable case _ of
                    This (_A :: A) -> coarbitrary _A a
                    That (_B :: B) -> coarbitrary _B a
                    Both _A _B -> coarbitrary _A $ coarbitrary _B a
                )
                (subFuture arbitrary)
                (subFuture arbitrary)
            , lift2 bind
                (subFuture arbitrary)
                (repeatable \(_A :: A) -> coarbitrary _A $ subFuture a)
            ]

futureSpec :: Spec Unit
futureSpec = describe "Future" do
  eqSpec $ genFuture (arbitrary :: Gen A)
  functorSpec genFuture
  applySpec genFuture
  applicativeSpec genFuture
  altSpec genFuture
  alternativeSpec genFuture
  alignSpec genFuture
  alignableSpec genFuture
  bindSpec genFuture
  monadSpec genFuture
  describe "<|>" do
    it "Resolves earlier times" do
      quickCheckGen do
        t1 <- genTime
        t2 <- genTime
        f1 <- Future (pure (min t1 t2)) <$> (arbitrary :: Gen (Lazy A))
        f2 <- Future (pure (max t1 t2)) <$> (arbitrary :: Gen (Lazy A))
        pure $ f1 <|> f2 =-= f1
    it "Chooses left in case of ties" do
      quickCheckGen do
        t <- genTime
        f1 <- Future (pure t) <$> (arbitrary :: Gen (Lazy A))
        f2 <- Future (pure t) <$> (arbitrary :: Gen (Lazy A))
        pure $ f1 <|> f2 =-= f1
    it
      "Chooses the option with the earliest occurance in its history in case of ties"
      do
        quickCheckGen do
          t1 <- genTime
          t2 <- genTime
          f1 :: Future A <- future (max t1 t2) <$> arbitrary
          f2 :: Future A <- future (max t1 t2) <$> arbitrary
          f3 :: Future B <- future (min t1 t2) <$> arbitrary
          pure $ f1 <|> f2 <* f3 =-= f2

genEventFrom :: forall a. Int -> Gen a -> Gen (EventModel a)
genEventFrom lower a = sized \size -> genEventSized lower size a

genEvent :: forall a. Gen a -> Gen (EventModel a)
genEvent a = genEventFrom 0 a

genEventSized :: forall a. Int -> Int -> Gen a -> Gen (EventModel a)
genEventSized lower size a
  | size == 0 = oneOf $ (pure empty) :|
      [ pure nil
      , do
          time <- chooseInt lower 127
          Event <$> genFutureFrom time (genStep time a)
      ]
  | otherwise =
      let
        subEvent :: forall b. Gen b -> Gen (EventModel b)
        subEvent = resize (_ / 2) <<< genEvent

        fa :: forall b. Coarbitrary b => Gen (b -> a)
        fa = repeatable \b -> coarbitrary b a
      in
        oneOf $
          (map <$> fa <*> subEvent (arbitrary :: Gen A)) :|
            [ lift2 alt (subEvent a) (subEvent a)
            , lift3 align
                ( repeatable case _ of
                    This (_A :: A) -> coarbitrary _A a
                    That (_B :: B) -> coarbitrary _B a
                    Both _A _B -> coarbitrary _A $ coarbitrary _B a
                )
                (subEvent arbitrary)
                (subEvent arbitrary)
            , compact <$> subEvent (genMaybe a)
            , _.left <<< separate <$> subEvent (genEither a a)
            , _.right <<< separate <$> subEvent (genEither a a)
            , switcherE <$> subEvent (resize (_ / 2) $ subEvent a)
            ]

genStep :: forall a. Int -> Gen a -> Gen (StepModel a)
genStep lower a = Step <$> a <*> genEventFrom lower a

eventSpec :: Spec Unit
eventSpec = describe "Event" do
  functorSpec genEvent
  altSpec genEvent
  alignSpec genEvent
  alignableSpec genEvent
  compactableFunctorSpec genEvent
  compactablePlusSpec genEvent
  filterableSpec genEvent
  describe "interpret" do
    it "obeys law: identity" do
      quickCheck \(as :: List (Maybe A)) ->
        interpret identity as =-= as
    it "obeys law: annihilation" do
      quickCheck \(as :: List (Maybe A)) ->
        interpret (const (empty :: EventModel B)) as =-= Nothing <$ as
  describe "empty" do
    it "satisfies the denotational semantics" do
      quickCheck' 1 $ denotationE (empty :: EventModel A) =-= empty
  describe "unionWith" do
    it "satisfies the denotational semantics" do
      quickCheckGen do
        e1 :: EventModel A <- genEvent arbitrary
        e2 <- genEvent arbitrary
        f <- arbitrary
        let
          unionWith' _ Nil Nil = Nil
          unionWith' _ xs Nil = xs
          unionWith' _ Nil ys = ys
          unionWith' f' xs'@(Tuple timex x : xs) ys'@(Tuple timey y : ys)
            | timex < timey = Tuple timex x : unionWith' f' xs ys'
            | timex > timey = Tuple timey y : unionWith' f' xs' ys
            | otherwise = Tuple timex (f' x y) : unionWith' f' xs ys
        pure do
          result <- denotationE (unionWith f e1 e2) =-= LL.fromFoldable
            ( unionWith' f
                (L.fromFoldable $ denotationE e1)
                (L.fromFoldable $ denotationE e2)
            )
          \t -> case result of
            Success -> Success
            Failed s -> Failed $ joinWith "\n"
              [ "Counterexample:"
              , "  e1: " <> show (observe t $ denotationE e1)
              , "  e2: " <> show (observe t $ denotationE e2)
              , s
              ]
