module Test.Effect.Reactive.Model where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Plus)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Monad.Fix (mfix)
import Control.Monad.Gen (chooseInt)
import Control.Plus (empty)
import Data.Align (class Align, class Alignable, align)
import Data.Array as Array
import Data.Compactable (class Compactable, separateDefault)
import Data.Filterable
  ( class Filterable
  , filterDefault
  , filterMapDefault
  , partitionDefault
  , partitionMapDefault
  )
import Data.Identity (Identity(..))
import Data.Lazy as DL
import Data.List as Strict
import Data.List.Lazy (List, repeat, (!!))
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Patch (class Patch, applyPatch)
import Data.These (these)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
import Test.Control.Alt (altSpec)
import Test.Control.Applicative (applicativeSpec)
import Test.Control.Apply (applySpec)
import Test.Control.Bind (bindSpec)
import Test.Control.Monad (monadSpec)
import Test.Data.Align (alignSpec, alignableSpec)
import Test.Data.Compactable (compactableFunctorSpec, compactablePlusSpec)
import Test.Data.Filterable (filterableSpec)
import Test.Data.Functor (functorSpec)
import Test.Data.Monoid (monoidSpec)
import Test.Data.Observe (class Observable, observe, (=-=))
import Test.Data.Semigroup (semigroupSpec)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Extra (quickCheckGen)
import Test.QuickCheck.Laws (A)
import Test.Spec (Spec, describe, it)

type Time = Int
type Raff a = Time -> a
type Push a = Time -> a
type Pull a = Time -> a

newtype Event a = Event (List (Maybe a))
newtype Behaviour a = Behaviour (List a)

derive instance Newtype (Event a) _
derive instance Functor Event
instance Compactable Event where
  compact = over Event $ map join
  separate e = separateDefault e

instance Filterable Event where
  filter p = filterDefault p
  filterMap f = filterMapDefault f
  partition p = partitionDefault p
  partitionMap f = partitionMapDefault f

instance Alt Event where
  alt (Event a) (Event b) = Event $ Lazy.zipWith (<|>) a b

instance Apply Event where
  apply (Event f) (Event a) = Event $ Lazy.zipWith apply f a

instance Bind Event where
  bind (Event as) k =
    Event $ zipWithTime (\a t -> flip indexE t =<< map k a) as

instance
  Observable t o a =>
  Observable (Tuple t (Array Unit)) (Array (Maybe o)) (Event a) where
  observe (Tuple t xs) (Event as) =
    observe t
      $ Array.fromFoldable
      $ Lazy.zipWith (const identity) (Lazy.fromFoldable xs) as

instance Plus Event where
  empty = Event $ Lazy.repeat Nothing

instance Align Event where
  align f (Event a) (Event b) = Event $ Lazy.zipWith (align f) a b

instance Alignable Event where
  nil = empty

instance Semigroup a => Semigroup (Event a) where
  append = align $ these identity identity append

instance Semigroup a => Monoid (Event a) where
  mempty = empty

instance Arbitrary a => Arbitrary (Event a) where
  arbitrary = Event
    <$> (_ <> Lazy.repeat Nothing) <<< Array.toUnfoldable
    <$> arbitrary

derive instance Newtype (Behaviour a) _
derive instance Functor Behaviour
instance Apply Behaviour where
  apply (Behaviour f) (Behaviour a) = Behaviour $ Lazy.zipWith ($) f a

instance Bind Behaviour where
  bind (Behaviour as) k =
    Behaviour $ zipWithTime (\a t -> flip sample t $ k a) as

instance Applicative Behaviour where
  pure = Behaviour <<< Lazy.repeat

instance Monad Behaviour

instance Semigroup a => Semigroup (Behaviour a) where
  append = lift2 append

instance Monoid a => Monoid (Behaviour a) where
  mempty = pure mempty

instance Lazy (Behaviour a) where
  defer f = Behaviour $ defer $ coerce f

instance Arbitrary a => Arbitrary (Behaviour a) where
  arbitrary = do
    a <- arbitrary
    Behaviour
      <$> (_ <> Lazy.repeat a) <<< Array.toUnfoldable
      <$> arbitrary

instance
  Observable t o a =>
  Observable (Tuple t (Array Unit)) (Array o) (Behaviour a) where
  observe (Tuple t us) (Behaviour as) =
    observe t
      $ Array.fromFoldable
      $ Lazy.zipWith (const identity) (Lazy.fromFoldable us) as

instance Lazy (Event a) where
  defer f = Event $ defer $ coerce f

interpretE
  :: forall a b
   . (Event a -> Raff (Event b))
  -> Strict.List (Maybe a)
  -> Strict.List (Maybe b)
interpretE f as = Strict.fromFoldable
  $ Lazy.take (Strict.length as)
  $ unwrap
  $ (_ $ 0)
  $ f
  $ Event
  $ Strict.toUnfoldable as <> repeat Nothing

interpretE2
  :: forall a b c
   . (Event a -> Event b -> Raff (Event c))
  -> Strict.List (Maybe a)
  -> Strict.List (Maybe b)
  -> Strict.List (Maybe c)
interpretE2 f as bs = Strict.fromFoldable
  $ Lazy.take (max (Strict.length as) (Strict.length bs))
  $ unwrap
  $ (_ $ 0)
  $ f
      (Event $ Strict.toUnfoldable as <> repeat Nothing)
      (Event $ Strict.toUnfoldable bs <> repeat Nothing)

interpretB
  :: forall a b
   . (Event a -> Raff (Behaviour b))
  -> Strict.List (Maybe a)
  -> Strict.List b
interpretB f as = Strict.fromFoldable
  $ Lazy.take (Strict.length as)
  $ unwrap
  $ (_ $ 0)
  $ f
  $ Event
  $ Strict.toUnfoldable as <> repeat Nothing

interpretB2
  :: forall a b c
   . (Event a -> Event b -> Raff (Behaviour c))
  -> Strict.List (Maybe a)
  -> Strict.List (Maybe b)
  -> Strict.List c
interpretB2 f as bs = Strict.fromFoldable
  $ Lazy.take (max (Strict.length as) (Strict.length bs))
  $ unwrap
  $ (_ $ 0)
  $ f
      (Event $ Strict.toUnfoldable as <> repeat Nothing)
      (Event $ Strict.toUnfoldable bs <> repeat Nothing)

push :: forall a b. (a -> Push (Maybe b)) -> Event a -> Event b
push f = over Event $ zipWithTime \ma t -> do
  a <- ma
  f a t

foldPushE
  :: forall a b. (a -> b -> Push (Maybe a)) -> a -> Event b -> Raff (Event a)
foldPushE f a eb = mfix \ea -> do
  b <- stepper a ea
  pure $ push identity $ (f <$> b) `applyE` eb

foldMaybeE
  :: forall a b. (a -> b -> Maybe a) -> a -> Event b -> Raff (Event a)
foldMaybeE f = foldPushE \a -> pure <<< f a

foldE :: forall a b. (a -> b -> a) -> a -> Event b -> Raff (Event a)
foldE f = foldMaybeE \a -> Just <<< f a

foldPushB
  :: forall a b
   . (a -> b -> Push (Maybe a))
  -> a
  -> Event b
  -> Raff (Behaviour a)
foldPushB f a eb = mfix \b -> do
  let ea = push identity $ (f <$> b) `applyE` eb
  stepper a ea

foldMaybeB
  :: forall a b. (a -> b -> Maybe a) -> a -> Event b -> Raff (Behaviour a)
foldMaybeB f = foldPushB \a -> pure <<< f a

foldB :: forall a b. (a -> b -> a) -> a -> Event b -> Raff (Behaviour a)
foldB f = foldMaybeB \a -> Just <<< f a

zipWithTime :: forall a b. (a -> Time -> b) -> List a -> List b
zipWithTime f as = Lazy.zipWith f as $ Lazy.iterate (_ + 1) 0

indexE :: forall a. Event a -> Time -> Maybe a
indexE (Event as) time = join $ as !! time

sample :: forall a. Behaviour a -> Pull a
sample (Behaviour as) time = unsafePartial $ fromJust $ as !! time

timeB :: Behaviour Time
timeB = Behaviour $ Lazy.iterate (_ + 1) 0

forgetE :: forall a. Time -> Event a -> DL.Lazy (Lazy.Step (Maybe a))
forgetE time = unwrap <<< Lazy.drop time <<< coerce

patcher
  :: forall patch a. Patch patch a => a -> Event patch -> Raff (Behaviour a)
patcher i e time = Behaviour
  $ Lazy.replicate time i <> Lazy.List (step i (forgetE time e))
  where
  step :: a -> DL.Lazy (Lazy.Step (Maybe patch)) -> DL.Lazy (Lazy.Step a)
  step a ls = DL.defer \_ -> Lazy.Cons a $ Lazy.List do
    s <- ls
    case s of
      Lazy.Nil -> unwrap $ Lazy.repeat a
      Lazy.Cons mpatch (Lazy.List mpatches) ->
        let
          next = fromMaybe a do
            patch <- mpatch
            applyPatch patch a
        in
          step next mpatches

stepper :: forall a. a -> Event a -> Raff (Behaviour a)
stepper i = coerce $
  (patcher (Identity i) :: Event a -> Int -> Behaviour (Identity a))

switcher :: forall a. Behaviour a -> Event (Behaviour a) -> Raff (Behaviour a)
switcher b e = join <$> stepper b e

switch :: forall a. Behaviour (Event a) -> Event a
switch (Behaviour es) = Event $ zipWithTime indexE es

switchE :: forall a. Event a -> Event (Event a) -> Raff (Event a)
switchE e0 ee = switch <$> stepper e0 ee

switchEImmediately :: forall a. Event a -> Event (Event a) -> Raff (Event a)
switchEImmediately e0 ee = do
  e <- switchE e0 ee
  pure $ join ee <|> e

applyE :: forall a b. Behaviour (a -> b) -> Event a -> Event b
applyE = over Event <<< Lazy.zipWith map <<< unwrap

modelSpec :: Spec Unit
modelSpec = do
  behaviourSpec
  eventSpec
  combinatorSpec

behaviourSpec :: Spec Unit
behaviourSpec = describe "Effect.Reactive.Model.Behaviour" do
  let arbBehaviour = arbitrary :: forall a. Arbitrary a => _ (Behaviour a)
  functorSpec arbBehaviour
  applySpec arbBehaviour
  applicativeSpec arbBehaviour
  bindSpec arbBehaviour
  monadSpec arbBehaviour
  semigroupSpec (arbitrary :: _ (Behaviour A))
  monoidSpec (arbitrary :: _ (Behaviour A))

eventSpec :: Spec Unit
eventSpec = describe "Effect.Reactive.Model.Event" do
  let arbEvent = arbitrary :: forall a. Arbitrary a => _ (Event a)
  functorSpec arbEvent
  compactableFunctorSpec arbEvent
  compactablePlusSpec arbEvent
  filterableSpec arbEvent
  altSpec arbEvent
  applySpec arbEvent
  bindSpec arbEvent
  alignSpec arbEvent
  alignableSpec arbEvent
  semigroupSpec (arbitrary :: _ (Event A))
  monoidSpec (arbitrary :: _ (Event A))

combinatorSpec :: Spec Unit
combinatorSpec = describe "Effect.Reactive.Model" do
  describe "patcher" do
    it "can be expressed via foldMaybeB" do
      quickCheckGen do
        a :: Identity A <- arbitrary
        e <- arbitrary
        t <- chooseInt 0 10
        let patcher' = foldMaybeB (flip applyPatch)
        pure $ patcher' a e t =-= (patcher a e t)
  describe "stepper" do
    it "prepends the initialValue to the stream" do
      quickCheckGen do
        a :: Identity A <- arbitrary
        Behaviour as <- arbitrary
        let e = Event $ Just <$> as
        pure $ stepper a e 0 =-= (Behaviour $ Lazy.cons a as)
