module Test.Data.Observe where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Function (applyFlipped)
import Data.Identity (Identity(..))
import Data.Lazy (Lazy, force)
import Data.List (List)
import Data.List.Lazy as L
import Data.List.Lazy.NonEmpty as NL
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.NonEmpty (NonEmpty)
import Data.Ord (class Ord1)
import Data.Ord.Max (Max)
import Data.Ord.Min (Min)
import Data.Semigroup.First (First(..))
import Data.Semigroup.Last (Last(..))
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.These (These)
import Data.Tuple (Tuple(..))
import Safe.Coerce (coerce)
import Test.QuickCheck (class Arbitrary, Result, (<?>))
import Test.QuickCheck.Laws (A(..), B(..), C(..), D(..), E(..))
import Unsafe.Coerce (unsafeCoerce)

class
  ( Arbitrary test
  , Ord observation
  , Show observation
  ) <=
  Observable test observation a
  | a -> test observation where
  observe :: test -> a -> observation

instance Observable Unit Int Int where
  observe = observeDefault

instance Observable Unit Number Number where
  observe = observeDefault

instance Observable Unit Char Char where
  observe = observeDefault

instance Observable Unit Boolean Boolean where
  observe = observeDefault

instance Observable Unit String String where
  observe = observeDefault

instance Observable Unit NonEmptyString NonEmptyString where
  observe = observeDefault

instance Observable Unit Ordering Ordering where
  observe = observeDefault

instance Observable Unit Void Void where
  observe = observeDefault

instance Observable Unit Unit Unit where
  observe = observeDefault

instance Observable Unit Ordering A where
  observe = coerce (observe :: Unit -> Ordering -> Ordering)

instance Observable Unit Ordering B where
  observe = coerce (observe :: Unit -> Ordering -> Ordering)

instance Observable Unit Ordering C where
  observe = coerce (observe :: Unit -> Ordering -> Ordering)

instance Observable Unit Ordering D where
  observe = coerce (observe :: Unit -> Ordering -> Ordering)

instance Observable Unit Ordering E where
  observe = coerce (observe :: Unit -> Ordering -> Ordering)

instance
  ( Observable xt xo x
  , Observable yt yo y
  ) =>
  Observable (Tuple xt yt) (Tuple xo yo) (Tuple x y) where
  observe (Tuple xt yt) (Tuple x y) = Tuple (observe xt x) (observe yt y)

instance Observable t o a => Observable t (Array o) (Array a) where
  observe = map <<< observe

instance Observable t o a => Observable t (NonEmptyArray o) (NonEmptyArray a) where
  observe = map <<< observe

instance Observable t o a => Observable t (List o) (List a) where
  observe = map <<< observe

instance Observable t o a => Observable t (NonEmptyList o) (NonEmptyList a) where
  observe = map <<< observe

instance Observable t o a => Observable t (L.List o) (L.List a) where
  observe = map <<< observe

instance
  ( Functor f
  , Ord1 f
  , Show (f o)
  , Observable t o a
  ) =>
  Observable t (NonEmpty f o) (NonEmpty f a) where
  observe = map <<< observe

instance Observable t o a => Observable t o (Identity a) where
  observe = coerce (observe :: t -> a -> o)

instance Observable t o a => Observable t o (Lazy a) where
  observe t = observe t <<< force

instance Observable t o a => Observable t (Maybe o) (Maybe a) where
  observe = map <<< observe

instance
  ( Observable xt xo x
  , Observable yt yo y
  ) =>
  Observable (Tuple xt yt) (Either xo yo) (Either x y) where
  observe (Tuple xt yt) = bimap (observe xt) (observe yt)

instance
  ( Observable xt xo x
  , Observable yt yo y
  ) =>
  Observable (Tuple xt yt) (These xo yo) (These x y) where
  observe (Tuple xt yt) = bimap (observe xt) (observe yt)

instance
  Observable t o a =>
  Observable t (NL.NonEmptyList o) (NL.NonEmptyList a) where
  observe = map <<< observe

instance Observable t o a => Observable t o (Additive a) where
  observe = coerce (observe :: t -> a -> o)

instance Observable t o a => Observable t o (Multiplicative a) where
  observe = coerce (observe :: t -> a -> o)

instance Observable t o a => Observable t o (First a) where
  observe = coerce (observe :: t -> a -> o)

instance Observable t o a => Observable t o (Last a) where
  observe = coerce (observe :: t -> a -> o)

instance Observable t o a => Observable t o (Conj a) where
  observe = unsafeCoerce (observe :: t -> a -> o)

instance Observable t o a => Observable t o (Disj a) where
  observe = unsafeCoerce (observe :: t -> a -> o)

instance Observable t o a => Observable t o (Dual a) where
  observe = coerce (observe :: t -> a -> o)

instance Observable t o a => Observable t o (Min a) where
  observe = unsafeCoerce (observe :: t -> a -> o)

instance Observable t o a => Observable t o (Max a) where
  observe = unsafeCoerce (observe :: t -> a -> o)

instance (Arbitrary a, Ord b, Show b) => Observable a b (a -> b) where
  observe = applyFlipped

observeDefault :: forall a. Eq a => Show a => Unit -> a -> a
observeDefault = const identity

-- | Self-documenting comparison operation that uses Observable
observeOp
  :: forall t o a
   . Observable t o a
  => (o -> o -> Boolean)
  -> String
  -> a
  -> a
  -> t
  -> Result
observeOp op failString a b t =
  (oa `op` ob) <?> show oa <> failString <> show ob
  where
  oa = observe t a
  ob = observe t b

-- | Self-documenting equality observeion that uses Observable
observeEquals :: forall t o a. Observable t o a => a -> a -> t -> Result
observeEquals = observeOp (==) " /= "

infix 2 observeEquals as =-=
infix 2 observeEquals as =-?

-- | Self-documenting inequality observeion that uses Observable
observeNotEquals :: forall t o a. Observable t o a => a -> a -> t -> Result
observeNotEquals = observeOp (/=) " == "

infix 2 observeNotEquals as /-=
infix 2 observeNotEquals as /-?

observeLessThan :: forall t o a. Observable t o a => a -> a -> t -> Result
observeLessThan = observeOp (<) " >= "

infix 2 observeLessThan as <-?

observeLessThanEq :: forall t o a. Observable t o a => a -> a -> t -> Result
observeLessThanEq = observeOp (<=) " > "

infix 2 observeLessThanEq as <=-?

observeGreaterThan :: forall t o a. Observable t o a => a -> a -> t -> Result
observeGreaterThan = observeOp (>) " <= "

infix 2 observeGreaterThan as >-?

observeGreaterThanEq :: forall t o a. Observable t o a => a -> a -> t -> Result
observeGreaterThanEq = observeOp (>=) " < "

infix 2 observeGreaterThanEq as >=-?

observeTrue :: forall t o a. Observable t o a => a -> a -> t -> Result
observeTrue = observeOp (\_ _ -> true) " ? "
