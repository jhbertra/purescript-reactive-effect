module Test.Effect.Reactive (reactiveSpec) where

import Prelude

import Control.Alt (alt)
import Control.Alternative (empty)
import Data.Align (aligned, nil)
import Data.Compactable (compact, separate)
import Data.Either (Either)
import Data.Filterable
  ( eitherBool
  , filter
  , filterMap
  , maybeBool
  , partition
  , partitionMap
  )
import Data.Int (odd)
import Data.Maybe (Maybe)
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
import Type.Proxy (Proxy(..))

int = Proxy :: Proxy Int
string = Proxy :: Proxy String
eIntString = Proxy :: Proxy (Either Int String)
fIntString = Proxy :: Proxy (Int -> String)
mInt = Proxy :: Proxy (Maybe Int)

reactiveSpec :: Spec Unit
reactiveSpec = describe "Effect.Reactive" do
  describe "Event" do
    matchesModel "map" int (map $ add 1)
    matchesModel "compact" mInt compact
    matchesModel "separate.left" eIntString (_.left <<< separate)
    matchesModel "separate.right" eIntString (_.right <<< separate)
    matchesModel "filter" int (filter odd)
    matchesModel "filterMap" int (filterMap (maybeBool odd))
    matchesModel "partition.no" int (_.no <<< partition odd)
    matchesModel "partition.yes" int (_.yes <<< partition odd)
    matchesModel "partitionMap.left" int
      (_.left <<< partitionMap (eitherBool odd))
    matchesModel "partitionMap.right" int
      (_.right <<< partitionMap (eitherBool odd))
    matchesModel2 "alt" int int alt
    matchesModel "empty" int (const $ map odd empty)
    matchesModel "nil" int (const $ map odd nil)
    matchesModel2 "apply" fIntString int apply
    matchesModel2 "aligned" int string aligned

matchesModel
  :: forall t o a b
   . Observable t o b
  => Arbitrary a
  => String
  -> Proxy a
  -> (forall t'. Event t' a -> Event t' b)
  -> Spec Unit
matchesModel name t f = matchesModelM name t (pure <<< f)

matchesModelM
  :: forall t o a b
   . Observable t o b
  => Arbitrary a
  => String
  -> Proxy a
  -> (forall t'. Event t' a -> Raff t' (Event t' b))
  -> Spec Unit
matchesModelM name _ f = describe name $ it "matches the model" do
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
  -> Proxy a
  -> Proxy b
  -> (forall t'. Event t' a -> Event t' b -> Event t' c)
  -> Spec Unit
matchesModel2 name t u f = matchesModelM2 name t u (map pure <<< f)

matchesModelM2
  :: forall t o a b c
   . Observable t o c
  => Arbitrary a
  => Arbitrary b
  => String
  -> Proxy a
  -> Proxy b
  -> (forall t'. Event t' a -> Event t' b -> Raff t' (Event t' c))
  -> Spec Unit
matchesModelM2 name _ _ f = describe name $ it "matches the model" do
  quickCheckImpure \as bs t -> do
    actual <- interpretEffect2 f as bs
    let expected = interpretModel2 f as bs
    pure $ (actual =-= expected) t
