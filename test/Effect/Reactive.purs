module Test.Effect.Reactive (reactiveSpec) where

import Prelude

import Control.Alt (alt)
import Control.Alternative (empty)
import Control.Monad.Fix (mfix, mfix2)
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
import Data.Function (on)
import Data.Int (even, odd)
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Data.String (toUpper)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Reactive (react)
import Test.Data.Observe (class Observable, (=-=))
import Test.Effect.Reactive.Dual
  ( Event(..)
  , Raff(..)
  , accumB
  , accumE
  , accumMaybeE
  , gate
  , interpretEffect
  , interpretEffect2
  , interpretModel
  , interpretModel2
  , liftSample2
  , stepper
  , switch
  )
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Extra (quickCheckImpure)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy(..))

unit' = Proxy :: Proxy Unit
bool = Proxy :: Proxy Boolean
int = Proxy :: Proxy Int
string = Proxy :: Proxy String
eIntString = Proxy :: Proxy (Either Int String)
fIntString = Proxy :: Proxy (Int -> String)
mInt = Proxy :: Proxy (Maybe Int)

reactiveSpec :: Spec Unit
reactiveSpec = describe "Effect.Reactive" do
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
  matchesModel2 "append" string string append
  matchesModel "mempty" string (const $ map toUpper mempty)
  matchesModelM "accumE" int (accumE (+) 0)
  matchesModelM "accumMaybeE" int (accumMaybeE (\b -> maybeBool (_ > b)) 0)
  matchesModelM2 "switch" bool int \eswitch evalues -> do
    let evalues' = negate <$> evalues
    bswitch <- stepper false eswitch
    let b = bswitch <#> if _ then evalues else evalues'
    pure $ switch b
  matchesModelM2 "join" bool int \eswitch evalues -> do
    let evalues' = negate <$> evalues
    bswitch <- stepper false eswitch
    let b = bswitch <#> if _ then evalues else evalues'
    -- TODO document this known limitation. You need to "ground" an input event
    -- if you want to feed it into a join, otherwise its trigger won't fire
    -- before it is subscribed to
    _ <- R
      (pure $ pure unit)
      (react (case evalues of E _ r -> r) \_ -> pure unit)
    pure $ join $ liftSample2 const b eswitch
  matchesModel "append+filter" int \e ->
    let
      e1 = map (_ + 1) $ filter even e
      e2 = map (_ + 1) $ filter odd e
    in
      on append (map Additive) e1 e2
  matchesModelM "recursive1A" int \e1 -> do
    e2 /\ _ <- mfix2 \_ b -> do
      let e2 = liftSample2 (+) b e1
      b' <- stepper 0 e2
      pure $ e2 /\ b'
    pure e2
  matchesModelM "recursive1B" int \e1 -> mfix \e2 -> do
    b <- stepper 0 e2
    pure $ liftSample2 (+) b e1
  matchesModelM "recursive2" int \e1 -> do
    e2 /\ _ <- mfix2 \_ e3 -> do
      b <- stepper 0 e3
      let e2 = liftSample2 (+) b e1
      let e3' = liftSample2 (+) (identity <$> b) e1
      pure $ e2 /\ e3'
    pure e2
  matchesModelM "recursive3" unit' \decE -> do
    counterB /\ decAllowedE <- mfix2 \_ decAllowedE -> do
      counterB <- accumB (#) 4 $ (_ - 1) <$ decAllowedE
      pure $ counterB /\ gate ((_ > 0) <$> counterB) decE
    pure $ liftSample2 const counterB decAllowedE
  matchesModelM "recursive4" unit' \inputE -> do
    _ /\ resultB <- mfix2 \resultE _ -> do
      focusB <- stepper false $ fst <$> resultE
      let resultB = (_ /\ 0) <$> focusB
      pure $ liftSample2 const resultB inputE /\ resultB
    pure $ liftSample2 const resultB inputE

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
