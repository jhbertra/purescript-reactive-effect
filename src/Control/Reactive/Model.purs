module Control.Reactive.Model where

import Prelude hiding ((<@>))

import Control.Bind (bindFlipped)
import Control.Lazy (class Lazy, defer)
import Control.Monad.Fix (class MonadFix, LazyTuple(..), mfix, overM)
import Control.Monad.Reader (class MonadAsk, Reader, ask, runReader)
import Control.Reactive.Behaviour (class Behaviour, (<@>))
import Control.Reactive.Event (class Event)
import Control.Reactive.Network (class Network, hold, sampleLazy)
import Data.Align (class Align, class Alignable)
import Data.Align as A
import Data.Array as Array
import Data.Compactable (class Compactable)
import Data.Either (either, hush)
import Data.Filterable
  ( class Filterable
  , filter
  , filterMapDefault
  , partitionMapDefault
  )
import Data.Lazy as DL
import Data.List.Lazy
  ( List(..)
  , drop
  , iterate
  , repeat
  , replicate
  , scanlLazy
  , tail
  , take
  , uncons
  , zipWith
  , (!!)
  , (:)
  )
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype, over, un, unwrap)
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

newtype NetworkM a = NetworkM (Reader Int a)
newtype EventF a = EventF (List (Maybe a))
newtype BehaviourF a = BehaviourF (List a)

derive instance Newtype (NetworkM a) _
derive instance Functor NetworkM
derive newtype instance Apply NetworkM
derive newtype instance Applicative NetworkM
derive newtype instance Bind NetworkM
derive newtype instance Monad NetworkM
derive newtype instance MonadAsk Int NetworkM
derive newtype instance MonadFix NetworkM

derive instance Newtype (EventF a) _
derive instance Functor EventF
derive newtype instance Lazy (EventF a)

instance Compactable EventF where
  compact = over EventF $ map join
  separate (EventF l) =
    { left: EventF $ bindFlipped (either Just $ const Nothing) <$> l
    , right: EventF $ bindFlipped hush <$> l
    }

instance Filterable EventF where
  filter p = over EventF $ map $ filter p
  filterMap f = filterMapDefault f
  partition p l =
    { yes: filter p l
    , no: filter (not <<< p) l
    }
  partitionMap f = partitionMapDefault f

instance Event EventF

instance Align EventF where
  align f (EventF l1) (EventF l2) = EventF $ zipWith zipper l1 l2
    where
    zipper Nothing Nothing = Nothing
    zipper (Just a) Nothing = Just $ f $ This a
    zipper Nothing (Just a) = Just $ f $ That a
    zipper (Just a) (Just b) = Just $ f $ Both a b

instance Alignable EventF where
  nil = EventF $ repeat Nothing

derive instance Newtype (BehaviourF a) _
derive instance Functor BehaviourF
derive newtype instance Apply BehaviourF
derive newtype instance Applicative BehaviourF
derive newtype instance Lazy (BehaviourF a)

instance Behaviour EventF BehaviourF where
  applyE (BehaviourF fs) (EventF as) = EventF $ zipWith map fs as

instance Network EventF BehaviourF NetworkM where
  accumE a e1 = do
    LazyTuple (Tuple e2 _) <- mfix $ overM LazyTuple \(Tuple e2 b) -> do
      let e2' = ((#) <$> b) <@> e1
      b' <- hold a e2
      pure $ Tuple e2' b'
    pure e2

  hold a e = do
    time <- ask
    pure
      $ BehaviourF
      $ replicate time a <> (scanlLazy fromMaybe) a (forgetE time e)
  sample b = DL.force <$> sampleLazy b
  sampleLazy (BehaviourF l) = do
    time <- ask
    pure $ DL.defer \_ -> unsafePartial $ fromJust $ l !! time
  observe = over EventF $ zipWith
    (\time -> map $ flip runReader time <<< unwrap)
    (iterate (add 1) 0)
  switchE es = do
    t <- ask
    pure $ EventF $ replicate t Nothing
      <> switch (un EventF $ A.nil) (forgetE t (forgetDiagonalE es))
    where
    switch xs ys = defer \_ -> switch' (unsafeUncons xs) (unsafeUncons ys)
    switch' { head: x, tail: xs } { head: Nothing, tail: ys } =
      defer \_ -> x : switch' (unsafeUncons xs) (unsafeUncons ys)
    switch' { head: x } { head: Just xs, tail: ys } =
      defer \_ -> x : switch'
        (unsafeUncons $ unsafePartial $ fromJust $ tail xs)
        (unsafeUncons ys)

    unsafeUncons :: forall a. List a -> { head :: a, tail :: List a }
    unsafeUncons l = unsafePartial $ fromJust $ uncons l
  switchB b = map diagonalB <$> hold b
  interpret f as =
    map (Array.fromFoldable <<< take (Array.length as) <<< unwrap)
      $ f
      $ EventF
      $ Array.toUnfoldable as <> repeat Nothing

forgetE :: forall a. Int -> EventF a -> List (Maybe a)
forgetE time (EventF as) = drop time as

forgetDiagonalE :: forall a. EventF (EventF a) -> EventF (List (Maybe a))
forgetDiagonalE = over EventF $ zipWith
  (map <<< forgetE)
  (iterate (add 1) 0)

diagonalB :: forall a. BehaviourF (BehaviourF a) -> BehaviourF a
diagonalB = over BehaviourF $ zipWith
  (\time (BehaviourF l) -> unsafePartial $ fromJust $ l !! time)
  (iterate (add 1) 0)
