module Test.Effect.Reactive.Dual where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (empty)
import Control.Lazy (class Lazy, defer)
import Control.Monad.Fix (class MonadFix, mfix)
import Control.Plus (class Plus)
import Data.Align (class Align, class Alignable, align, nil)
import Data.Array as Array
import Data.Compactable (class Compactable, compact, separate)
import Data.Filterable
  ( class Filterable
  , filter
  , filterMap
  , partition
  , partitionMap
  )
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Reactive as R
import Effect.Reactive.Internal (Time(..))
import Test.Effect.Reactive.Model as M

data Event (t :: R.Timeline) a = E (M.Event a) (R.Event t a)
data Behaviour (t :: R.Timeline) a = B (M.Behaviour a) (R.Behaviour t a)
data Raff (t :: R.Timeline) a = R (M.Raff a) (R.Raff t a)
data Push (t :: R.Timeline) a = RP (M.Push a) (R.RaffPush t a)

liftEM :: forall t a. M.Event a -> Event t a
liftEM e = E e $ defer \_ -> unsafeThrow "undefined"

liftER :: forall t a. R.Event t a -> Event t a
liftER = E $ defer \_ -> unsafeThrow "undefined"

liftBM :: forall t a. M.Behaviour a -> Behaviour t a
liftBM b = B b $ defer \_ -> unsafeThrow "undefined"

liftBR :: forall t a. R.Behaviour t a -> Behaviour t a
liftBR = B $ defer \_ -> unsafeThrow "undefined"

liftRM :: forall t a. M.Raff a -> Raff t a
liftRM m = R m $ defer \_ -> unsafeThrow "undefined"

liftRR :: forall t a. R.Raff t a -> Raff t a
liftRR = R $ defer \_ -> unsafeThrow "undefined"

derive instance Functor (Raff t)
instance Apply (Raff t) where
  apply (R mf rf) (R ma ra) = R (mf <*> ma) (rf <*> ra)

instance Applicative (Raff t) where
  pure a = R (pure a) (pure a)

instance Bind (Raff t) where
  bind (R m r) k = R
    (m >>= k >>> case _ of R m' _ -> m')
    (r >>= k >>> case _ of R _ r' -> r')

instance Monad (Raff t)

instance MonadFix (Raff t) where
  mfix f = R (mfix fm) (mfix fr)
    where
    fm a = let R m _ = f a in m
    fr a = let R _ r = f a in r

derive instance Functor (Push t)
instance Apply (Push t) where
  apply (RP mf rf) (RP ma ra) = RP (mf <*> ma) (rf <*> ra)

instance Applicative (Push t) where
  pure a = RP (pure a) (pure a)

instance Bind (Push t) where
  bind (RP m r) k = RP
    (m >>= k >>> case _ of RP m' _ -> m')
    (r >>= k >>> case _ of RP _ r' -> r')

instance Monad (Push t)

instance MonadFix (Push t) where
  mfix f = RP (mfix fm) (mfix fr)
    where
    fm a = let RP m _ = f a in m
    fr a = let RP _ r = f a in r

instance Functor (Event t) where
  map f (E m e) = E (map f m) (map f e)

instance Compactable (Event t) where
  compact (E m e) = E (compact m) (compact e)
  separate (E m e) =
    let
      msep = separate m
      esep = separate e
    in
      { left: E msep.left esep.left, right: E msep.right esep.right }

instance Filterable (Event t) where
  filter p (E m e) = E (filter p m) (filter p e)
  filterMap f (E m e) = E (filterMap f m) (filterMap f e)
  partition p (E m e) =
    let
      mpart = partition p m
      epart = partition p e
    in
      { no: E mpart.no epart.no, yes: E mpart.yes epart.yes }
  partitionMap f (E m e) =
    let
      mpart = partitionMap f m
      epart = partitionMap f e
    in
      { left: E mpart.left epart.left, right: E mpart.right epart.right }

instance Alt (Event t) where
  alt (E m1 e1) (E m2 e2) = E (m1 <|> m2) (e1 <|> e2)

instance Plus (Event t) where
  empty = E empty empty

instance Apply (Event t) where
  apply (E m1 e1) (E m2 e2) = E (m1 <*> m2) (e1 <*> e2)

instance Bind (Event t) where
  bind (E m r) k = E
    (m >>= k >>> case _ of E m' _ -> m')
    (r >>= k >>> case _ of E _ r' -> r')

instance Align (Event t) where
  align f (E m1 e1) (E m2 e2) = E (align f m1 m2) (align f e1 e2)

instance Alignable (Event t) where
  nil = E nil nil

instance Semigroup a => Semigroup (Event t a) where
  append (E m1 e1) (E m2 e2) = E (append m1 m2) (append e1 e2)

instance Semigroup a => Monoid (Event t a) where
  mempty = E mempty mempty

instance Lazy (Event t a) where
  defer f =
    E (defer \_ -> case f unit of E m _ -> m)
      (defer \_ -> case f unit of E _ e -> e)

instance Functor (Behaviour t) where
  map f (B m e) = B (map f m) (map f e)

instance Apply (Behaviour t) where
  apply (B m1 e1) (B m2 e2) = B (m1 <*> m2) (e1 <*> e2)

instance Applicative (Behaviour t) where
  pure a = B (pure a) (pure a)

instance Lazy (Behaviour t a) where
  defer f =
    B (defer \_ -> case f unit of B m _ -> m)
      (defer \_ -> case f unit of B _ b -> b)

interpretModel
  :: forall t a b
   . (Event t a -> Raff t (Event t b))
  -> Array (Maybe a)
  -> Array (Maybe b)
interpretModel f = interpretModel2 (\_ ea -> f ea) []

interpretModel2
  :: forall t a b c
   . (Event t a -> Event t b -> Raff t (Event t c))
  -> Array (Maybe a)
  -> Array (Maybe b)
  -> Array (Maybe c)
interpretModel2 f as bs = Array.fromFoldable $
  M.interpretE2
    ( \e1 e2 -> map (\(E m _) -> m)
        $ (\(R m _) -> m)
        $ f
            (E e1 (defer \_ -> unsafeThrow "undefined"))
            (E e2 (defer \_ -> unsafeThrow "undefined"))
    )
    (Array.toUnfoldable as)
    (Array.toUnfoldable bs)

interpretEffect
  :: forall a b
   . (forall t. Event t a -> Raff t (Event t b))
  -> Array (Maybe a)
  -> Aff (Array (Maybe b))
interpretEffect f = interpretEffect2 (\_ ea -> f ea) []

interpretEffect2
  :: forall a b c
   . (forall t. Event t a -> Event t b -> Raff t (Event t c))
  -> Array (Maybe a)
  -> Array (Maybe b)
  -> Aff (Array (Maybe c))
interpretEffect2 f = R.interpret2
  ( \e1 e2 -> map (\(E _ e) -> e)
      $ (\(R _ m) -> m)
      $ f
          (E (defer \_ -> unsafeThrow "undefined") e1)
          (E (defer \_ -> unsafeThrow "undefined") e2)
  )

accumE
  :: forall t a b
   . (b -> a -> b)
  -> b
  -> Event t a
  -> Raff t (Event t b)
accumE f i (E m e) = R (liftEM <$> M.accumE f i m) (liftER <$> R.accumE f i e)

accumME
  :: forall t a b
   . (b -> a -> Push t b)
  -> b
  -> Event t a
  -> Raff t (Event t b)
accumME f i (E m e) = R
  (liftEM <$> M.accumME (\b a -> case f b a of RP p _ -> p) i m)
  (liftER <$> R.accumME (\b a -> case f b a of RP _ r -> r) i e)

accumMaybeE
  :: forall t a b
   . (b -> a -> Maybe b)
  -> b
  -> Event t a
  -> Raff t (Event t b)
accumMaybeE f i (E m e) = R
  (liftEM <$> M.accumMaybeE f i m)
  (liftER <$> R.accumMaybeE f i e)

accumMaybeME
  :: forall t a b
   . (b -> a -> Push t (Maybe b))
  -> b
  -> Event t a
  -> Raff t (Event t b)
accumMaybeME f i (E m e) = R
  (liftEM <$> M.accumMaybeME (\b a -> case f b a of RP p _ -> p) i m)
  (liftER <$> R.accumMaybeME (\b a -> case f b a of RP _ r -> r) i e)

accumB
  :: forall t a b
   . (b -> a -> b)
  -> b
  -> Event t a
  -> Raff t (Behaviour t b)
accumB f i (E m e) = R (liftBM <$> M.accumB f i m) (liftBR <$> R.accumB f i e)

accumMB
  :: forall t a b
   . (b -> a -> Push t b)
  -> b
  -> Event t a
  -> Raff t (Behaviour t b)
accumMB f i (E m e) = R
  (liftBM <$> M.accumMB (\b a -> case f b a of RP p _ -> p) i m)
  (liftBR <$> R.accumMB (\b a -> case f b a of RP _ r -> r) i e)

accumMaybeB
  :: forall t a b
   . (b -> a -> Maybe b)
  -> b
  -> Event t a
  -> Raff t (Behaviour t b)
accumMaybeB f i (E m e) = R
  (liftBM <$> M.accumMaybeB f i m)
  (liftBR <$> R.accumMaybeB f i e)

accumMaybeMB
  :: forall t a b
   . (b -> a -> Push t (Maybe b))
  -> b
  -> Event t a
  -> Raff t (Behaviour t b)
accumMaybeMB f i (E m e) = R
  (liftBM <$> M.accumMaybeMB (\b a -> case f b a of RP p _ -> p) i m)
  (liftBR <$> R.accumMaybeMB (\b a -> case f b a of RP _ r -> r) i e)

switch :: forall t a. Behaviour t (Event t a) -> Event t a
switch (B m b) =
  E (M.switch $ (\(E m' _) -> m') <$> m) (R.switch $ (\(E _ e) -> e) <$> b)

stepper :: forall t a. a -> Event t a -> Raff t (Behaviour t a)
stepper i (E m e) = R (liftBM <$> M.stepper i m) (liftBR <$> R.stepper i e)

gate :: forall t a. Behaviour t Boolean -> Event t a -> Event t a
gate (B mb b) (E me e) = E (M.gate mb me) (R.gate b e)

liftSample2
  :: forall t a b c. (a -> b -> c) -> Behaviour t a -> Event t b -> Event t c
liftSample2 f (B mb b) (E me e) =
  E (M.liftSample2 f mb me) (R.liftSample2 f b e)

timeB :: forall t. Behaviour t Time
timeB = B (Time <<< toNumber <$> M.timeB) R.timeB
