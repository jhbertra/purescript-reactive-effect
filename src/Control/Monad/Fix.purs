module Control.Monad.Fix where

import Prelude

import Control.Lazy (class Lazy, defer, fix)
import Control.Monad.RWS.Trans (RWSResult(..), RWST(..), runRWST)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Control.Monad.State.Trans (StateT(..), runStateT)
import Control.Monad.Writer.Trans (WriterT(..), runWriterT)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Lazy (force)
import Data.Lazy as DL
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Aff, error, fiberCanceler, launchAff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

-- | Type class for monads that support fixpoints.
-- |
-- | `mfix f` runs `f` once with the eventual result of `f` as input. Make sure
-- | not to apply the supplied function until the computation returned; else
-- | a dynamic error will be thrown.
class (Monad m) <= MonadFix m where
  mfix :: forall a. Lazy a => (a -> m a) -> m a

instance monadFixRWST ::
  ( Lazy s
  , Lazy w
  , Monoid w
  , MonadFix m
  ) =>
  MonadFix (RWST r w s m) where
  mfix f = RWST \r s ->
    map unwrap
      $ mfix
      $ overM LazyRWSResult
      $ case _ of RWSResult _ a _ -> runRWST (f a) r s

instance MonadFix Identity where
  mfix f = Identity $ fix $ coerce f

message :: String
message =
  "Control.Monad.Fix: Premature access to result of fixpoint computation."

instance MonadFix Effect where
  mfix f = do
    resultRef <- Ref.new $ defer \_ -> unsafeThrow message
    result <- f $ defer \_ -> unsafePerformEffect $ Ref.read resultRef
    Ref.write result resultRef
    pure result

instance MonadFix Aff where
  mfix f = makeAff \resolve -> do
    resultRef <- Ref.new $ defer \_ -> unsafePerformEffect do
      resolve $ Left $ error message
      throw message
    fiber <- launchAff do
      result <- f $ defer \_ -> unsafePerformEffect $ Ref.read resultRef
      liftEffect do
        Ref.write result resultRef
        resolve $ Right result
    pure $ fiberCanceler fiber

instance MonadFix (Function r) where
  mfix f r = fix (flip f r)

instance MonadFix DL.Lazy where
  mfix f = DL.defer \_ -> fix (DL.force <<< f)

instance (MonadFix m) => MonadFix (ReaderT r m) where
  mfix f = ReaderT \r -> mfix (flip runReaderT r <<< f)

instance (Lazy s, MonadFix m) => MonadFix (StateT s m) where
  mfix f = StateT \s -> mfix2 $ const <<< flip runStateT s <<< f

instance (Lazy w, MonadFix m, Monoid w) => MonadFix (WriterT w m) where
  mfix f = WriterT $ mfix2 $ const <<< runWriterT <<< f

newtype LazyTuple a b = LazyTuple (Tuple a b)

derive instance Newtype (LazyTuple a b) _
derive instance Functor (LazyTuple a)
instance (Lazy a, Lazy b) => Lazy (LazyTuple a b) where
  defer f = LazyTuple $ Tuple
    (defer $ fst <<< unwrap <<< f)
    (defer $ snd <<< unwrap <<< f)

newtype LazyRWSResult s a w = LazyRWSResult (RWSResult s a w)

derive instance Newtype (LazyRWSResult s a w) _
instance (Lazy s, Lazy a, Lazy w) => Lazy (LazyRWSResult s a w) where
  defer f = LazyRWSResult $ RWSResult
    (defer $ (case _ of RWSResult s _ _ -> s) <<< unwrap <<< f)
    (defer $ (case _ of RWSResult _ a _ -> a) <<< unwrap <<< f)
    (defer $ (case _ of RWSResult _ _ w -> w) <<< unwrap <<< f)

liftLazy :: forall a. Lazy a => DL.Lazy a -> a
liftLazy la = defer \_ -> DL.force la

overM
  :: forall m s t a b
   . Newtype s a
  => Newtype t b
  => Functor m
  => (a -> s)
  -> (a -> m b)
  -> s
  -> m t
overM _ = (unsafeCoerce :: (a -> m b) -> s -> m t)

mfix2
  :: forall m a b
   . Lazy a
  => Lazy b
  => MonadFix m
  => (a -> b -> m (a /\ b))
  -> m (a /\ b)
mfix2 f = force
  <$> mfix \t -> pure <$> f (liftLazy $ fst <$> t) (liftLazy $ snd <$> t)

mfix3
  :: forall m a b c
   . Lazy a
  => Lazy b
  => Lazy c
  => MonadFix m
  => (a -> b -> c -> m (a /\ b /\ c))
  -> m (a /\ b /\ c)
mfix3 f = force <$> mfix \t -> pure <$> f
  (liftLazy $ fst <$> t)
  (liftLazy $ fst <<< snd <$> t)
  (liftLazy $ snd <<< snd <$> t)

mfix4
  :: forall m a b c d
   . Lazy a
  => Lazy b
  => Lazy c
  => Lazy d
  => MonadFix m
  => (a -> b -> c -> d -> m (a /\ b /\ c /\ d))
  -> m (a /\ b /\ c /\ d)
mfix4 f = force <$> mfix \t -> pure <$> f
  (liftLazy $ fst <$> t)
  (liftLazy $ fst <<< snd <$> t)
  (liftLazy $ fst <<< snd <<< snd <$> t)
  (liftLazy $ snd <<< snd <<< snd <$> t)

-- | A workaround for the missing Lazy instance for effects.
-- | Abuses the fact that effects are represented as functions with no
-- | arguments at runtime, and that Function does have a Lazy instance.
mfixEffect
  :: forall m a. MonadFix m => (Effect a -> m (Effect a)) -> m (Effect a)
mfixEffect f = do
  effAsFn <- mfix \(effAsFn :: Unit -> a) -> do
    eff <- f $ unsafeCoerce effAsFn
    pure $ unsafeCoerce eff
  pure $ unsafeCoerce effAsFn
