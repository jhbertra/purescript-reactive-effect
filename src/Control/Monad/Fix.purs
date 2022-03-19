module Control.Monad.Fix where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Monad.RWS.Trans (RWSResult(..), RWST(..), runRWST)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Control.Monad.State.Trans (StateT(..), runStateT)
import Control.Monad.Writer.Trans (WriterT(..), runWriterT)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Lazy as DL
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, error, fiberCanceler, launchAff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

foreign import fixEffect
  :: forall a. ((Unit -> a) -> Effect a) -> Effect a

foreign import fixPure_ :: forall a. ((Unit -> a) -> a) -> a

fixPure :: forall a. Lazy a => (a -> a) -> a
fixPure = fixPure_ <<< lcmap defer

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
  mfix = Identity <<< fixPure <<< map unwrap

message :: String
message =
  "Control.Monad.Fix: Premature access to result of fixpoint computation."

instance MonadFix Aff where
  mfix f = makeAff \resolve -> do
    resultRef <- Ref.new Nothing
    fiber <- launchAff do
      result <- f $ defer \_ -> unsafePerformEffect do
        mResult <- Ref.read resultRef
        case mResult of
          Nothing -> do
            resolve $ Left $ error message
            unsafeThrow message
          Just a -> pure a
      liftEffect do
        Ref.write (Just result) resultRef
        resolve $ Right result
    pure $ fiberCanceler fiber

instance MonadFix Effect where
  mfix = fixEffect <<< lcmap defer

instance MonadFix (Function r) where
  mfix f r = fixPure (flip f r)

instance (MonadFix m) => MonadFix (ReaderT r m) where
  mfix f = ReaderT \r -> mfix (flip runReaderT r <<< f)

instance (Lazy s, MonadFix m) => MonadFix (StateT s m) where
  mfix f = StateT \s ->
    map unwrap
      $ mfix
      $ overM LazyTuple
      $ flip runStateT s <<< f <<< fst

instance (Lazy w, MonadFix m, Monoid w) => MonadFix (WriterT w m) where
  mfix f = WriterT
    $ map unwrap
    $ mfix
    $ overM LazyTuple
    $ runWriterT <<< f <<< fst

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
