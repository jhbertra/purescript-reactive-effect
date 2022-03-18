module Control.Monad.Fix where

import Prelude

import Control.Monad.RWS.Trans (RWSResult(..), RWST(..), runRWST)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Control.Monad.State.Trans (StateT(..), runStateT)
import Control.Monad.Writer.Trans (WriterT(..), runWriterT)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Aff, error, fiberCanceler, launchAff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

foreign import fixEffect
  :: forall a. ((Unit -> a) -> Effect a) -> Effect a

foreign import fixPure_ :: forall a. ((Unit -> a) -> a) -> a

fixPure :: forall a. (Lazy a -> a) -> a
fixPure = fixPure_ <<< lcmap defer

-- | Type class for monads that support fixpoints.
-- |
-- | `mfix f` runs `f` once with the eventual result of `f` as input. Make sure
-- | not to apply the supplied function until the computation returned; else
-- | a dynamic error will be thrown.
class (Monad m) <= MonadFix m where
  mfix :: forall a. (Lazy a -> m a) -> m a

instance monadFixRWST :: (Monoid w, MonadFix m) => MonadFix (RWST r w s m) where
  mfix f = RWST \r s -> mfix \la -> runRWST
    (f $ defer \_ -> case force la of RWSResult _ a _ -> a)
    r
    s

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

instance (MonadFix m) => MonadFix (StateT s m) where
  mfix f = StateT \s -> mfix $ flip runStateT s <<< f <<< map fst

instance (MonadFix m, Monoid w) => MonadFix (WriterT w m) where
  mfix f = WriterT $ mfix $ runWriterT <<< f <<< map fst
