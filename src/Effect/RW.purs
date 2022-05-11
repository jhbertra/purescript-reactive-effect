module Effect.RW where

import Prelude

import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Fix (class MonadFix, mfix)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unlift (class MonadUnliftEffect, withRunInEffect)
import Safe.Coerce (coerce)

newtype RWEffect r w a = RWE (r -> Ref w -> Effect a)

derive instance Functor (RWEffect r w)

instance Apply (RWEffect r w) where
  apply (RWE mf) (RWE ma) = RWE \r w -> do
    f <- mf r w
    a <- ma r w
    pure $ f a

instance Applicative (RWEffect r w) where
  pure a = RWE \_ _ -> pure a

instance Bind (RWEffect r w) where
  bind (RWE m) f = RWE \r w -> do
    a <- m r w
    coerce f a r w

instance Monad (RWEffect r w)

instance MonadEffect (RWEffect r w) where
  liftEffect e = RWE \_ _ -> e

instance MonadUnliftEffect (RWEffect r w) where
  withRunInEffect runAction = RWE \r w -> runAction \(RWE m) -> m r w

instance MonadBase Effect (RWEffect r w) where
  liftBase = liftEffect

instance MonadUnlift Effect (RWEffect r w) where
  withRunInBase = withRunInEffect

instance MonadFix (RWEffect r w) where
  mfix f = RWE \r w -> mfix \a -> coerce f a r w

instance MonadAsk r (RWEffect r w) where
  ask = RWE \r _ -> pure r

instance MonadReader r (RWEffect r w) where
  local f (RWE m) = RWE \r -> m $ f r

instance Semigroup w => MonadTell w (RWEffect r w) where
  tell w = RWE \_ w' -> Ref.modify_ (_ <> w) w'

instance Monoid w => MonadWriter w (RWEffect r w) where
  listen (RWE m) = RWE \r wRef -> do
    a <- m r wRef
    w <- Ref.read wRef
    pure $ Tuple a w
  pass (RWE m) = RWE \r w -> do
    Tuple a f <- m r w
    Ref.modify_ f w
    pure a

instance Monoid w => MonadRec (RWEffect r w) where
  tailRecM f a = RWE \r w -> tailRecM (f >>> case _ of RWE m -> m r w) a

instance Lazy (RWEffect r w a) where
  defer f = RWE \r w -> coerce (f unit) r w

instance Semigroup a => Semigroup (RWEffect r w a) where
  append = lift2 append

instance Monoid a => Monoid (RWEffect r w a) where
  mempty = pure mempty

runRWEffect
  :: forall r w a. Monoid w => RWEffect r w a -> r -> Effect (Tuple w a)
runRWEffect (RWE m) r = do
  w <- Ref.new mempty
  a <- m r w
  w' <- Ref.read w
  pure $ Tuple w' a

execRWEffect :: forall r w a. Monoid w => RWEffect r w a -> r -> Effect w
execRWEffect m r = do
  Tuple w _ <- runRWEffect m r
  pure w
