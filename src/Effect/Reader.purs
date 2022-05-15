module Effect.Reader where

import Prelude

import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Fix (class MonadFix, mfix)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Unlift (class MonadUnlift)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unlift (class MonadUnliftEffect, withRunInEffect)
import Safe.Coerce (coerce)

newtype ReaderEffect r a = RE (r -> Effect a)

type role ReaderEffect representational representational

derive instance Functor (ReaderEffect r)

instance Apply (ReaderEffect r) where
  apply (RE mf) (RE ma) = RE \r -> do
    f <- mf r
    a <- ma r
    pure $ f a

instance Applicative (ReaderEffect r) where
  pure a = RE \_ -> pure a

instance Bind (ReaderEffect r) where
  bind (RE m) f = RE \r -> do
    a <- m r
    coerce f a r

instance Monad (ReaderEffect r)

instance MonadEffect (ReaderEffect r) where
  liftEffect e = RE \_ -> e

instance MonadUnliftEffect (ReaderEffect r) where
  withRunInEffect runAction = RE \r -> runAction \(RE m) -> m r

instance MonadBase Effect (ReaderEffect r) where
  liftBase = liftEffect

instance MonadUnlift Effect (ReaderEffect r) where
  withRunInBase = withRunInEffect

instance MonadFix (ReaderEffect r) where
  mfix f = RE \r -> mfix \a -> coerce f a r

instance MonadAsk r (ReaderEffect r) where
  ask = RE pure

instance MonadReader r (ReaderEffect r) where
  local f (RE m) = RE \r -> m $ f r

instance MonadRec (ReaderEffect r) where
  tailRecM f seed = RE \r -> tailRecM (\a -> let RE m = f a in m r) seed

instance Semigroup a => Semigroup (ReaderEffect r a) where
  append = lift2 append

instance Monoid a => Monoid (ReaderEffect r a) where
  mempty = pure mempty

instance Lazy (ReaderEffect r a) where
  defer f = RE \r -> coerce f unit r

runReaderEffect :: forall r a. ReaderEffect r a -> r -> Effect a
runReaderEffect = coerce
