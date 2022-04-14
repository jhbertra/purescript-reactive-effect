module Effect.Reactive.Class where

import Prelude

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Free.Trans (FreeT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Effect.Class (class MonadEffect)
import Effect.Reactive.Internal (Raff)

class MonadEffect m <= MonadRaff t m where
  liftRaff :: Raff t ~> m

instance MonadRaff t (Raff t) where
  liftRaff = identity

instance MonadRaff t m => MonadRaff t (ReaderT r m) where
  liftRaff = lift <<< liftRaff

instance (MonadRaff t m, Monoid r) => MonadRaff t (WriterT r m) where
  liftRaff = lift <<< liftRaff

instance MonadRaff t m => MonadRaff t (StateT r m) where
  liftRaff = lift <<< liftRaff

instance MonadRaff t m => MonadRaff t (ContT r m) where
  liftRaff = lift <<< liftRaff

instance MonadRaff t m => MonadRaff t (ExceptT r m) where
  liftRaff = lift <<< liftRaff

instance MonadRaff t m => MonadRaff t (ListT m) where
  liftRaff = lift <<< liftRaff

instance MonadRaff t m => MonadRaff t (MaybeT m) where
  liftRaff = lift <<< liftRaff

instance (MonadRaff t m, Monoid w) => MonadRaff t (RWST r w s m) where
  liftRaff = lift <<< liftRaff

instance (MonadRaff t m, Functor f) => MonadRaff t (FreeT f m) where
  liftRaff = lift <<< liftRaff
