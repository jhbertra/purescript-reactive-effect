module Control.Lazy.Lazy1 where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Identity.Trans (IdentityT(..))
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (StateT)
import Control.Monad.Writer.Trans (WriterT(..))
import Data.Lazy as DL
import Data.List.Lazy (List)
import Data.List.Lazy.NonEmpty (NonEmptyList(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)

class Lazy1 (f :: Type -> Type) where
  defer1 :: forall a. (Unit -> f a) -> f a

instance Lazy1 (Function a) where
  defer1 = defer

instance Lazy1 DL.Lazy where
  defer1 = defer

instance Lazy1 List where
  defer1 = defer

instance Lazy1 Aff where
  defer1 = defer

instance Lazy1 NonEmptyList where
  defer1 f = NonEmptyList $ DL.defer $ DL.force <<< unwrap <<< f

instance Lazy1 (ReaderT r m) where
  defer1 f = ReaderT $ defer $ unwrap <<< f

instance Lazy1 (StateT s m) where
  defer1 = defer

instance Lazy1 (RWST r w s m) where
  defer1 = defer

instance Lazy1 m => Lazy1 (WriterT w m) where
  defer1 f = WriterT $ defer1 $ unwrap <<< f

instance Lazy1 m => Lazy1 (ExceptT e m) where
  defer1 f = ExceptT $ defer1 $ unwrap <<< f

instance Lazy1 m => Lazy1 (IdentityT m) where
  defer1 f = IdentityT $ defer1 $ unwrap <<< f
