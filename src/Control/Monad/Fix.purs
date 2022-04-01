module Control.Monad.Fix where

import Prelude

import Control.Lazy (class Lazy, defer, fix)
import Control.Monad.RWS.Trans (RWSResult(..), RWST(..), runRWST)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Control.Monad.State.Trans (StateT(..), runStateT)
import Control.Monad.Writer.Trans (WriterT(..), runWriterT)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Lazy as DL
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, error, fiberCanceler, launchAff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row as R
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
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
  mfix = Identity <<< fix <<< map unwrap

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
  mfix f = StateT \s -> mfixTuple $ flip runStateT s <<< f <<< fst

instance (Lazy w, MonadFix m, Monoid w) => MonadFix (WriterT w m) where
  mfix f = WriterT $ mfixTuple $ runWriterT <<< f <<< fst

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

mfixTuple
  :: forall m a b
   . Lazy a
  => Lazy b
  => MonadFix m
  => (Tuple a b -> m (Tuple a b))
  -> m (Tuple a b)
mfixTuple = map unwrap <<< mfix <<< overM LazyTuple

mfixRecord
  :: forall m rl r
   . RowToList r rl
  => DeferRecord rl r r
  => MonadFix m
  => ({ | r } -> m { | r })
  -> m { | r }
mfixRecord = map unwrap <<< mfix <<< overM LazyRecord

newtype LazyRecord r = LazyRecord { | r }

derive instance Newtype (LazyRecord r) _
instance (RowToList r rl, DeferRecord rl r r) => Lazy (LazyRecord r) where
  defer = LazyRecord <<< Builder.buildFromScratch
    <<< deferRecord (Proxy :: _ rl)
    <<< map unwrap

class
  DeferRecord (rl :: RowList Type) (ri :: Row Type) (ro :: Row Type)
  | rl -> ri ro where
  deferRecord :: Proxy rl -> (Unit -> { | ri }) -> Builder {} { | ro }

instance DeferRecord RL.Nil ri () where
  deferRecord _ _ = identity

instance
  ( IsSymbol label
  , R.Cons label a ro' ro
  , R.Cons label a ri' ri
  , R.Lacks label ro'
  , Lazy a
  , DeferRecord rl ri ro'
  ) =>
  DeferRecord (RL.Cons label a rl) ri ro where
  deferRecord _ f = Builder.insert label a <<< deferRecord rl f
    where
    label = Proxy :: _ label
    rl = Proxy :: _ rl
    a = defer $ Record.get label <<< f
