module Effect.Reactive
  ( Raff
  , askTime
  , launchRaff
  , launchRaff_
  , runInAff
  , runLater
  , runLater_
  , runRaff
  , runRaff_
  , unsafeRunRaff
  ) where

import Prelude

import Control.Monad.Fix (class MonadFix)
import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT(..)
  , ask
  , asks
  , local
  , runReaderT
  )
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Data.Array as Array
import Data.CatList (CatList, null)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HashSet (HashSet)
import Data.HashSet as HS
import Data.Monoid.Additive (Additive(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect, whileE)
import Effect.Aff (Aff, Fiber, delay, launchAff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Reactive.Types (Canceller, Time, Timeline)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Safe.Coerce (coerce)

newtype Raff (t :: Timeline) a = Raff (Raff' t a)

derive newtype instance Functor (Raff t)
derive newtype instance Apply (Raff t)
derive newtype instance Applicative (Raff t)
derive newtype instance Bind (Raff t)
derive newtype instance Monad (Raff t)
derive newtype instance MonadEffect (Raff t)
derive newtype instance MonadFix (Raff t)
derive newtype instance Semigroup a => Semigroup (Raff t a)
derive newtype instance Monoid a => Monoid (Raff t a)

foreign import getTime :: forall t. Effect (Time t)

runInAff :: forall a. (forall t. Raff t a) -> Aff a
runInAff r = do
  delay $ Milliseconds 0.0
  liftEffect $ runRaff r

launchRaff :: forall a. (forall t. Raff t a) -> Effect (Fiber a)
launchRaff r = launchAff (runInAff r)

launchRaff_ :: forall a. (forall t. Raff t a) -> Effect Unit
launchRaff_ r = launchAff_ (runInAff r)

runRaff :: forall a. (forall t. Raff t a) -> Effect a
runRaff (Raff r) = do
  time <- getTime
  runRaffImpl time r

unsafeRunRaff :: forall a t. Time t -> Raff t a -> Effect a
unsafeRunRaff t (Raff r) = runRaffImpl t r

runRaff_ :: forall a. (forall t. Raff t a) -> Effect Unit
runRaff_ r = void (runRaff r)

runLater :: forall t. Raff t Unit -> Raff t Canceller
runLater (Raff r) = Raff $ Raff' $ ReaderT \(RaffEnv { state, writer }) -> do
  let writer' = RaffW { lateActionCount: Additive 1, lateActions: pure r }
  RaffW { lateActionCount } <- Ref.modify (_ <> writer') writer
  let
    index = coerce lateActionCount - 1
    addCancelled (RaffS s) =
      RaffS s { cancelledActions = HS.insert index s.cancelledActions }
  pure $ Ref.modify_ addCancelled state

runLater_ :: forall t. Raff t Unit -> Raff t Unit
runLater_ = void <<< runLater

askTime :: forall t. Raff t (Time t)
askTime = Raff $ asks case _ of RaffR { time } -> time

newtype Raff' (t :: Timeline) a = Raff' (ReaderT (RaffEnv t) Effect a)

derive newtype instance Functor (Raff' t)
derive newtype instance Apply (Raff' t)
derive newtype instance Applicative (Raff' t)
derive newtype instance Bind (Raff' t)
derive newtype instance Monad (Raff' t)
derive newtype instance MonadEffect (Raff' t)
derive newtype instance MonadFix (Raff' t)
derive newtype instance Semigroup a => Semigroup (Raff' t a)
derive newtype instance Monoid a => Monoid (Raff' t a)

instance MonadAsk (RaffR t) (Raff' t) where
  ask = Raff' $ asks case _ of RaffEnv e -> e.reader

instance MonadReader (RaffR t) (Raff' t) where
  local f (Raff' r) = Raff' $ local f' r
    where
    f' (RaffEnv e) = RaffEnv e { reader = f e.reader }

instance MonadTell (RaffW t) (Raff' t) where
  tell w = Raff' do
    RaffEnv e <- ask
    liftEffect $ Ref.modify_ (_ <> w) e.writer

instance MonadWriter (RaffW t) (Raff' t) where
  listen (Raff' r) = Raff' do
    RaffEnv e <- ask
    w <- liftEffect $ Ref.read e.writer
    flip Tuple w <$> r
  pass (Raff' r) = Raff' do
    RaffEnv e <- ask
    Tuple a w <- r
    liftEffect $ Ref.modify_ w e.writer
    pure a

newtype RaffEnv (t :: Timeline) = RaffEnv
  { reader :: RaffR t
  , writer :: Ref (RaffW t)
  , state :: Ref RaffS
  }

newtype RaffR (t :: Timeline) = RaffR
  { time :: Time t
  }

newtype RaffW (t :: Timeline) = RaffW
  { lateActions :: CatList (Raff' t Unit)
  , lateActionCount :: Additive Int
  }

newtype RaffS = RaffS
  { cancelledActions :: HashSet Int
  }

instance Semigroup (RaffW t) where
  append (RaffW w1) (RaffW w2) = RaffW $ w1 <> w2

instance Monoid (RaffW t) where
  mempty = RaffW mempty

runRaffImpl :: forall t a. Time t -> Raff' t a -> Effect a
runRaffImpl time (Raff' r) = do
  writer <- Ref.new $ RaffW mempty
  state <- Ref.new $ RaffS mempty
  let reader = RaffR { time }
  let env = RaffEnv { reader, writer, state }
  a <- runReaderT r env
  flip whileE (pure unit) do
    RaffS { cancelledActions } <- Ref.read state
    RaffW { lateActions } <- Ref.read writer
    if null lateActions then
      pure false
    else do
      Ref.write (RaffW mempty) writer
      Ref.write (RaffS mempty) state
      let
        runIfNotCancelled i (Raff' action)
          | HS.member i cancelledActions = pure unit
          | otherwise = action
      runReaderT
        (foldMapWithIndex runIfNotCancelled $ Array.fromFoldable $ lateActions)
        env
      pure true
  pure a
