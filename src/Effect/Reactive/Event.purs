module Effect.Reactive.Event
  ( Event
  , EventIO
  , newEvent
  , sink
  , execute
  , executeMap
  , withTime
  , scanE
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)
import Data.Align (class Align, class Alignable, align)
import Data.Compactable (class Compactable)
import Data.Either (Either(..), hush)
import Data.Exists (Exists, mkExists, runExists)
import Data.Filterable (class Filterable, filter, filterMap)
import Data.Foldable (traverse_)
import Data.These (these)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Class (class MonadRaff, liftRaff)
import Effect.Reactive.Internal
  ( class IsParent
  , ParentNode
  , Raff
  , addChild
  , cached
  , emptyNode
  , fire
  , networkTime
  , newBuffer
  , newExecute
  , newInput
  , newLatch
  , newMulti
  , newOutput
  , newProcess
  , readLatch
  , removeChild
  , toParent
  )
import Effect.Reactive.Types (Time)

newtype Event t a = Event (Raff t (Exists (ParentNode t a)))
type EventIO t a = { event :: Event t a, fire :: a -> Raff t Unit }

mkEvent :: forall t a b node. IsParent t a b node => Raff t node -> Event t a
mkEvent = Event <<< map (mkExists <<< toParent) <<< cached

runEvent
  :: forall t a r
   . (forall b. ParentNode t a b -> Raff t r)
  -> Event t a
  -> Raff t r
runEvent f (Event ra) = ra >>= \na -> runExists f na

instance Functor (Event t) where
  map f = map mkEvent $ runEvent \nodeA -> do
    nodeB <- newProcess \_ a write -> write $ f $ a
    nodeA `addChild` nodeB
    pure nodeB

instance Compactable (Event t) where
  compact = map mkEvent $ runEvent \nodeMa -> do
    nodeA <- newProcess \_ ma write -> traverse_ write ma
    nodeMa `addChild` nodeA
    pure nodeA
  separate event =
    { left: mkEvent $ runEvent
        ( \nodeMab -> do
            nodeA <- newProcess \_ mab write -> case mab of
              Left a -> write a
              _ -> pure unit
            nodeMab `addChild` nodeA
            pure nodeA
        )
        event
    , right: mkEvent $ runEvent
        ( \nodeMab -> do
            nodeB <- newProcess \_ mab write -> case mab of
              Right b -> write b
              _ -> pure unit
            nodeMab `addChild` nodeB
            pure nodeB
        )
        event
    }

instance Filterable (Event t) where
  filter p = map mkEvent $ runEvent \nodeMa -> do
    nodeA <- newProcess \_ a write -> when (p a) $ write a
    nodeMa `addChild` nodeA
    pure nodeA
  partition p e =
    { yes: filter p e
    , no: filter (not <<< p) e
    }
  filterMap f = map mkEvent $ runEvent \nodeMa -> do
    nodeA <- newProcess \_ a write -> traverse_ write $ f a
    nodeMa `addChild` nodeA
    pure nodeA
  partitionMap f e =
    { left: filterMap (hush <<< swapEither <<< f) e
    , right: filterMap (hush <<< f) e
    }
    where
    swapEither = case _ of
      Left x -> Right x
      Right y -> Left y

instance Alt (Event t) where
  alt ea eb = mkEvent $ runEvent
    ( \a -> runEvent
        ( \b -> do
            out <- newBuffer
            _ <- newMulti { a, b } { out } \_ inputs outputs -> do
              ma <- inputs.a
              mb <- inputs.b
              traverse_ outputs.out $ ma <|> mb
            pure out
        )
        eb
    )
    ea

instance Apply (Event t) where
  apply ef ea = mkEvent $ runEvent
    ( \f -> runEvent
        ( \a -> do
            out <- newBuffer
            _ <- newMulti { f, a } { out } \_ inputs outputs -> do
              mf <- inputs.f
              ma <- inputs.a
              traverse_ outputs.out $ mf <*> ma
            pure out
        )
        ea
    )
    ef

instance Plus (Event t) where
  empty = mkEvent emptyNode

instance Align (Event t) where
  align f ea eb = mkEvent $ runEvent
    ( \a -> runEvent
        ( \b -> do
            out <- newBuffer
            _ <- newMulti { a, b } { out } \_ inputs outputs -> do
              ma <- inputs.a
              mb <- inputs.b
              traverse_ outputs.out $ align f ma mb
            pure out
        )
        eb

    )
    ea

instance Alignable (Event t) where
  nil = empty

instance Semigroup a => Semigroup (Event t a) where
  append = align $ these identity identity append

instance Semigroup a => Monoid (Event t a) where
  mempty = empty

newEvent :: forall t a. Raff t (EventIO t a)
newEvent = do
  input <- newInput
  pure
    { event: mkEvent $ pure input
    , fire: \a -> fire a input
    }

sink :: forall t a. Event t a -> (a -> Effect Unit) -> Raff t (Raff t Unit)
sink event eff = event # runEvent \nodeA -> do
  output <- newOutput $ liftEffect <<< eff
  nodeA `addChild` output
  pure $ nodeA `removeChild` output

execute :: forall t a. Event t (Raff t a) -> Event t a
execute = executeMap identity

executeMap :: forall t a b. (a -> Raff t b) -> Event t a -> Event t b
executeMap f = map mkEvent $ runEvent \nodeA -> do
  nodeB <- newExecute f
  nodeA `addChild` nodeB
  pure $ nodeB

withTime :: forall t a. Event t a -> Event t (Tuple (Time t) a)
withTime = executeMap \a -> Tuple <$> networkTime <@> a

scanE :: forall t m a. MonadRaff t m => a -> Event t (a -> a) -> m (Event t a)
scanE seed = map liftRaff $ runEvent \nodeF -> do
  latch <- newLatch seed
  process <- newProcess \_ f write -> do
    a <- readLatch latch
    write $ f a
  nodeF `addChild` process
  process `addChild` latch
  pure $ Event $ pure $ mkExists $ toParent $ latch
