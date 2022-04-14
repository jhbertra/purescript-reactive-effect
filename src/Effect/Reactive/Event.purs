module Effect.Reactive.Event (Event, EventIO, newEvent, sink) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus)
import Data.Compactable (class Compactable)
import Data.Either (Either(..), hush)
import Data.Filterable (class Filterable, filter, filterMap)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( ExistsNode
  , Raff
  , addChild
  , cached
  , emptyNode
  , fire
  , mkExistsNode
  , newBuffer
  , newInput
  , newMulti
  , newOutput
  , newProcess
  , removeChild
  , runExistsNode
  )
import Safe.Coerce (coerce)

newtype Event t a = Event (Raff t (ExistsNode t a))
type EventIO t a = { event :: Event t a, fire :: a -> Raff t Unit }

mkEvent :: forall t a. Raff t (ExistsNode t a) -> Event t a
mkEvent = coerce (cached :: Raff t (ExistsNode t a) -> _ _ _)

instance Functor (Event t) where
  map f (Event ra) = mkEvent do
    existsNode <- ra
    runExistsNode existsNode \nodeA -> do
      nodeB <- newProcess \_ a write -> write $ f $ a
      nodeA `addChild` nodeB
      pure $ mkExistsNode nodeB

instance Compactable (Event t) where
  compact (Event rma) = mkEvent do
    existsNode <- rma
    runExistsNode existsNode \nodeMa -> do
      nodeA <- newProcess \_ ma write -> traverse_ write ma
      nodeMa `addChild` nodeA
      pure $ mkExistsNode nodeA
  separate (Event rmab) =
    { left: mkEvent do
        existsNode <- rmab
        runExistsNode existsNode \nodeMab -> do
          nodeA <- newProcess \_ mab write -> case mab of
            Left a -> write a
            _ -> pure unit
          nodeMab `addChild` nodeA
          pure $ mkExistsNode nodeA
    , right: mkEvent do
        existsNode <- rmab
        runExistsNode existsNode \nodeMab -> do
          nodeB <- newProcess \_ mab write -> case mab of
            Right b -> write b
            _ -> pure unit
          nodeMab `addChild` nodeB
          pure $ mkExistsNode nodeB
    }

instance Filterable (Event t) where
  filter p (Event ra) = mkEvent do
    existsNode <- ra
    runExistsNode existsNode \nodeMa -> do
      nodeA <- newProcess \_ a write -> when (p a) $ write a
      nodeMa `addChild` nodeA
      pure $ mkExistsNode nodeA
  partition p e =
    { yes: filter p e
    , no: filter (not <<< p) e
    }
  filterMap f (Event ra) = mkEvent do
    existsNode <- ra
    runExistsNode existsNode \nodeMa -> do
      nodeA <- newProcess \_ a write -> traverse_ write $ f a
      nodeMa `addChild` nodeA
      pure $ mkExistsNode nodeA
  partitionMap f e =
    { left: filterMap (hush <<< swapEither <<< f) e
    , right: filterMap (hush <<< f) e
    }
    where
    swapEither = case _ of
      Left x -> Right x
      Right y -> Left y

instance Alt (Event t) where
  alt (Event ra) (Event rb) = mkEvent do
    ea <- ra
    eb <- rb
    runExistsNode ea \a -> do
      runExistsNode eb \b -> do
        out <- newBuffer
        _ <- newMulti { a, b } { out } \_ inputs outputs -> do
          ma <- inputs.a
          mb <- inputs.b
          traverse_ outputs.out $ ma <|> mb
        pure $ mkExistsNode out

instance Plus (Event t) where
  empty = mkEvent $ mkExistsNode <$> emptyNode

newEvent :: forall t a. Raff t (EventIO t a)
newEvent = do
  input <- newInput
  pure
    { event: mkEvent $ pure $ mkExistsNode input
    , fire: \a -> fire a input
    }

sink :: forall t a. Event t a -> (a -> Effect Unit) -> Raff t (Raff t Unit)
sink (Event ra) eff = do
  output <- newOutput $ liftEffect <<< eff
  existsNode <- ra
  runExistsNode existsNode \nodeA -> do
    nodeA `addChild` output
    pure $ nodeA `removeChild` output
