module Effect.Reactive.Event
  ( Event
  , EventIO
  , newEvent
  , sink
  , execute
  , executeMap
  , withTime
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)
import Data.Align (class Align, class Alignable, align)
import Data.Compactable (class Compactable)
import Data.Either (Either(..), hush)
import Data.Filterable (class Filterable, filter, filterMap)
import Data.Foldable (traverse_)
import Data.These (these)
import Data.Tuple (Tuple(..))
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
  , networkTime
  , newBuffer
  , newExecute
  , newInput
  , newMulti
  , newOutput
  , newProcess
  , removeChild
  , runExistsNode
  )
import Effect.Reactive.Types (Time)
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

instance Apply (Event t) where
  apply (Event rf) (Event ra) = mkEvent do
    ef <- rf
    ea <- ra
    runExistsNode ef \f -> do
      runExistsNode ea \a -> do
        out <- newBuffer
        _ <- newMulti { f, a } { out } \_ inputs outputs -> do
          mf <- inputs.f
          ma <- inputs.a
          traverse_ outputs.out $ mf <*> ma
        pure $ mkExistsNode out

instance Plus (Event t) where
  empty = mkEvent $ mkExistsNode <$> emptyNode

instance Align (Event t) where
  align f (Event ra) (Event rb) = mkEvent do
    ea <- ra
    eb <- rb
    runExistsNode ea \a -> do
      runExistsNode eb \b -> do
        out <- newBuffer
        _ <- newMulti { a, b } { out } \_ inputs outputs -> do
          ma <- inputs.a
          mb <- inputs.b
          traverse_ outputs.out $ align f ma mb
        pure $ mkExistsNode out

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

execute :: forall t a. Event t (Raff t a) -> Event t a
execute = executeMap identity

executeMap :: forall t a b. (a -> Raff t b) -> Event t a -> Event t b
executeMap f (Event ra) = mkEvent do
  ea <- ra
  runExistsNode ea \nodeA -> do
    nodeB <- newExecute f
    nodeA `addChild` nodeB
    pure $ mkExistsNode $ nodeB

withTime :: forall t a. Event t a -> Event t (Tuple (Time t) a)
withTime = executeMap \a -> Tuple <$> networkTime <@> a
