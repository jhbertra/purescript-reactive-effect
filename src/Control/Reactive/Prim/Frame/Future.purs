module Control.Reactive.Prim.Frame.Future
  ( FutureFrame
  , mkFutureFrame
  , subscribeFutureFrame
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1)
import Control.Reactive.Prim.Frame (FrameM, runLater, runOnNextFrame)
import Control.Reactive.Util (Canceller, Subscribe', Subscriber')
import Data.Align (class Align)
import Data.Maybe (Maybe(..))
import Data.These (thatOrBoth, thisOrBoth)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

-- A frame in the future that can be awaited.
newtype FutureFrame t = FutureFrame (Subscribe' FrameM t)

derive newtype instance Lazy (FutureFrame t)
instance Lazy1 FutureFrame where
  defer1 = defer

instance Functor FutureFrame where
  map f (FutureFrame subscribe) = FutureFrame \k -> subscribe $ k <<< f

-- The apply instance picks the frame that is the farthest in the future, while
-- holding onto the result of the earlier frame.
instance Apply FutureFrame where
  apply (FutureFrame subscribeF) (FutureFrame subscribeA) = FutureFrame
    \subscriber -> do
      rf <- Ref.new Nothing
      ra <- Ref.new Nothing
      cancellerF <- subscribeF $ subscriberF subscriber rf ra
      cancellerA <- subscribeA $ subscriberA subscriber rf ra
      pure do
        cancellerF
        cancellerA
        Ref.write Nothing rf
        Ref.write Nothing ra
    where
    subscriberF subscriber rf ra f = do
      ma <- liftEffect $ Ref.read ra
      case ma of
        Nothing -> liftEffect $ Ref.write (Just f) rf
        Just a -> do
          liftEffect $ Ref.write Nothing ra
          subscriber $ f a
    subscriberA subscriber rf ra a = do
      mf <- liftEffect $ Ref.read rf
      case mf of
        Nothing -> liftEffect $ Ref.write (Just a) ra
        Just f -> do
          liftEffect $ Ref.write Nothing rf
          subscriber $ f a

-- The alt instance picks the time that is the nearest in the future, with
-- a left-bias in the case of simultaneous occurrance.
instance Alt FutureFrame where
  alt (FutureFrame subscribe1) (FutureFrame subscribe2) = FutureFrame
    \subscriber -> do
      canceller2R <- Ref.new $ pure unit
      canceller1 <- subscribe1 $ subscriber' subscriber $ Ref.read canceller2R
      canceller2 <- subscribe2 $ subscriber' subscriber $ pure $ canceller1
      Ref.write canceller2 canceller2R
      pure $ canceller1 *> canceller2
    where
    subscriber' subscriber getOtherCanceller t = do
      liftEffect $ join getOtherCanceller
      subscriber t

-- The align instance picks the time that is the nearest in the future, or both
-- in the case of simultaneous occurrance (i.e. occurrance on the same frame).
instance Align FutureFrame where
  align f (FutureFrame subscribeA) (FutureFrame subscribeB) = FutureFrame
    \subscriber -> do
      ra <- Ref.new Nothing
      rb <- Ref.new Nothing
      cancellerA <- subscribeA $ subscriberA subscriber ra rb
      cancellerB <- subscribeB $ subscriberB subscriber ra rb
      pure do
        cancellerA
        cancellerB
        Ref.write Nothing ra
        Ref.write Nothing rb
    where
    subscriberA subscriber ra rb a = do
      liftEffect $ Ref.write (Just a) ra
      runLater do
        mb <- liftEffect $ Ref.read rb
        subscriber $ f $ thisOrBoth a mb
        liftEffect $ Ref.write Nothing ra
        liftEffect $ Ref.write Nothing rb
    subscriberB subscriber ra rb b = do
      liftEffect $ Ref.write (Just b) rb
      runLater do
        ma <- liftEffect $ Ref.read ra
        subscriber $ f $ thatOrBoth b ma
        liftEffect $ Ref.write Nothing ra
        liftEffect $ Ref.write Nothing rb

-- The Bind instance picks another future frame when the first one arrives.
instance Bind FutureFrame where
  bind (FutureFrame subscribe) k = FutureFrame \subscriber -> do
    innerCancellerR <- Ref.new $ pure unit
    canceller <- subscribe \t -> liftEffect do
      let FutureFrame subscribe' = k t
      innerCanceller <- subscribe' subscriber
      Ref.write innerCanceller innerCancellerR
    pure do
      canceller
      join $ Ref.read innerCancellerR

-- The Applicative instance refers to a time that has already occurred.
instance Applicative FutureFrame where
  pure t = FutureFrame \subscriber ->
    runOnNextFrame (subscriber t) *> pure (pure unit)

-- The Plus instance refers to a time that never arrives.
instance Plus FutureFrame where
  empty = FutureFrame $ const $ pure $ pure unit

instance Alternative FutureFrame
instance Monad FutureFrame

-- The semigroup instance lifts the max operation over the times
instance Ord t => Semigroup (FutureFrame t) where
  append = lift2 max

-- The monoid instance refers to the beginning of time.
instance Bounded t => Monoid (FutureFrame t) where
  mempty = pure bottom

subscribeFutureFrame
  :: forall t. FutureFrame t -> Subscriber' FrameM t -> Effect Canceller
subscribeFutureFrame (FutureFrame subscribe) = subscribe

mkFutureFrame :: forall t. Subscribe' FrameM t -> Effect (FutureFrame t)
mkFutureFrame subscribe = do
  -- To keep track of whether thre frame has occurred yet
  switchR <- Ref.new true
  pure $ FutureFrame \subscriber -> do
    switch <- Ref.read switchR
    if switch then do
      cancellerR <- Ref.new $ pure unit
      canceller <- subscribe \t -> do
        liftEffect $ Ref.write false switchR
        liftEffect $ join $ Ref.read cancellerR
        subscriber t
      Ref.write canceller cancellerR
      pure canceller
    else
      pure $ pure unit
