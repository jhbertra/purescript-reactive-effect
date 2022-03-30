module Control.Reactive.Prim.Frame.Future
  ( FutureFrame
  , mkFutureFrame
  , subscribeFutureFrame
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, class Plus)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1)
import Control.Monad.Fix (class MonadFix)
import Control.Reactive.Moment
  ( class MonadAdjustMoment
  , class MonadMoment
  , adjustMoment
  , moment
  )
import Control.Reactive.Prim.Frame.Moment
  ( FrameMoment
  , FrameTime
  , runLater_
  , scheduleFrameEffect
  )
import Control.Reactive.Util (Canceller, Subscribe', Subscriber')
import Data.Align (class Align)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.These (thatOrBoth, thisOrBoth)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

-- A frame in the future that can be awaited.
newtype FutureFrame t = FutureFrame (Subscribe' FrameMoment t)

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
      ra <- Ref.new Nothing
      rb <- Ref.new Nothing
      scheduled <- Ref.new false
      canceller1R <- Ref.new $ pure unit
      canceller2R <- Ref.new $ pure unit
      cancelled <- Ref.new false
      let
        schedule = do
          unlessM (liftEffect $ Ref.read scheduled) do
            runLater_ $ unlessM (liftEffect $ Ref.read cancelled) do
              traverse_ subscriber =<< liftEffect do
                join $ Ref.read canceller1R
                join $ Ref.read canceller2R
                ma <- Ref.read ra
                mb <- Ref.read rb
                pure $ ma <|> mb
            liftEffect $ Ref.write true scheduled
      canceller1 <- subscribe1 $ subscriber' ra schedule
      canceller2 <- subscribe2 $ subscriber' rb schedule
      Ref.write canceller1 canceller1R
      Ref.write canceller2 canceller2R
      pure $ canceller1 *> canceller2
    where
    subscriber' ref schedule a = do
      liftEffect $ Ref.write (Just a) ref
      schedule

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
      runLater_ do
        mb <- liftEffect $ Ref.read rb
        subscriber $ f $ thisOrBoth a mb
        liftEffect $ Ref.write Nothing ra
        liftEffect $ Ref.write Nothing rb
    subscriberB subscriber ra rb b = do
      liftEffect $ Ref.write (Just b) rb
      runLater_ do
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
  pure t = FutureFrame \subscriber -> do
    subscribe <- scheduleFrameEffect (subscriber t)
    subscribe $ pure $ pure unit

-- The Plus instance refers to a time that never arrives.
instance Plus FutureFrame where
  empty = FutureFrame $ const $ pure $ pure unit

instance Alternative FutureFrame
instance Monad FutureFrame

instance MonadFix FutureFrame where
  mfix f = FutureFrame \subscriber -> do
    resultRef <- Ref.new $ defer \_ -> unsafeThrow
      "Control.Reactive.Prim.Future: Premature access to result of fixpoint computation."
    let
      FutureFrame subscribe =
        f $ defer \_ -> unsafePerformEffect $ Ref.read resultRef
    subscribe \a -> do
      liftEffect $ Ref.write a resultRef
      subscriber a

instance MonadMoment FrameTime FutureFrame where
  moment = FutureFrame \subscriber -> do
    subscribe <- scheduleFrameEffect (subscriber =<< moment)
    subscribe $ pure $ pure unit

instance MonadAdjustMoment FrameTime FutureFrame where
  adjustMoment f (FutureFrame subscribe) = FutureFrame \subscriber ->
    subscribe $ adjustMoment f <<< subscriber

-- The semigroup instance lifts the append operation over future frames
instance Semigroup t => Semigroup (FutureFrame t) where
  append = lift2 append

-- The monoid instance refers to the beginning of time.
instance Monoid t => Monoid (FutureFrame t) where
  mempty = pure mempty

subscribeFutureFrame
  :: forall t. FutureFrame t -> Subscriber' FrameMoment t -> Effect Canceller
subscribeFutureFrame (FutureFrame subscribe) = subscribe

mkFutureFrame :: forall t. Subscribe' FrameMoment t -> Effect (FutureFrame t)
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
