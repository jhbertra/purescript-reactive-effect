module Control.Reactive.Moment where

import Prelude

import Control.Lazy.Lazy1 (class Lazy1)
import Control.Monad.Fix (class MonadFix)
import Data.Tuple (Tuple)

-- A class for computations that occur in a single moment of logical time.
-- Observable simulteneity is required for the semantics of an FRP
-- implementation, even though all computations require some amount of real
-- world time to perform. We therefore speak of logical time, which means
-- different things depending on the implementation.
--
-- For example, a lazy list of (Int, value) pairs can implement `MonadMoment
-- Int` by returning an infinite list of unitally increasing (Int, Int) pairs.
-- The notion of logical time here is represented by the index of a given list
-- element.
--
-- In the production implementation, events are processed in frames that are
-- scheduled whenever an input fires. The logical time of a frame is the time
-- at which the frame started running (even though events processed during the
-- frame will occur at slightly later times in real world terms).
--
-- Laws:
--
--   -- Immutability (the time is constant for the whole computation):
--   now *> m *> now = now
class (Bounded t, MonadFix m, Lazy1 m) <= MonadMoment t m | m -> t where
  -- | Add the current moment to a given computation
  withMoment :: forall a. m a -> m (Tuple t a)

-- An extension of `MonadMoment` that allows offsetting the time observed by
-- another computation.
class MonadMoment t m <= MonadAdjustMoment t m | m -> t where
  -- | Locally adjust the moment of another computation.
  adjustMoment :: forall a. (t -> t) -> m a -> m a
