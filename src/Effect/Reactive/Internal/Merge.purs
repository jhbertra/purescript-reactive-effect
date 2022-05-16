module Effect.Reactive.Internal.Merge where

import Prelude

import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), maybe')
import Effect.Class (liftEffect)
import Effect.Reactive.Internal
  ( EventRep
  , PropagateM
  , _subscribe
  , currentDepth
  , invalidDepth
  , propagate
  , raiseNowClearLater
  , unlessRaised
  , writeNowClearLater
  )
import Effect.Ref as Ref
import Effect.Ref.Maybe as RM

_mergeWithMaybeM
  :: forall a b c
   . (a -> PropagateM (Maybe c))
  -> (b -> PropagateM (Maybe c))
  -> (a -> b -> PropagateM (Maybe c))
  -> EventRep a
  -> EventRep b
  -> EventRep c
_mergeWithMaybeM mergeLeft mergeRight mergeBoth ea eb subscriber = do
  occurrenceA <- liftEffect RM.empty
  occurrenceB <- liftEffect RM.empty
  propagated <- liftEffect $ Ref.new false
  depthA <- liftEffect $ Ref.new invalidDepth
  depthB <- liftEffect $ Ref.new invalidDepth
  depth <- liftEffect $ Ref.new invalidDepth

  resultA <- _subscribe ea
    { propagate: \a -> do
        writeNowClearLater a occurrenceA
        myDepth <- liftEffect $ Ref.read depth
        propagate myDepth $ unlessRaised propagated do
          raiseNowClearLater propagated
          mb <- liftEffect $ RM.read occurrenceB
          mc <- maybe' (\_ -> mergeLeft a) (mergeBoth a) mb
          traverse_ subscriber.propagate mc
    , recalculateDepth: \newDepthA -> do
        oldDepthA <- Ref.read depthA
        unless (oldDepthA == newDepthA) do
          Ref.write newDepthA depthA
          depthB' <- Ref.read depthB
          Ref.write ((max depthB' newDepthA) + 1) depth
    }

  resultB <- _subscribe eb
    { propagate: \b -> do
        writeNowClearLater b occurrenceB
        myDepth <- liftEffect $ Ref.read depth
        propagate myDepth $ unlessRaised propagated do
          raiseNowClearLater propagated
          ma <- liftEffect $ RM.read occurrenceA
          mc <- maybe' (\_ -> mergeRight b) (\a -> mergeBoth a b) ma
          traverse_ subscriber.propagate mc
    , recalculateDepth: \newDepthB -> do
        oldDepthB <- Ref.read depthB
        unless (oldDepthB == newDepthB) do
          Ref.write newDepthB depthB
          depthA' <- Ref.read depthA
          Ref.write ((max depthA' newDepthB) + 1) depth
    }

  liftEffect do
    depthA' <- Ref.read resultA.subscription.depth
    depthB' <- Ref.read resultB.subscription.depth
    Ref.write depthA' depthA
    Ref.write depthB' depthB
    Ref.write ((max depthA' depthB') + 1) depth

  frameDepth <- currentDepth
  myDepth <- liftEffect $ Ref.read depth
  let ma = resultA.occurrence
  let mb = resultB.occurrence
  occurrence <-
    if frameDepth >= myDepth then
      -- If this event should have already fired
      -- calculate the occurrence from the results read from the parents,
      -- as they should have already propagated this frame if firing, and
      -- return it.
      case ma, mb of
        Just a, Just b -> mergeBoth a b
        Just a, Nothing -> mergeLeft a
        Nothing, Just b -> mergeRight b
        _, _ -> pure Nothing
    else do
      -- Run the initial merge later, when the depth is right. The parents
      -- may not have propagad yet, so the readings will be inaccurate
      -- until they have. We still need to write an occurrence that has
      -- occurred now.
      for_ ma \a -> writeNowClearLater a occurrenceA
      for_ mb \b -> writeNowClearLater b occurrenceB
      propagate myDepth do
        occA <- liftEffect $ RM.read occurrenceA
        occB <- liftEffect $ RM.read occurrenceB
        traverse_ subscriber.propagate =<< case occA, occB of
          Just a, Just b -> mergeBoth a b
          Just a, Nothing -> mergeLeft a
          Nothing, Just b -> mergeRight b
          _, _ -> pure Nothing
      pure Nothing

  pure
    { occurrence
    , subscription:
        { depth
        , unsubscribe: do
            resultA.subscription.unsubscribe
            resultB.subscription.unsubscribe
        }
    }
