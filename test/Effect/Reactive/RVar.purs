module Test.Effect.Reactive.RVar (rvarSpec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Reactive.Util (Canceller)
import Data.Array (snoc)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error)
import Effect.Class (liftEffect)
import Effect.Reactive (Raff, unsafeRunRaff)
import Effect.Reactive.RVar as RVar
import Effect.Reactive.Types (Time(..))
import Effect.Ref as Ref
import Safe.Coerce (coerce)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

rvarSpec :: Spec Unit
rvarSpec = describe "RVar" do
  newEqSpec

newEqSpec :: Spec Unit
newEqSpec = before (newEq 0 :: Aff (RVar.RVarIO _ Int)) $ describe "newEq" do
  it "Reads Nothing after creation" \{ rvar } -> do
    read 0 rvar `shouldEqualM` Nothing
  it "Has maximum lastUpdate after creation" \{ rvar } -> do
    lastUpdate 0 rvar `shouldEqualM` top
  it "Reads Nothing if start not called" \{ control, rvar } -> do
    write 0 42 control
    read 1 rvar `shouldEqualM` Nothing
  it "Has maximum lastUpdate if start not called" \{ control, rvar } -> do
    write 0 42 control
    lastUpdate 1 rvar `shouldEqualM` top
  it "Reads what was provided at start" \{ control, rvar } -> do
    start 0 42 control
    read 1 rvar `shouldEqualM` Just 42
  it "Updates lastUpdate on start" \{ control, rvar } -> do
    start 0 42 control
    lastUpdate 1 rvar `shouldEqualM` 0.0
  it "Reads what was provided at last write" \{ control, rvar } -> do
    start 0 42 control
    write 1 12 control
    write 2 24 control
    read 3 rvar `shouldEqualM` Just 24
  it "Updates lastUpdate on write" \{ control, rvar } -> do
    start 0 42 control
    write 1 12 control
    write 2 24 control
    lastUpdate 3 rvar `shouldEqualM` 2.0
  it "Reads Nothing after stop" \{ control, rvar } -> do
    start 0 42 control
    write 1 12 control
    write 2 24 control
    stop 3 control
    read 4 rvar `shouldEqualM` Nothing
  it "Updates lastUpdate to top on stop" \{ control, rvar } -> do
    start 0 42 control
    write 1 12 control
    write 2 24 control
    stop 3 control
    lastUpdate 4 rvar `shouldEqualM` top
  it "Reads Nothing if written after stop" \{ control, rvar } -> do
    start 0 42 control
    stop 1 control
    write 2 12 control
    write 3 24 control
    read 4 rvar `shouldEqualM` Nothing
  it "Has max lastUpdate if written after stop" \{ control, rvar } -> do
    start 0 42 control
    stop 1 control
    write 2 12 control
    write 3 24 control
    lastUpdate 4 rvar `shouldEqualM` top
  it "Can be restarted" \{ control, rvar } -> do
    start 0 42 control
    stop 1 control
    start 2 12 control
    write 3 24 control
    read 4 rvar `shouldEqualM` Just 24
    lastUpdate 5 rvar `shouldEqualM` 3.0
  it "Forwards all write calls to connections" \{ control, rvar } -> do
    Tuple received _ <- connectRVar 0 rvar
    start 0 42 control
    write 1 21 control
    write 2 15 control
    write 3 5 control
    stop 0 control
    received `shouldEqualM` [ 42, 21, 15, 5 ]
  it "Writes the current value to new subscribers" \{ control, rvar } -> do
    start 0 42 control
    Tuple received _ <- connectRVar 1 rvar
    write 2 21 control
    write 3 15 control
    write 4 5 control
    stop 5 control
    received `shouldEqualM` [ 42, 21, 15, 5 ]
  it "Does not write after disconneting" \{ control, rvar } -> do
    start 0 42 control
    Tuple received disconnect <- connectRVar 1 rvar
    write 2 21 control
    disconnect
    write 3 15 control
    write 4 5 control
    stop 5 control
    received `shouldEqualM` [ 42, 21 ]
  it "Disconnecting does not stop the RVar" \{ control, rvar } -> do
    start 0 42 control
    Tuple _ disconnect <- connectRVar 1 rvar
    write 2 21 control
    disconnect
    write 3 15 control
    write 4 5 control
    read 5 rvar `shouldEqualM` Just 5
  it "Does not write after stopping" \{ control, rvar } -> do
    start 0 42 control
    Tuple received _ <- connectRVar 1 rvar
    write 2 21 control
    stop 5 control
    write 3 15 control
    write 4 5 control
    received `shouldEqualM` [ 42, 21 ]
  it "Writes again after restarting" \{ control, rvar } -> do
    start 0 42 control
    Tuple received _ <- connectRVar 1 rvar
    write 2 21 control
    stop 5 control
    write 3 15 control
    start 4 5 control
    write 5 12 control
    received `shouldEqualM` [ 42, 21, 5, 12 ]
  it "Does not write if disonnected while stopped" \{ control, rvar } -> do
    start 0 42 control
    Tuple received disconnect <- connectRVar 1 rvar
    write 2 21 control
    stop 5 control
    write 3 15 control
    disconnect
    start 4 5 control
    write 5 12 control
    received `shouldEqualM` [ 42, 21 ]
  it "Does not write when immediately disconnected" \{ control, rvar } -> do
    start 0 42 control
    received <- runRaff 1 do
      Tuple received disconnect <- connectRVar' rvar
      control.write 5
      liftEffect disconnect
      pure received
    received `shouldEqualM` []
  it "Does not write when immediately stopped" \{ control, rvar } -> do
    start 0 42 control
    received <- runRaff 1 do
      Tuple received _ <- connectRVar' rvar
      control.stop
      pure received
    received `shouldEqualM` []
  it "Delays sending the current value to subscribers" \{ control, rvar } -> do
    start 0 42 control
    received <- runRaff 1 do
      Tuple received _ <- connectRVar' rvar
      control.write 5
      control.write 6
      control.stop
      control.start 7
      control.write 8
      pure received
    received `shouldEqualM` [ 8 ]

runRaff :: forall t a. Int -> Raff t a -> Aff a
runRaff time = liftEffect <<< unsafeRunRaff (Time $ toNumber time)

newEq :: forall t a. Eq a => Int -> Aff (RVar.RVarIO t a)
newEq time = runRaff time RVar.newEq

connectRVar'
  :: forall t a
   . RVar.RVar t a
  -> Raff t (Tuple (Aff (Array a)) Canceller)
connectRVar' rvar = do
  buffer <- liftEffect $ Ref.new []
  dispose <- RVar.subscribe rvar $ liftEffect
    <<< flip Ref.modify_ buffer
    <<< flip snoc
  pure $ Tuple (liftEffect $ Ref.read buffer) (liftEffect dispose)

connectRVar
  :: forall t a
   . Int
  -> RVar.RVar t a
  -> Aff (Tuple (Aff (Array a)) (Aff Unit))
connectRVar time = map (map liftEffect) <<< runRaff time <<< connectRVar'

read :: forall t a. Int -> RVar.RVar t a -> Aff (Maybe a)
read time = runRaff time <<< RVar.read

write :: forall t a. Int -> a -> RVar.RVarControl t a -> Aff Unit
write time a control = runRaff time $ control.write a

start :: forall t a. Int -> a -> RVar.RVarControl t a -> Aff Unit
start time a control = runRaff time $ control.start a

stop :: forall t a. Int -> RVar.RVarControl t a -> Aff Unit
stop time control = runRaff time $ control.stop

lastUpdate :: forall t a. Int -> RVar.RVar t a -> Aff Number
lastUpdate time = coerce <<< runRaff time <<< RVar.lastUpdate

shouldEqualM
  :: forall m t. MonadThrow Error m => Show t => Eq t => m t -> t -> m Unit
shouldEqualM actualM expected = flip shouldEqual expected =<< actualM
