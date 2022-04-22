module Effect.Reactive.Event
  ( Behaviour
  , BehaviourIO
  , Event
  , EventIO
  , (<&)
  , (<&>)
  , animate
  , applyE
  , applyFirstE
  , bracketAnimate
  , bracketReact
  , execute
  , executeMap
  , filterApply
  , filterMapApply
  , fromAddListener
  , fromAddListener_
  , fromChanges
  , gate
  , interpretB
  , interpretB2
  , interpretE
  , interpretE2
  , mapAccum
  , newBehaviour
  , newEvent
  , partitionApply
  , partitionMapApply
  , react
  , sampleB
  , scanB
  , scanE
  , stepper
  -- , switchB
  , switchE
  -- , switchMapB
  , switchMapE
  , timeB
  , withTime
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Lazy.Lazy1 (class Lazy1)
import Control.Monad.Fix (liftLazy)
import Control.Plus (class Plus, empty)
import Data.Align (class Align, class Alignable, align, aligned)
import Data.Compactable (class Compactable, compact, separate)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Filterable (class Filterable)
import Data.Foldable (for_, traverse_)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Lazy (force)
import Data.Lazy as DL
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.These (These(..), these, theseLeft, theseRight)
import Data.TimeFn (evalTimeFn)
import Data.TimeFn as TF
import Data.Traversable (for)
import Data.Traversable.Accum (Accum)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Class (class MonadRaff, liftRaff)
import Effect.Reactive.Internal
  ( class IsParent
  , Latch
  , LatchWrite
  , ParentNode
  , Raff
  , actuate
  , addChild
  , addParent
  , cached
  , deactivate
  , emptyNode
  , fire
  , ground
  , networkTime
  , newAnimation
  , newBuffer
  , newCircuit
  , newExecute
  , newInput
  , newLatch
  , newLazyLatch
  , newOutput
  , newProcess
  , onConnected
  , readLatch
  , readNode
  , removeChild
  , removeParent
  , runLater
  , setAnimation
  , testRaff
  , toParent
  )
import Effect.Reactive.Types (Time)
import Effect.Ref as Ref
import Effect.Unlift (askUnliftEffect, unliftEffect)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------------------------
-- Events - discrete occurances at instantaneous points in time.
-------------------------------------------------------------------------------

type SomeParent t a = Exists (ParentNode t a)

newtype Event t a = Event (Raff t (SomeParent t (DL.Lazy a)))
type EventIO t a = { event :: Event t a, fire :: a -> Raff t Unit }

mkEvent
  :: forall t a b node
   . IsParent t (DL.Lazy a) b node
  => Raff t node
  -> Event t a
mkEvent = Event <<< map (mkExists <<< toParent) <<< cached

pureEvent
  :: forall t a b node. IsParent t (DL.Lazy a) b node => node -> Event t a
pureEvent = Event <<< pure <<< mkExists <<< toParent

mkEventNotCached
  :: forall t a b node
   . IsParent t (DL.Lazy a) b node
  => Raff t node
  -> Event t a
mkEventNotCached = Event <<< map (mkExists <<< toParent)

runEvent
  :: forall m t a r
   . MonadRaff t m
  => (forall b. ParentNode t (DL.Lazy a) b -> m r)
  -> Event t a
  -> m r
runEvent f (Event ra) = liftRaff ra >>= \na -> runExists f na

instance Functor (Event t) where
  map f = map mkEvent $ runEvent \nodeA -> do
    nodeB <- newProcess "FunctorEvent" (_ <<< map f)
    nodeA `addChild` nodeB
    pure nodeB

-- TODO add support for pull-based node evaluation
instance Compactable (Event t) where
  compact = map mkEvent $ runEvent \nodeMa -> do
    nodeA <- newProcess "CompactEvent$compact" \write lma -> do
      defer \_ -> traverse_ (write <<< pure) $ force lma
    nodeMa `addChild` nodeA
    pure nodeA
  separate event =
    let
      mMulti = runEvent
        ( \eitherN -> cached do
            left <- newBuffer "CompactEvent$separate$left"
            right <- newBuffer "CompactEvent$separate$right"
            newCircuit "CompactEvent$separate"
              { eitherAB: eitherN }
              { left, right }
              \inputs outputs ->
                do
                  eitherAB <- inputs.eitherAB
                  case force <$> eitherAB of
                    Just (Left a) -> outputs.left $ pure a
                    Just (Right b) -> outputs.right $ pure b
                    _ -> pure unit
            pure { left, right }
        )
        event
    in
      { left: mkEventNotCached $ _.left <$> mMulti
      , right: mkEventNotCached $ _.right <$> mMulti
      }

instance Filterable (Event t) where
  filter p = map mkEvent $ runEvent \nodeMa -> do
    nodeA <- newProcess "FilterableEvent$filter" \write a -> defer \_ ->
      when (p $ force a) $ write a
    nodeMa `addChild` nodeA
    pure nodeA
  partition p event =
    let
      mMulti = runEvent
        ( \nodeA -> cached do
            no <- newBuffer "FilterableEvent$partition$no"
            yes <- newBuffer "FilterableEvent$partition$yes"
            newCircuit "FilterableEvent$partition" { a: nodeA } { no, yes }
              \inputs outputs ->
                do
                  ma <- inputs.a
                  case force <$> ma of
                    Just a
                      | p a -> outputs.yes $ pure a
                      | otherwise -> outputs.no $ pure a
                    _ -> pure unit
            pure { no, yes }
        )
        event
    in
      { no: mkEventNotCached $ _.no <$> mMulti
      , yes: mkEventNotCached $ _.yes <$> mMulti
      }
  filterMap f = map mkEvent $ runEvent \nodeMa -> do
    nodeA <- newProcess "FilterableEvent$filterMap" \write ->
      traverse_ (write <<< pure) <<< f <<< force
    nodeMa `addChild` nodeA
    pure nodeA
  partitionMap f event =
    let
      mMulti = runEvent
        ( \nodeA -> cached do
            left <- newBuffer "FilterableEvent$partitionMap$left"
            right <- newBuffer "FilterableEvent$partitionMap$right"
            newCircuit "FilterableEvent$partitionMap" { a: nodeA }
              { left, right }
              \inputs outputs ->
                do
                  ma <- inputs.a
                  case f <<< force <$> ma of
                    Just (Left a) -> outputs.left $ pure a
                    Just (Right b) -> outputs.right $ pure b
                    _ -> pure unit
            pure { left, right }
        )
        event
    in
      { left: mkEventNotCached $ _.left <$> mMulti
      , right: mkEventNotCached $ _.right <$> mMulti
      }

instance Alt (Event t) where
  alt ea eb = mkEvent $ runEvent
    ( \a -> runEvent
        ( \b -> do
            out <- newBuffer "AltEvent"
            newCircuit "AltEvent" { a, b } { out } \inputs outputs -> do
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
            out <- newBuffer "ApplyEvent"
            newCircuit "ApplyEvent" { f, a } { out } \inputs outputs ->
              do
                mf <- inputs.f
                ma <- inputs.a
                traverse_ outputs.out $ lift2 apply mf ma
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
            out <- newBuffer "AlignEvent"
            newCircuit "AlignEvent" { a, b } { out }
              \inputs outputs -> do
                ma <- inputs.a
                mb <- inputs.b
                let
                  f' = these
                    (map (f <<< This))
                    (map (f <<< That))
                    (lift2 (map f <<< Both))
                traverse_ outputs.out $ align f' ma mb
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

instance Lazy (Event t a) where
  defer f = Event $ defer $ coerce f

instance Lazy1 (Event t) where
  defer1 = defer

newEvent :: forall t a. Raff t (EventIO t a)
newEvent = do
  input <- newInput "newEvent"
  pure
    { event: pureEvent input
    , fire: \a -> fire (pure a) input
    }

fromAddListener
  :: forall t a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Raff t (Tuple (Raff t Unit) (Event t a))
fromAddListener addListener = do
  input <- newInput "fromAddListener"
  u <- askUnliftEffect
  dispose <- liftEffect $ addListener \a -> unliftEffect u $ fire (pure a) input
  pure $ Tuple (liftEffect dispose) (pureEvent input)

fromAddListener_
  :: forall t a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Raff t (Event t a)
fromAddListener_ = map snd <<< fromAddListener

react :: forall t a. Event t a -> (a -> Effect Unit) -> Raff t (Raff t Unit)
react event eff = event # runEvent \nodeA -> do
  output <- newOutput "react" $ liftEffect <<< eff <<< DL.force
  nodeA `addChild` output
  pure $ nodeA `removeChild` output

bracketReact
  :: forall t r a
   . Event t a
  -> Effect r
  -> (r -> Effect Unit)
  -> (r -> a -> Effect Unit)
  -> Raff t (Raff t Unit)
bracketReact event acquire release eff = event # runEvent \nodeA -> do
  resourceRef <- liftEffect $ Ref.new Nothing
  output <- newOutput "bracketReact" \a -> liftEffect do
    mResource <- liftEffect $ Ref.read resourceRef
    traverse_ (flip eff (force a)) mResource
  dispose <- onConnected output do
    resource <- acquire
    Ref.write (Just resource) resourceRef
    pure do
      release resource
      Ref.write Nothing resourceRef
  nodeA `addChild` output
  pure do
    nodeA `removeChild` output
    liftEffect dispose

execute :: forall t a. Event t (Raff t a) -> Event t a
execute = executeMap identity

executeMap :: forall t a b. (a -> Raff t b) -> Event t a -> Event t b
executeMap f = map mkEvent $ runEvent \nodeA -> do
  -- FIXME build lazilly
  nodeB <- newExecute "executeMap" (map pure <<< f <<< force)
  nodeA `addChild` nodeB
  pure $ nodeB

withTime :: forall t a. Event t a -> Event t (Tuple (Time t) a)
withTime a = Tuple <$> timeB <&> a

scanE :: forall t m a. MonadRaff t m => a -> Event t (a -> a) -> m (Event t a)
scanE seed = runEvent \nodeF -> liftRaff do
  { setUpdates, latch } <- newLazyLatch "scanE" $ pure seed
  process <- newProcess "scanE" \write f -> do
    a <- readLatch latch
    write $ f <*> a
  nodeF `addChild` process
  void $ setUpdates process
  pure $ pureEvent process

switchE :: forall t m a. MonadRaff t m => Event t (Event t a) -> m (Event t a)
switchE = switchMapE identity

switchMapE
  :: forall t m a b
   . MonadRaff t m
  => (a -> Event t b)
  -> Event t a
  -> m (Event t b)
switchMapE f = runEvent \nodeA -> liftRaff do
  currentRef <- liftEffect $ Ref.new Nothing
  out <- newBuffer "switchMapE"
  process <- newProcess "switchMapE" \write -> map f >>> liftLazy >>>
    runEvent \nodeB -> do
      mCurrent <- liftEffect (Ref.read currentRef)
      traverse_ (runExists (removeParent out)) mCurrent
      out `addParent` nodeB
      liftEffect $ Ref.write (Just $ mkExists $ toParent $ nodeB) currentRef
      mb <- readNode nodeB
      traverse_ write mb
  nodeA `addChild` process
  process `addChild` out
  pure $ pureEvent out

-------------------------------------------------------------------------------
-- Behaviours - continuous functions of time
-------------------------------------------------------------------------------

type TimeFn t a = TF.TimeFn (Time t) a

newtype Behaviour t a = Behaviour
  (Raff t { updates :: LatchWrite t, latch :: Latch t (TimeFn t a) })

type BehaviourIO t a = { behaviour :: Behaviour t a, fire :: a -> Raff t Unit }

mkBehaviour
  :: forall t a
   . Raff t { updates :: LatchWrite t, latch :: Latch t (TimeFn t a) }
  -> Behaviour t a
mkBehaviour raff = unsafeCoerce cached raff

pureBehaviour
  :: forall t a
   . LatchWrite t
  -> Latch t (TimeFn t a)
  -> Behaviour t a
pureBehaviour updates latch = Behaviour $ pure $ unsafeCoerce { updates, latch }

instance Lazy (Behaviour t a) where
  defer f = Behaviour $ defer $ coerce f

instance Lazy1 (Behaviour t) where
  defer1 = defer

instance Functor (Behaviour t) where
  map f (Behaviour r) = mkBehaviour do
    b <- r
    a <- readLatch b.latch
    { setUpdates, latch } <- newLazyLatch "FunctorBehaviour" $ map f <$> a
    out <- newProcess "FunctorBehaviour" \write _ -> do
      a' <- readLatch b.latch
      write $ map f <$> a'
    b.updates `addChild` out
    updates <- setUpdates out
    pure { updates, latch }

instance Apply (Behaviour t) where
  apply (Behaviour r1) (Behaviour r2) = mkBehaviour do
    b1 <- r1
    b2 <- r2
    f <- readLatch b1.latch
    a <- readLatch b2.latch
    { setUpdates, latch } <- newLazyLatch "ApplyBehaviour" $ lift2 apply f a
    out <- newBuffer "ApplyBehaviour"
    newCircuit "ApplyBehaviour" { f: b1.updates, a: b2.updates } { out }
      \_ outputs -> do
        f' <- readLatch b1.latch
        a' <- readLatch b2.latch
        outputs.out $ lift2 apply f' a'
    updates <- setUpdates out
    pure { updates, latch }

liftTimeFn :: forall t a. TimeFn t a -> Behaviour t a
liftTimeFn fn = mkBehaviour do
  out <- ground
  { setUpdates, latch } <- newLatch "liftTimeFn" fn
  updates <- setUpdates out
  pure { updates, latch }

instance Applicative (Behaviour t) where
  pure = liftTimeFn <<< TF.K

instance Semigroup a => Semigroup (Behaviour t a) where
  append = lift2 append

instance Monoid a => Monoid (Behaviour t a) where
  mempty = pure mempty

instance HeytingAlgebra a => HeytingAlgebra (Behaviour t a) where
  ff = pure ff
  tt = pure tt
  implies = lift2 implies
  conj = lift2 conj
  disj = lift2 disj
  not = map not

instance BooleanAlgebra a => BooleanAlgebra (Behaviour t a)

instance Semiring a => Semiring (Behaviour t a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance Ring a => Ring (Behaviour t a) where
  sub = lift2 sub

instance CommutativeRing a => CommutativeRing (Behaviour t a)

instance DivisionRing a => DivisionRing (Behaviour t a) where
  recip = map recip

instance Field a => EuclideanRing (Behaviour t a) where
  degree _ = 1
  div = lift2 div
  mod = lift2 mod

newBehaviour :: forall t a. a -> Raff t (BehaviourIO t a)
newBehaviour initialValue = do
  input <- newInput "newBehaviour"
  { setUpdates, latch } <- newLatch "newBehaviour" $ pure initialValue
  latchWrite <- setUpdates input
  pure
    { behaviour: pureBehaviour latchWrite latch
    , fire: \a -> fire (TF.K a) input
    }

fromChanges
  :: forall t a
   . a
  -> ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Raff t (Behaviour t a)
fromChanges initialValue = stepper initialValue <=< fromAddListener_

animate
  :: forall t a. Behaviour t a -> (a -> Effect Unit) -> Raff t (Raff t Unit)
animate (Behaviour r) eff = do
  u <- askUnliftEffect
  { updates, latch } <- r
  animation <- newAnimation
  clearAnimationRef <- liftEffect $ Ref.new $ pure unit
  let
    clearCurrent = do
      join $ Ref.read clearAnimationRef
      Ref.write (pure unit) clearAnimationRef
    animateTimeFn = do
      fa <- unliftEffect u $ force <$> readLatch latch
      case fa of
        TF.K a -> do
          clearCurrent
          eff a
        TF.F f -> do
          clearAnimation <- setAnimation animation $ eff <<< f
          Ref.write clearAnimation clearAnimationRef
  runLater $ liftEffect $ animateTimeFn
  output <- newOutput "animate" $ const animateTimeFn
  updates `addChild` output
  pure do
    updates `removeChild` output
    liftEffect clearCurrent

bracketAnimate
  :: forall t r a
   . Behaviour t a
  -> Effect r
  -> (r -> Effect Unit)
  -> (r -> a -> Effect Unit)
  -> Raff t (Raff t Unit)
bracketAnimate (Behaviour r) acquire release eff = do
  u <- askUnliftEffect
  { updates, latch } <- r
  animation <- newAnimation
  clearAnimationRef <- liftEffect $ Ref.new $ pure unit
  resourceRef <- liftEffect $ Ref.new Nothing
  let
    clearCurrent = do
      join $ Ref.read clearAnimationRef
      Ref.write (pure unit) clearAnimationRef
    animateTimeFn = do
      fa <- unliftEffect u $ force <$> readLatch latch
      case fa of
        TF.K a -> do
          clearCurrent
          mResource <- Ref.read resourceRef
          traverse_ (flip eff a) mResource
        TF.F f -> do
          clearAnimation <- setAnimation animation \t -> do
            mResource <- Ref.read resourceRef
            traverse_ (flip eff (f t)) mResource
          Ref.write clearAnimation clearAnimationRef
  output <- newOutput "bracketAnimate" $ const animateTimeFn
  dispose <- onConnected output do
    resource <- acquire
    Ref.write (Just resource) resourceRef
    unliftEffect u $ runLater $ liftEffect animateTimeFn
    pure do
      clearCurrent
      release resource
      Ref.write Nothing resourceRef
  updates `addChild` output
  pure do
    updates `removeChild` output
    liftEffect dispose

timeB :: forall t. Behaviour t (Time t)
timeB = liftTimeFn identity

sampleB :: forall t m a. Lazy a => MonadRaff t m => Behaviour t a -> m a
sampleB (Behaviour r) = liftRaff do
  { latch } <- r
  lazyf <- readLatch latch
  time <- networkTime
  pure $ defer \_ -> evalTimeFn (force lazyf) time

unsafeSampleB :: forall t m a. MonadRaff t m => Behaviour t a -> m a
unsafeSampleB (Behaviour r) = liftRaff do
  { latch } <- r
  lazyf <- readLatch latch
  time <- networkTime
  pure $ evalTimeFn (force lazyf) time

stepper :: forall t m a. MonadRaff t m => a -> Event t a -> m (Behaviour t a)
stepper a event = liftRaff do
  { setUpdates, latch } <- newLazyLatch "stepper" $ pure $ pure a
  nodeFA <- newProcess "stepper" (_ <<< map pure)
  runLater do
    runEvent (\nodeA -> nodeA `addChild` nodeFA) event
  latchWrite <- setUpdates nodeFA
  pure $ pureBehaviour latchWrite latch

applyE :: forall t b a. Behaviour t (a -> b) -> Event t a -> Event t b
applyE (Behaviour r) = map mkEvent
  ( runEvent \nodeA -> do
      nodeB <- newProcess "applyE" \write a -> do
        { latch } <- r
        f <- readLatch latch
        time <- networkTime
        write $ flip evalTimeFn time <$> f <*> a
      nodeA `addChild` nodeB
      pure nodeB
  )

infixl 4 applyE as <&>
infixl 4 applyFirstE as <&

applyFirstE :: forall t b a. Behaviour t b -> Event t a -> Event t b
applyFirstE f g = (const <$> f) <&> g

scanB
  :: forall t m a
   . MonadRaff t m
  => a
  -> Event t (a -> a)
  -> m (Behaviour t a)
scanB seed = runEvent \nodeF -> liftRaff do
  { setUpdates, latch } <- newLazyLatch "scanB" $ pure $ pure seed
  process <- newProcess "scanB" \write f -> do
    time <- networkTime
    fa <- readLatch latch
    write $ TF.K <$> (f <*> (flip evalTimeFn time <$> fa))
  nodeF `addChild` process
  latchWrite <- setUpdates process
  pure $ pureBehaviour latchWrite latch

-- switchB
--   :: forall t m a
--    . MonadRaff t m
--   => Behaviour t a
--   -> Event t (Behaviour t a)
--   -> m (Behaviour t a)
-- switchB b = switchMapB b identity

-- switchMapB
--   :: forall t m a b
--    . MonadRaff t m
--   => Behaviour t b
--   -> (a -> Behaviour t b)
--   -> Event t a
--   -> m (Behaviour t b)
-- switchMapB (Behaviour r0) f = runEvent \nodeA -> liftRaff do
--   b0 <- r0
--   b0 <- readLatch b0.latch
--   currentRef <- liftEffect $ Ref.new b0.updates
--   { setUpdates, latch } <- newLatch $ TF.L b0
--   out <- newBuffer
--   runExists (addParent out) b0.updates
--   process <- newProcess \write la -> do
--     let Behaviour rn = liftLazy $ f <$> la
--     bn <- rn
--     out `addParent` bn.updates
--     mfb <- readNode bn.updates
--     traverse_ write mfb
--   nodeA `addChild` process
--   process `addChild` out
--   pure $ pureBehaviour out latch

--   out <- newBuffer
--   process <- newProcess \write -> map f >>> liftLazy >>> runEvent \nodeB -> do
--     mCurrent <- liftEffect (Ref.read currentRef)
--     traverse_ (runExists (removeParent out)) mCurrent
--     out `addParent` nodeB
--     liftEffect $ Ref.write (Just $ mkExists $ toParent $ nodeB) currentRef
--     mb <- readNode nodeB
--     traverse_ write mb
--   nodeA `addChild` process
--   process `addChild` out
--   pure $ pureEvent out

mapAccum
  :: forall t m s a
   . MonadRaff t m
  => s
  -> Event t (s -> Accum s a)
  -> m (Accum (Behaviour t s) (Event t a))
mapAccum seed = runEvent \nodeF -> liftRaff do
  { setUpdates, latch } <- newLazyLatch "mapAccum" $ pure $ pure seed
  value <- newBuffer "mapAccum$value"
  accum <- newBuffer "mapAccum$accum"
  newCircuit "mapAccum" { f: nodeF } { accum, value } \inputs outputs -> do
    inputs.f >>= traverse_ \lf -> do
      time <- networkTime
      la <- readLatch latch
      let acc = lf <*> (flip evalTimeFn time <$> la)
      outputs.accum $ TF.K <<< _.accum <$> acc
      outputs.value $ _.value <$> acc
  updates <- setUpdates accum
  pure { accum: pureBehaviour updates latch, value: pureEvent value }

filterApply :: forall t a. Behaviour t (a -> Boolean) -> Event t a -> Event t a
filterApply bp =
  compact <<< applyE ((\p a -> if p a then Just a else Nothing) <$> bp)

gate :: forall t. Behaviour t Boolean -> Event t ~> Event t
gate b = compact <<< applyE ((\p a -> if p then Just a else Nothing) <$> b)

filterMapApply
  :: forall t a b
   . Behaviour t (a -> Maybe b)
  -> Event t a
  -> Event t b
filterMapApply bp = compact <<< applyE (($) <$> bp)

partitionApply
  :: forall t a
   . Behaviour t (a -> Boolean)
  -> Event t a
  -> { no :: Event t a, yes :: Event t a }
partitionApply bp =
  relabel <<< separate <<< applyE
    ((\p a -> if p a then Right a else Left a) <$> bp)
  where
  relabel { left, right } = { yes: right, no: left }

partitionMapApply
  :: forall t a b c
   . Behaviour t (a -> Either b c)
  -> Event t a
  -> { left :: Event t b, right :: Event t c }
partitionMapApply bp = separate <<< applyE (($) <$> bp)

interpretE
  :: forall a b
   . (forall t. Event t a -> Raff t (Event t b))
  -> List (Maybe a)
  -> Effect (List (Maybe b))
interpretE buildNetwork inputs = do
  resultsRef <- Ref.new Nil
  testRaff \flushScheduler -> do
    { event: inputE, fire: fireE } <- newEvent
    { event: nothingE, fire: fireNothing } <- newEvent
    outputE <- buildNetwork inputE
    _ <- react (Just <$> outputE <|> nothingE) $ flip Ref.modify_ resultsRef <<<
      (:)
    actuate
    for_ inputs \input -> do
      traverse_ fireE input
      fireNothing Nothing
      flushScheduler
    deactivate
  List.reverse <$> Ref.read resultsRef

interpretE2
  :: forall a b c
   . (forall t. Event t a -> Event t b -> Raff t (Event t c))
  -> List (Maybe a)
  -> List (Maybe b)
  -> Effect (List (Maybe c))
interpretE2 buildNetwork as bs = do
  resultsRef <- Ref.new Nil
  testRaff \flushScheduler -> do
    { event: inputA, fire: fireA } <- newEvent
    { event: inputB, fire: fireB } <- newEvent
    { event: nothingE, fire: fireNothing } <- newEvent
    outputE <- buildNetwork inputA inputB
    _ <- react (Just <$> outputE <|> nothingE) $ flip Ref.modify_ resultsRef <<<
      (:)
    actuate
    for_ (aligned as bs) \theseAB -> do
      traverse_ fireA $ join $ theseLeft theseAB
      traverse_ fireB $ join $ theseRight theseAB
      fireNothing Nothing
      flushScheduler
    deactivate
  List.reverse <$> Ref.read resultsRef

interpretB
  :: forall a b
   . (forall t. Event t a -> Raff t (Behaviour t b))
  -> List (Maybe a)
  -> Effect (List b)
interpretB buildNetwork inputs = do
  testRaff \flushScheduler -> do
    { event: inputE, fire: fireE } <- newEvent
    { fire: fireAlways } <- newEvent
    outputB <- buildNetwork inputE
    actuate
    outputs <- for inputs \input -> do
      traverse_ fireE input
      fireAlways unit
      flushScheduler
      unsafeSampleB outputB
    deactivate
    pure outputs

interpretB2
  :: forall a b c
   . (forall t. Event t a -> Event t b -> Raff t (Behaviour t c))
  -> List (Maybe a)
  -> List (Maybe b)
  -> Effect (List c)
interpretB2 buildNetwork as bs = do
  testRaff \flushScheduler -> do
    { event: inputA, fire: fireA } <- newEvent
    { event: inputB, fire: fireB } <- newEvent
    { fire: fireAlways } <- newEvent
    outputB <- buildNetwork inputA inputB
    actuate
    outputs <- for (aligned as bs) \theseAB -> do
      traverse_ fireA $ join $ theseLeft theseAB
      traverse_ fireB $ join $ theseRight theseAB
      fireAlways unit
      flushScheduler
      unsafeSampleB outputB
    deactivate
    pure outputs
