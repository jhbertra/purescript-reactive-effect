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
  , interpretE
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
  , switchB
  , switchE
  , switchMapB
  , switchMapE
  , timeB
  , withTime
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2)
import Control.Plus (class Plus, empty)
import Control.Reactive.TimeFn (TimeFn(..), evalTimeFn)
import Data.Align (class Align, class Alignable, align)
import Data.Compactable (class Compactable, compact, separate)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Filterable (class Filterable)
import Data.Foldable (for_, traverse_)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Lazy (force)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.These (these)
import Data.Traversable (for)
import Data.Traversable.Accum (Accum)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Reactive.Class (class MonadRaff, liftRaff)
import Effect.Reactive.Internal
  ( class IsParent
  , LatchNode
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
  , newOutput
  , newProcess
  , onConnected
  , readLatch
  , readNode
  , removeChild
  , removeParent
  , setAnimation
  , testRaff
  , toParent
  )
import Effect.Reactive.Types (Time)
import Effect.Ref as Ref
import Effect.Unlift (askUnliftEffect, unliftEffect)

-------------------------------------------------------------------------------
-- Events - discrete occurances at instantaneous points in time.
-------------------------------------------------------------------------------

newtype Event t a = Event (Raff t (Exists (ParentNode t a)))
type EventIO t a = { event :: Event t a, fire :: a -> Raff t Unit }

mkEvent :: forall t a b node. IsParent t a b node => Raff t node -> Event t a
mkEvent = Event <<< map (mkExists <<< toParent) <<< cached

pureEvent :: forall t a b node. IsParent t a b node => node -> Event t a
pureEvent = Event <<< pure <<< mkExists <<< toParent

mkEventNotCached
  :: forall t a b node. IsParent t a b node => Raff t node -> Event t a
mkEventNotCached = Event <<< map (mkExists <<< toParent)

runEvent
  :: forall m t a r
   . MonadRaff t m
  => (forall b. ParentNode t a b -> m r)
  -> Event t a
  -> m r
runEvent f (Event ra) = liftRaff ra >>= \na -> runExists f na

instance Functor (Event t) where
  map f = map mkEvent $ runEvent \nodeA -> do
    nodeB <- newProcess (_ <<< f)
    nodeA `addChild` nodeB
    pure nodeB

instance Compactable (Event t) where
  compact = map mkEvent $ runEvent \nodeMa -> do
    nodeA <- newProcess traverse_
    nodeMa `addChild` nodeA
    pure nodeA
  separate event =
    let
      mMulti = runEvent
        ( \eitherN -> cached do
            left <- newBuffer
            right <- newBuffer
            newCircuit { eitherAB: eitherN } { left, right } \inputs outputs ->
              do
                eitherAB <- inputs.eitherAB
                case eitherAB of
                  Just (Left a) -> outputs.left a
                  Just (Right b) -> outputs.right b
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
    nodeA <- newProcess \write a -> when (p a) $ write a
    nodeMa `addChild` nodeA
    pure nodeA
  partition p event =
    let
      mMulti = runEvent
        ( \nodeA -> cached do
            no <- newBuffer
            yes <- newBuffer
            newCircuit { a: nodeA } { no, yes } \inputs outputs ->
              do
                ma <- inputs.a
                case ma of
                  Just a
                    | p a -> outputs.yes a
                    | otherwise -> outputs.no a
                  _ -> pure unit
            pure { no, yes }
        )
        event
    in
      { no: mkEventNotCached $ _.no <$> mMulti
      , yes: mkEventNotCached $ _.yes <$> mMulti
      }
  filterMap f = map mkEvent $ runEvent \nodeMa -> do
    nodeA <- newProcess \write -> traverse_ write <<< f
    nodeMa `addChild` nodeA
    pure nodeA
  partitionMap f event =
    let
      mMulti = runEvent
        ( \nodeA -> cached do
            left <- newBuffer
            right <- newBuffer
            newCircuit { a: nodeA } { left, right } \inputs outputs ->
              do
                ma <- inputs.a
                case f <$> ma of
                  Just (Left a) -> outputs.left a
                  Just (Right b) -> outputs.right b
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
            out <- newBuffer
            newCircuit { a, b } { out } \inputs outputs -> do
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
            newCircuit { f, a } { out } \inputs outputs -> do
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
            newCircuit { a, b } { out } \inputs outputs -> do
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
    { event: pureEvent input
    , fire: \a -> fire a input
    }

fromAddListener
  :: forall t a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Raff t (Tuple (Raff t Unit) (Event t a))
fromAddListener addListener = do
  input <- newInput
  u <- askUnliftEffect
  dispose <- liftEffect $ addListener \a -> unliftEffect u $ fire a input
  pure $ Tuple (liftEffect dispose) (pureEvent input)

fromAddListener_
  :: forall t a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Raff t (Event t a)
fromAddListener_ = map snd <<< fromAddListener

react :: forall t a. Event t a -> (a -> Effect Unit) -> Raff t (Raff t Unit)
react event eff = event # runEvent \nodeA -> do
  output <- newOutput $ liftEffect <<< eff
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
  output <- newOutput \a -> liftEffect do
    mResource <- liftEffect $ Ref.read resourceRef
    traverse_ (flip eff a) mResource
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
  nodeB <- newExecute f
  nodeA `addChild` nodeB
  pure $ nodeB

withTime :: forall t a. Event t a -> Event t (Tuple (Time t) a)
withTime = executeMap \a -> Tuple <$> networkTime <@> a

scanE :: forall t m a. MonadRaff t m => a -> Event t (a -> a) -> m (Event t a)
scanE seed = runEvent \nodeF -> liftRaff do
  latch <- newLatch seed
  process <- newProcess \write f -> do
    a <- readLatch latch
    write $ f a
  nodeF `addChild` process
  process `addChild` latch
  pure $ pureEvent latch

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
  out <- newBuffer
  process <- newProcess \write -> f >>> runEvent \nodeB -> do
    mCurrent <- liftEffect (Ref.read currentRef)
    traverse_ (runExists (removeParent out)) mCurrent
    out `addParent` nodeB
    liftEffect $ Ref.write (Just $ mkExists $ toParent $ nodeB) currentRef
    ma <- readNode nodeB
    traverse_ write ma
  nodeA `addChild` process
  process `addChild` out
  pure $ pureEvent out

-------------------------------------------------------------------------------
-- Behaviours - continuous functions of time
-------------------------------------------------------------------------------

newtype Behaviour t a = Behaviour (Raff t (LatchNode t (TimeFn (Time t) a)))
type BehaviourIO t a = { behaviour :: Behaviour t a, fire :: a -> Raff t Unit }

mkBehaviour
  :: forall t a. Raff t (LatchNode t (TimeFn (Time t) a)) -> Behaviour t a
mkBehaviour = Behaviour <<< cached

pureBehaviour :: forall t a. LatchNode t (TimeFn (Time t) a) -> Behaviour t a
pureBehaviour = Behaviour <<< pure

instance Functor (Behaviour t) where
  map f (Behaviour rlfa) = mkBehaviour do
    lfa <- rlfa
    fa <- readLatch lfa
    lfb <- newLatch $ f <$> fa
    pfb <- newProcess (_ <<< map f)
    lfa `addChild` pfb
    pfb `addChild` lfb
    pure lfb

instance Apply (Behaviour t) where
  apply (Behaviour rlff) (Behaviour rlfa) = mkBehaviour do
    lff <- rlff
    lfa <- rlfa
    ff <- readLatch lff
    fa <- readLatch lfa
    lfb <- newLatch $ ff <*> fa
    newCircuit { ff: lff, fa: lfa } { lfb } \inputs outputs -> do
      mff <- inputs.ff
      mfa <- inputs.fa
      ff' <- maybe (readLatch lff) pure mff
      fa' <- maybe (readLatch lfa) pure mfa
      outputs.lfb $ ff' <*> fa'
    pure lfb

liftTimeFn :: forall t a. TimeFn (Time t) a -> Behaviour t a
liftTimeFn fn = mkBehaviour do
  groundNode <- ground
  latch <- newLatch fn
  groundNode `addChild` latch
  pure latch

instance Applicative (Behaviour t) where
  pure = liftTimeFn <<< K

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
  input <- newInput
  latch <- newLatch (K initialValue)
  pure
    { behaviour: pureBehaviour latch
    , fire: \a -> fire (K a) input
    }

fromChanges
  :: forall t a
   . a
  -> ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Raff t (Behaviour t a)
fromChanges initialValue = stepper initialValue <=< fromAddListener_

animate
  :: forall t a. Behaviour t a -> (a -> Effect Unit) -> Raff t (Raff t Unit)
animate (Behaviour rlfa) eff = do
  lfa <- rlfa
  animation <- newAnimation
  clearAnimationRef <- liftEffect $ Ref.new $ pure unit
  let
    clearCurrent = do
      join $ Ref.read clearAnimationRef
      Ref.write (pure unit) clearAnimationRef
    animateTimeFn fa = liftEffect case fa of
      K a -> do
        clearCurrent
        eff a
      F f -> do
        clearAnimation <- setAnimation animation $ eff <<< f
        Ref.write clearAnimation clearAnimationRef
      L lf -> animateTimeFn $ force lf
  liftEffect <<< animateTimeFn =<< readLatch lfa
  output <- newOutput animateTimeFn
  lfa `addChild` output
  pure do
    lfa `removeChild` output
    liftEffect clearCurrent

bracketAnimate
  :: forall t r a
   . Behaviour t a
  -> Effect r
  -> (r -> Effect Unit)
  -> (r -> a -> Effect Unit)
  -> Raff t (Raff t Unit)
bracketAnimate (Behaviour rlfa) acquire release eff = do
  u <- askUnliftEffect
  lfa <- rlfa
  animation <- newAnimation
  clearAnimationRef <- liftEffect $ Ref.new $ pure unit
  resourceRef <- liftEffect $ Ref.new Nothing
  let
    clearCurrent = do
      join $ Ref.read clearAnimationRef
      Ref.write (pure unit) clearAnimationRef
    animateTimeFn fa = liftEffect case fa of
      K a -> do
        clearCurrent
        mResource <- Ref.read resourceRef
        traverse_ (flip eff a) mResource
      F f -> do
        clearAnimation <- setAnimation animation \t -> do
          mResource <- Ref.read resourceRef
          traverse_ (flip eff (f t)) mResource
        Ref.write clearAnimation clearAnimationRef
      L lf -> animateTimeFn $ force lf
  liftEffect <<< animateTimeFn =<< readLatch lfa
  output <- newOutput animateTimeFn
  dispose <- onConnected output do
    resource <- acquire
    Ref.write (Just resource) resourceRef
    animateTimeFn =<< unliftEffect u (readLatch lfa)
    pure do
      clearCurrent
      release resource
      Ref.write Nothing resourceRef
  lfa `addChild` output
  pure do
    lfa `removeChild` output
    liftEffect dispose

timeB :: forall t. Behaviour t (Time t)
timeB = liftTimeFn identity

sampleB :: forall t m a. MonadRaff t m => Behaviour t a -> m a
sampleB (Behaviour rlfa) =
  liftRaff $ evalTimeFn <$> (readLatch =<< rlfa) <*> networkTime

stepper :: forall t m a. MonadRaff t m => a -> Event t a -> m (Behaviour t a)
stepper a = map K >>> runEvent \nodeA -> liftRaff do
  latch <- newLatch $ K a
  nodeA `addChild` latch
  pure $ pureBehaviour latch

applyE :: forall t b a. Behaviour t (a -> b) -> Event t a -> Event t b
applyE (Behaviour rlff) = map mkEvent
  ( runEvent \nodeA -> do
      lff <- rlff
      nodeB <- newBuffer
      newCircuit { ff: lff, a: nodeA } { b: nodeB } \inputs outputs -> do
        mff <- inputs.ff
        ma <- inputs.a
        ff <- maybe (readLatch lff) pure mff
        f <- evalTimeFn ff <$> networkTime
        traverse_ (outputs.b <<< f) ma
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
scanB seed = stepper seed <=< scanE seed

switchB
  :: forall t m a
   . MonadRaff t m
  => Behaviour t a
  -> Event t (Behaviour t a)
  -> m (Behaviour t a)
switchB b = switchMapB b identity

switchMapB
  :: forall t m a b
   . MonadRaff t m
  => Behaviour t b
  -> (a -> Behaviour t b)
  -> Event t a
  -> m (Behaviour t b)
switchMapB (Behaviour rlfb0) f = runEvent \nodeA -> liftRaff do
  currentRef <- liftEffect $ Ref.new Nothing
  lfb0 <- rlfb0
  f0 <- readLatch lfb0
  out <- newLatch f0
  process <- newProcess \write a -> do
    let Behaviour rlfb = f a
    lfb <- rlfb
    mCurrent <- liftEffect (Ref.read currentRef)
    traverse_ (removeParent out) mCurrent
    out `addParent` lfb
    liftEffect $ Ref.write (Just lfb) currentRef
    mfb <- readNode lfb
    traverse_ write mfb
  nodeA `addChild` process
  process `addChild` out
  pure $ pureBehaviour out

mapAccum
  :: forall t m s a
   . MonadRaff t m
  => s
  -> Event t (s -> Accum s a)
  -> m (Accum (Behaviour t s) (Event t a))
mapAccum seed = runEvent \nodeF -> liftRaff do
  accum <- newLatch (K seed)
  value <- newBuffer
  newCircuit { f: nodeF } { accum, value } \inputs outputs -> do
    inputs.f >>= traverse_ \f -> do
      time <- networkTime
      acc <- f <<< flip evalTimeFn time <$> readLatch accum
      outputs.accum $ K acc.accum
      outputs.value acc.value
  pure { accum: pureBehaviour accum, value: pureEvent value }

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
      sampleB outputB
    deactivate
    pure outputs
