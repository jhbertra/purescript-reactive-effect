module Effect.Reactive.RVar
  ( RVar
  , RVarIO
  , RVarControl
  , Write
  , execute
  , executeMap
  , interpret
  , lastUpdate
  , new
  , newEq
  , newWith
  , read
  , subscribe
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, class Plus)
import Control.Apply (lift2)
import Control.Monad.Fix (mfixEffect)
import Control.Plus (empty)
import Data.Align (class Align, class Alignable)
import Data.Array (uncons)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (for_, sequence_, traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Int (round, toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.These (These(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Reactive (Raff, askTime, runLater, runLater_, unsafeRunRaff)
import Effect.Reactive.Types (Canceller, Time(..), Timeline)
import Effect.Ref as Ref

type Write (t :: Timeline) a = a -> Raff t Unit
newtype BracketSubscriber' (t :: Timeline) a r = BracketSubscriber
  { start :: a -> Raff t r
  , next :: r -> Write t a
  , done :: r -> Effect Unit
  }

type BracketSubscriber (t :: Timeline) a = Exists (BracketSubscriber' t a)
type BracketSubscribe (t :: Timeline) a =
  BracketSubscriber t a -> Raff t Canceller

newtype RVar (t :: Timeline) a = RVar
  { read :: Raff t (Maybe a)
  , subscribe :: BracketSubscribe t a
  , lastUpdate :: Raff t (Time t)
  }

type RVarControl (t :: Timeline) a =
  { stop :: Raff t Unit
  , start :: Write t a
  , write :: Write t a
  }

type RVarIO (t :: Timeline) a =
  { control :: RVarControl t a
  , rvar :: RVar t a
  }

instance Functor (RVar t) where
  map f (RVar ra) = RVar ra
    { read = map f <$> ra.read
    , subscribe = runExists case _ of
        BracketSubscriber { start, next, done } ->
          ra.subscribe $ mkExists $ BracketSubscriber
            { start: start <<< f
            , done
            , next: \r -> next r <<< f
            }
    }

instance Apply (RVar t) where
  apply (RVar rf) (RVar ra) = RVar
    { read: lift2 apply rf.read ra.read
    , lastUpdate: rf.lastUpdate <> ra.lastUpdate
    , subscribe: runExists case _ of
        BracketSubscriber { start, next, done } -> do
          rRef <- liftEffect $ Ref.new Nothing
          dispose1 <- rf.subscribe $ mkExists $ BracketSubscriber
            { start: \f -> do
                mr <- liftEffect $ Ref.read rRef
                case mr of
                  Nothing -> do
                    ma <- ra.read
                    case ma of
                      Just a -> do
                        r <- start $ f a
                        liftEffect $ Ref.write (Just r) rRef
                        pure $ Just r
                      Nothing -> pure Nothing
                  Just r -> pure $ Just r
            , done: \_ -> do
                mr <- Ref.read rRef
                for_ mr \r -> do
                  Ref.write Nothing rRef
                  done r
            , next: \_ f -> do
                ma <- ra.read
                mr <- liftEffect $ Ref.read rRef
                for_ (Tuple <$> ma <*> mr) \(Tuple a r) ->
                  next r $ f a
            }
          dispose2 <- ra.subscribe $ mkExists $ BracketSubscriber
            { start: \a -> do
                mr <- liftEffect $ Ref.read rRef
                case mr of
                  Nothing -> do
                    mf <- rf.read
                    case mf of
                      Just f -> do
                        r <- start $ f a
                        liftEffect $ Ref.write (Just r) rRef
                        pure $ Just r
                      Nothing -> pure Nothing
                  Just r -> pure $ Just r
            , done: \_ -> do
                mr <- Ref.read rRef
                for_ mr \r -> do
                  Ref.write Nothing rRef
                  done r
            , next: \_ a -> do
                unlessM (eq <$> rf.lastUpdate <*> ra.lastUpdate) do
                  mf <- rf.read
                  mr <- liftEffect $ Ref.read rRef
                  for_ (Tuple <$> mf <*> mr) \(Tuple f r) ->
                    next r $ f a
            }
          pure $ dispose1 *> dispose2
    }

instance Alt (RVar t) where
  alt (RVar r1) (RVar r2) = RVar
    { read: do
        t1 <- r1.lastUpdate
        t2 <- r2.lastUpdate
        case compare t1 t2 of
          LT -> r2.read
          _ -> r1.read
    , lastUpdate: r1.lastUpdate <> r2.lastUpdate
    , subscribe: runExists case _ of
        BracketSubscriber { start, next, done } -> do
          r1Ref <- liftEffect $ Ref.new Nothing
          r2Ref <- liftEffect $ Ref.new Nothing
          let
            done' rRef r = do
              Ref.write Nothing rRef
              mr1 <- Ref.read r1Ref
              mr2 <- Ref.read r2Ref
              case mr1 <|> mr2 of
                Nothing -> done r
                _ -> pure unit
          dispose1 <- r1.subscribe $ mkExists $ BracketSubscriber
            { start: \a -> do
                mr2 <- liftEffect $ Ref.read r2Ref
                case mr2 of
                  Nothing -> do
                    r <- start a
                    liftEffect $ Ref.write (Just r) r1Ref
                    pure r
                  Just r -> pure r
            , done: done' r1Ref
            , next
            }
          dispose2 <- r2.subscribe $ mkExists $ BracketSubscriber
            { start: \a -> do
                mr1 <- liftEffect $ Ref.read r1Ref
                case mr1 of
                  Nothing -> do
                    r <- start a
                    liftEffect $ Ref.write (Just r) r2Ref
                    pure r
                  Just r -> pure r
            , done: done' r2Ref
            , next: \r a ->
                unlessM (eq <$> r1.lastUpdate <*> r2.lastUpdate) do
                  next r a
            }
          pure $ dispose1 *> dispose2
    }

instance Align (RVar t) where
  align f (RVar r1) (RVar r2) = RVar
    { read: do
        a <- r1.read
        b <- r2.read
        eval a b
    , lastUpdate: r1.lastUpdate <> r2.lastUpdate
    , subscribe: runExists case _ of
        BracketSubscriber { start, next, done } -> do
          r1Ref <- liftEffect $ Ref.new Nothing
          r2Ref <- liftEffect $ Ref.new Nothing
          let
            done' rRef r = do
              Ref.write Nothing rRef
              mr1 <- Ref.read r1Ref
              mr2 <- Ref.read r2Ref
              case mr1 <|> mr2 of
                Nothing -> done r
                _ -> pure unit
          dispose1 <- r1.subscribe $ mkExists $ BracketSubscriber
            { start: \a -> do
                mr2 <- liftEffect $ Ref.read r2Ref
                case mr2 of
                  Nothing -> do
                    r <- start $ f $ This a
                    liftEffect $ Ref.write (Just r) r1Ref
                    pure r
                  Just r -> pure r
            , done: done' r1Ref
            , next: \r a -> do
                b <- r2.read
                traverse_ (next r) =<< eval (Just a) b
            }
          dispose2 <- r2.subscribe $ mkExists $ BracketSubscriber
            { start: \b -> do
                mr1 <- liftEffect $ Ref.read r1Ref
                case mr1 of
                  Nothing -> do
                    r <- start $ f $ That b
                    liftEffect $ Ref.write (Just r) r2Ref
                    pure r
                  Just r -> pure r
            , done: done' r2Ref
            , next: \r b ->
                unlessM (eq <$> r1.lastUpdate <*> r2.lastUpdate) do
                  a <- r1.read
                  traverse_ (next r) =<< eval a (Just b)
            }
          pure $ dispose1 *> dispose2
    }
    where
    eval ma mb = do
      t1 <- r1.lastUpdate
      t2 <- r2.lastUpdate
      pure $ f <$> case ma, mb of
        Nothing, Nothing -> Nothing
        Just a, Nothing -> Just $ This a
        Nothing, Just b -> Just $ That b
        Just a, Just b -> Just case compare t1 t2 of
          GT -> This a
          LT -> That b
          EQ -> Both a b

instance Bind (RVar t) where
  bind (RVar ra) k = RVar
    { read: map join <<< traverse (\(RVar r) -> r.read) <<< map k =<< ra.read
    , lastUpdate: maybe (pure top) (\(RVar r) -> r.lastUpdate) <<< map k =<<
        ra.read
    , subscribe: runExists case _ of
        BracketSubscriber { start, next, done } -> do
          nextId <- liftEffect $ Ref.new 0
          disposersRef <- liftEffect $ Ref.new HM.empty
          rRef <- liftEffect $ Ref.new Nothing
          let
            subscribeAndAdd rb = do
              id <- liftEffect $
                Ref.modify' (\id -> { state: id + 1, value: id }) nextId
              void $ mfixEffect \dispose -> rb.subscribe $ mkExists $
                BracketSubscriber
                  { start: \a -> do
                      liftEffect $
                        Ref.modify_ (HM.insert id dispose) disposersRef
                      mr <- liftEffect $ Ref.read rRef
                      case mr of
                        Nothing -> do
                          r <- start a
                          liftEffect $ Ref.write (Just r) rRef
                          pure r
                        Just r -> do
                          next r a
                          pure r
                  , done: \r -> do
                      disposers <- Ref.modify (HM.delete id) disposersRef
                      when (HM.isEmpty disposers) do
                        liftEffect $ Ref.write Nothing rRef
                        done r
                  , next
                  }
          ra.subscribe $ mkExists $ BracketSubscriber
            { start: \a -> do
                let RVar rb = k a
                subscribeAndAdd rb
                pure unit
            , done: \_ -> sequence_ =<< Ref.read disposersRef
            , next: \_ a -> do
                let RVar rb = k a
                subscribeAndAdd rb
            }
    }

instance Applicative (RVar t) where
  pure a = RVar
    { read: pure $ pure a
    , lastUpdate: mempty
    , subscribe: runExists case _ of
        BracketSubscriber { start, done } -> do
          r <- start a
          pure $ liftEffect $ done r
    }

instance Plus (RVar t) where
  empty = RVar
    { read: pure Nothing
    , lastUpdate: pure $ top
    , subscribe: const $ pure $ pure unit
    }

instance Alignable (RVar t) where
  nil = empty

instance Alternative (RVar t)

instance Monad (RVar t)

instance Semigroup a => Semigroup (RVar t a) where
  append = lift2 append

instance Monoid a => Monoid (RVar t a) where
  mempty = pure mempty

data SubscriberRef' t a r
  = Idle (BracketSubscriber' t a r)
  | Active (BracketSubscriber' t a r) r (Time t)

type SubscriberRef t a = Exists (SubscriberRef' t a)

newWithEffect :: forall t a. (a -> a -> Boolean) -> Effect (RVarIO t a)
newWithEffect equals = do
  lastUpdateRef <- Ref.new top
  valueRef <- Ref.new Nothing
  nextSubscriberId <- Ref.new 0
  subscribersRef :: _ (_ _ (SubscriberRef t a)) <- Ref.new HM.empty
  writeLateActionCanceller <- Ref.new $ pure unit
  let

    rvar :: RVar t a
    rvar = RVar
      { read: readValue
      , lastUpdate: readLastUpdate
      , subscribe: runExists \subscriber -> do
          sId <- addSubscriber $ mkExists (Idle subscriber)
          runLater_ do
            subscribers <- readSubscribers
            ma <- readValue
            for_ (Tuple <$> ma <*> HM.lookup sId subscribers) case _ of
              Tuple a sRef -> startSubscriber a sId sRef
          pure $ removeSubscriber sId
      }

    readLastUpdate :: Raff t (Time t)
    readLastUpdate = liftEffect $ Ref.read lastUpdateRef

    readValue :: Raff t (Maybe a)
    readValue = liftEffect $ Ref.read valueRef

    writeValue :: a -> Raff t (Time t)
    writeValue a = do
      time <- askTime
      liftEffect do
        Ref.write time lastUpdateRef
        Ref.write (Just a) valueRef
      pure time

    clearValue :: Raff t Unit
    clearValue = liftEffect do
      Ref.write Nothing valueRef
      Ref.write top lastUpdateRef

    readSubscribers :: Raff t (HashMap Int (SubscriberRef t a))
    readSubscribers = liftEffect $ Ref.read subscribersRef

    addSubscriber :: SubscriberRef t a -> Raff t Int
    addSubscriber subscriber = liftEffect do
      sId <- Ref.modify'
        (\sId -> { state: sId + 1, value: sId })
        nextSubscriberId
      Ref.modify_ (HM.insert sId subscriber) subscribersRef
      pure sId

    removeSubscriber :: Int -> Effect Unit
    removeSubscriber sId = do
      subscribers <- Ref.read subscribersRef
      case HM.lookup sId subscribers of
        Nothing -> pure unit
        Just subscriber -> do
          stopSubscriber sId subscriber
          Ref.write (HM.delete sId subscribers) subscribersRef

    shouldSkipWrite :: a -> Raff t Boolean
    shouldSkipWrite value = do
      mCurrent <- readValue
      case mCurrent of
        Nothing -> pure true
        Just current -> pure $ equals value current

    write :: Write t a
    write a = unlessM (shouldSkipWrite a) do
      cancelWriteLateAction
      time <- writeValue a
      runLaterWrite do
        unsetWriteLateAction
        subscribers <- readSubscribers
        for_ subscribers $ runExists case _ of
          Active (BracketSubscriber { next }) r startTime ->
            when (startTime < time) $ next r a
          _ -> pure unit

    unsetWriteLateAction :: Raff t Unit
    unsetWriteLateAction =
      liftEffect $ Ref.write mempty writeLateActionCanceller

    cancelWriteLateAction :: Raff t Unit
    cancelWriteLateAction =
      liftEffect $ join $ Ref.read writeLateActionCanceller

    runLaterWrite :: Raff t Unit -> Raff t Unit
    runLaterWrite =
      liftEffect <<< flip Ref.write writeLateActionCanceller <=< runLater

    start :: Write t a
    start a = readValue >>= case _ of
      Just _ -> do
        Console.warn "Start called on already started RVar"
      Nothing -> do
        void $ writeValue a
        runLater_ do
          subscribers <- readSubscribers
          forWithIndex_ subscribers (startSubscriber a)

    stop :: Raff t Unit
    stop = readValue >>= case _ of
      Just _ -> do
        clearValue
        subscribers <- readSubscribers
        liftEffect $ forWithIndex_ subscribers stopSubscriber
      Nothing -> do
        Console.warn "Stop called on already stopped RVar"

    startSubscriber :: a -> Int -> SubscriberRef t a -> Raff t Unit
    startSubscriber a sId = runExists case _ of
      Idle (BracketSubscriber s) -> do
        time <- askTime
        r <- s.start a
        let sRef = mkExists (Active (BracketSubscriber s) r time)
        liftEffect $ Ref.modify_ (HM.insert sId sRef) subscribersRef
      _ -> mempty

    stopSubscriber :: Int -> SubscriberRef t a -> Effect Unit
    stopSubscriber sId = runExists case _ of
      Active s@(BracketSubscriber { done }) r _ -> do
        done r
        Ref.modify_ (HM.insert sId $ mkExists (Idle s)) subscribersRef
      _ -> mempty

    control :: RVarControl t a
    control = { start, stop, write }

  pure { control, rvar }

newWith :: forall t a. (a -> a -> Boolean) -> Raff t (RVarIO t a)
newWith = liftEffect <<< newWithEffect

newEq :: forall t a. Eq a => Raff t (RVarIO t a)
newEq = newWith eq

new :: forall t a. Raff t (RVarIO t a)
new = newWith strictEquals

read :: forall t a. RVar t a -> Raff t (Maybe a)
read (RVar r) = r.read

lastUpdate :: forall t a. RVar t a -> Raff t (Time t)
lastUpdate (RVar r) = r.lastUpdate

subscribe :: forall t a. RVar t a -> (a -> Raff t Unit) -> Raff t Canceller
subscribe (RVar r) f = r.subscribe $ mkExists $ BracketSubscriber
  { start: \a -> f a
  , done: \_ -> pure unit
  , next: \_ a -> f a
  }

execute :: forall t a. RVar t (Raff t a) -> RVar t a
execute (RVar rra) = RVar rra
  { read = join $ sequence <$> rra.read
  , subscribe = runExists case _ of
      BracketSubscriber { start, next, done } ->
        rra.subscribe $ mkExists $ BracketSubscriber
          { start: (start =<< _)
          , done
          , next: \r -> (next r =<< _)
          }
  }

executeMap :: forall t a b. (a -> Raff t b) -> RVar t a -> RVar t b
executeMap f (RVar ra) = RVar ra
  { read = ra.read >>= traverse f
  , subscribe = runExists case _ of
      BracketSubscriber { start, next, done } ->
        ra.subscribe $ mkExists $ BracketSubscriber
          { start: start <=< f
          , done
          , next: \r -> next r <=< f
          }
  }

interpret
  :: forall a b
   . Eq a
  => (forall t. RVar t a -> Raff t (RVar t b))
  -> Array (Tuple Int a)
  -> Effect (Array (Tuple Int b))
interpret f as = case uncons $ lmap (Time <<< toNumber) <$> as of
  Nothing -> pure []
  Just { head, tail } -> do
    let (Tuple t0 a0) = head
    bs <- Ref.new Nil
    { start, stop, write } <- unsafeRunRaff (Time zero) do
      { control, rvar } <- newEq
      RVar rb <- f rvar
      let
        writeWithTime writing b = whenM (liftEffect $ Ref.read writing)
          do
            Time t <- askTime
            liftEffect $ Ref.modify_ (Tuple (round t) b : _) bs
      _ <- mfixEffect \dispose ->
        rb.subscribe $ mkExists $ BracketSubscriber
          { start: \b -> do
              writing <- liftEffect $ Ref.new true
              writeWithTime writing b
              pure writing
          , next: writeWithTime
          , done: \writing -> do
              Ref.write false writing
              dispose
          }
      pure control
    unsafeRunRaff t0 $ start a0
    for_ tail \(Tuple tn an) -> do
      unsafeRunRaff tn $ write an
    unsafeRunRaff top stop
    Array.reverse <<< Array.fromFoldable <$> Ref.read bs

foreign import strictEquals :: forall a. a -> a -> Boolean
