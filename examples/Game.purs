module Game where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Alternative (class Alternative, empty, guard)
import Control.Lazy (class Lazy)
import Control.Monad.Fix (class MonadFix, mfix4)
import Control.Monad.Reader (ReaderT(..), ask, asks, runReaderT)
import Data.Array (range)
import Data.Either (Either(..))
import Data.Filterable (filter, partition)
import Data.Foldable
  ( class Foldable
  , fold
  , foldMap
  , foldl
  , foldlDefault
  , foldrDefault
  , for_
  , traverse_
  )
import Data.Function (applyFlipped)
import Data.Functor.Compose (Compose(..))
import Data.Int (toNumber)
import Data.List.NonEmpty (traverse1)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Number (sqrt)
import Data.Semigroup.Foldable
  ( class Foldable1
  , foldMap1
  , foldl1Default
  , foldr1Default
  )
import Data.Semigroup.Traversable
  ( class Traversable1
  , sequence1
  , sequence1Default
  )
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Reactive
  ( class MonadPull
  , class MonadRaff
  , Behaviour
  , Event
  , Raff
  , accumB
  , accumE
  , animateWithSetup
  , launchRaff_
  , liftPull
  , makeEvent
  , mapAccum_
  , performWithSetup
  , sample
  , stepper
  , timeB
  , (<&)
  , (<&>)
  , (~>)
  )
import Effect.Reactive.Internal (Time(..), subTime, zeroTime)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Ref.Maybe as RM
import Effect.Unlift (class MonadUnliftEffect)
import Graphics.Canvas
  ( CanvasElement
  , Context2D
  , Dimensions
  , Rectangle
  , TextAlign(..)
  , TextBaseline(..)
  , clearRect
  , fillRect
  , fillText
  , getCanvasDimensions
  , getCanvasElementById
  , getContext2D
  , setFillStyle
  , setFont
  , setTextAlign
  , setTextBaseline
  , setTransform
  )
import Partial.Unsafe (unsafePartial)
import Record as Record
import Safe.Coerce (coerce)
import Web.Event.EventTarget
  ( addEventListener
  , eventListener
  , removeEventListener
  )
import Web.HTML (window)
import Web.HTML.Window (cancelAnimationFrame, requestAnimationFrame)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)

-------------------------------------------------------------------------------
-- Game Monad
-------------------------------------------------------------------------------

type GameSettings t =
  { stepEvent :: Event t Unit
  }

type GameEnv t =
  { settings :: GameSettings t
  , nextId :: Ref Int
  , objects :: Ref (Map Int (Ref ObjectState))
  , keyupEvent :: Event t KeyboardEvent
  , keydownEvent :: Event t KeyboardEvent
  , dimensions :: Dimensions
  }

newtype Game t a = Game (ReaderT (GameEnv t) (Raff t) a)

derive newtype instance Functor (Game t)
derive newtype instance Apply (Game t)
derive newtype instance Applicative (Game t)
derive newtype instance Bind (Game t)
derive newtype instance Monad (Game t)
derive newtype instance MonadEffect (Game t)
derive newtype instance MonadUnliftEffect (Game t)

derive newtype instance MonadFix (Game t)
derive newtype instance Semigroup a => Semigroup (Game t a)
derive newtype instance Monoid a => Monoid (Game t a)

instance Lazy (Game t a) where
  defer f = Game $ ReaderT \gs -> coerce f unit gs

instance MonadPull t (Game t) where
  liftPull = Game <<< ReaderT <<< const <<< liftPull

instance MonadRaff t (Game t) where
  liftRaff = Game <<< ReaderT <<< const

-- | Get the game's step event.
step :: forall t. Game t (Event t Unit)
step = Game $ asks _.settings.stepEvent

-- | Get the game's step event tagged with the current time.
stepWithTime :: forall t. Game t (Event t Time)
stepWithTime = (timeB <& _) <$> step

-- | Get the game's step event tagged with the differential time between steps.
stepWithDiffTime :: forall t. Game t (Event t Time)
stepWithDiffTime =
  mapAccum_ (\t0 t1 -> { accum: t1, value: t1 `subTime` t0 }) zeroTime
    =<< stepWithTime

-- | Get the game's step event.
askDimensions :: forall t. Game t Dimensions
askDimensions = Game $ asks _.dimensions

-------------------------------------------------------------------------------
-- Game data
-------------------------------------------------------------------------------

type Vec2 =
  { x :: Number
  , y :: Number
  }

type Sprite = Context2D -> Effect Unit

type ObjectState =
  { id :: Int
  , position :: Vec2
  , sprite :: Sprite
  }

type ObjectAttributes t =
  { position :: Behaviour t Vec2
  , dimensions :: Behaviour t Dimensions
  , sprite :: Behaviour t Sprite
  }

type Object t =
  { id :: Int
  , step :: Event t Unit
  , hitBox :: Behaviour t Rectangle
  }

newObject :: forall t. ObjectAttributes t -> Game t (Object t)
newObject { position, dimensions, sprite } = do
  { nextId, objects } <- Game ask
  id <- liftEffect $ Ref.modify' (\id -> { state: id + 1, value: id }) nextId
  let
    bobject = { id, position: _, sprite: _ } <$> position <*> sprite
    addObject = do
      objectRef <- Ref.new
        { id
        , position: { x: 0.0, y: 0.0 }
        , sprite: mempty
        }
      Ref.modify_ (Map.insert id objectRef) objects
      pure objectRef
    removeObject _ = Ref.modify_ (Map.delete id) objects
  animateWithSetup bobject addObject removeObject Ref.write
  { id, step: _, hitBox: Record.union <$> position <*> dimensions } <$> step

destroyObject :: forall t. Object t -> Game t Unit
destroyObject { id } = do
  { objects } <- Game ask
  liftEffect $ Ref.modify_ (Map.delete id) objects

-------------------------------------------------------------------------------
-- Framework
-------------------------------------------------------------------------------

type GameContext =
  { canvas :: CanvasElement
  , context :: Context2D
  , dimensions :: Dimensions
  }

runGame :: forall t a. GameSettings t -> Game t a -> Raff t a
runGame settings (Game rt) = do
  nextId <- liftEffect $ Ref.new 0
  objects <- liftEffect $ Ref.new Map.empty
  w <- liftEffect window
  let wTarget = Window.toEventTarget w
  keyupEvent <- makeEvent \fire -> do
    listener <- eventListener $ KeyboardEvent.fromEvent >>> traverse_ fire
    addEventListener keyup listener false wTarget
    pure $ removeEventListener keyup listener false wTarget
  keydownEvent <- makeEvent \fire -> do
    listener <- eventListener $ KeyboardEvent.fromEvent >>> traverse_ fire
    addEventListener keydown listener false wTarget
    pure $ removeEventListener keydown listener false wTarget
  canvas <- liftEffect $ (\b -> unsafePartial $ fromJust b) <$>
    getCanvasElementById "canvas"
  context <- liftEffect $ getContext2D canvas
  dimensions@{ width, height } <- liftEffect $ getCanvasDimensions canvas
  a <- runReaderT rt
    { settings, nextId, objects, keyupEvent, keydownEvent, dimensions }
  let
    render _ = do
      os <- traverse Ref.read =<< Ref.read objects
      renderScene os context dimensions
  _ <- performWithSetup
    (pure unit)
    (\_ -> clearRect context ({ x: 0.0, y: 0.0, width, height } :: Rectangle))
    (render <$ settings.stepEvent)
  pure a

renderScene :: Map Int ObjectState -> Context2D -> Dimensions -> Effect Unit
renderScene objects context { width, height } = do
  setTransform context { a: 1.0, b: 0.0, c: 0.0, d: 1.0, e: 0.0, f: 0.0 }
  clearRect context ({ x: 0.0, y: 0.0, width, height } :: Rectangle)
  for_ objects \{ position: { x, y }, sprite } -> do
    setTransform context { a: 1.0, b: 0.0, c: 0.0, d: 1.0, e: x, f: y }
    sprite context

mkDefaultSettings :: forall t. Raff t (GameSettings t)
mkDefaultSettings = do
  stepEvent <- makeEvent \fire -> do
    frameIdRef <- RM.empty
    w <- window
    let
      fireStep = do
        fire unit
        frameId <- requestAnimationFrame fireStep w
        RM.write frameId frameIdRef
    frameId <- requestAnimationFrame fireStep w
    RM.write frameId frameIdRef
    pure do
      mFrameId <- RM.read frameIdRef
      traverse_ (flip cancelAnimationFrame w) mFrameId
  pure { stepEvent }

-------------------------------------------------------------------------------
-- Games
-------------------------------------------------------------------------------

main :: Effect Unit
main = launchAff_ $ launchRaff_ do
  settings <- mkDefaultSettings
  runGame settings pong
  pure empty

newtype Dpad a = Dpad
  { up :: a
  , down :: a
  , left :: a
  , right :: a
  }

derive instance Functor Dpad
derive instance Newtype (Dpad a) _

instance Apply Dpad where
  apply (Dpad f) (Dpad a) = Dpad
    { up: f.up a.up
    , down: f.down a.down
    , left: f.left a.left
    , right: f.right a.right
    }

instance Applicative Dpad where
  pure a = Dpad
    { up: a
    , down: a
    , left: a
    , right: a
    }

instance Bind Dpad where
  bind (Dpad a) k = Dpad
    { up: (unwrap $ k a.up).up
    , down: (unwrap $ k a.down).down
    , left: (unwrap $ k a.left).left
    , right: (unwrap $ k a.right).right
    }

instance Monad Dpad

instance Foldable Dpad where
  foldMap = foldMap1
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance Foldable1 Dpad where
  foldMap1 f (Dpad d) = f d.up
    <> f d.down
    <> f d.left
    <> f d.right
  foldl1 f = foldl1Default f
  foldr1 f = foldr1Default f

instance Traversable Dpad where
  traverse = traverse1
  sequence = sequence1

instance Traversable1 Dpad where
  traverse1 f (Dpad d) = ado
    up <- f d.up
    down <- f d.down
    left <- f d.left
    right <- f d.right
    in Dpad { up, down, left, right }
  sequence1 = sequence1Default

dpad :: forall t. Dpad String -> Game t (Behaviour t (Dpad Boolean))
dpad (Dpad { up, down, left, right }) = do
  d <- unwrap $
    { up: _, down: _, left: _, right: _ }
      <$> Compose (isKeyDown up)
      <*> Compose (isKeyDown down)
      <*> Compose (isKeyDown left)
      <*> Compose (isKeyDown right)
  pure $ Dpad <$> d

-------------------------------------------------------------------------------
-- Moving rectangle
-------------------------------------------------------------------------------

movingRectanle :: forall t. Game t Unit
movingRectanle = do
  bdpad <- dpad $ Dpad
    { up: "ArrowUp"
    , down: "ArrowDown"
    , left: "ArrowLeft"
    , right: "ArrowRight"
    }
  let
    speed = 128.0
    signumdpad = scaleVec2 speed <$> Dpad
      { up: { x: 0.0, y: -1.0 }
      , down: { x: 0.0, y: 1.0 }
      , left: { x: -1.0, y: 0.0 }
      , right: { x: 1.0, y: 0.0 }
      }
    gateVec2 true p = p
    gateVec2 false _ = { x: 0.0, y: 0.0 }
    vdpad = unwrap $ gateVec2 <$> Compose bdpad <*> Compose (pure signumdpad)
    v = unwrap <<< foldMap Additive <$> vdpad
  position <- integrate2 { x: 0.0, y: 0.0 } v
  void $ rect (pure "red") (pure { width: 100.0, height: 100.0 }) position

integrate
  :: forall t. Number -> Behaviour t Number -> Game t (Behaviour t Number)
integrate = integrateMap identity

integrateMap
  :: forall t
   . (Number -> Number)
  -> Number
  -> Behaviour t Number
  -> Game t (Behaviour t Number)
integrateMap f seed dx = do
  dt <- map (\(Time dt) -> dt * 0.001) <$> stepWithDiffTime
  accumB (map f <<< add) seed $ mul <$> dx <&> dt

integrate2
  :: forall t. Vec2 -> Behaviour t Vec2 -> Game t (Behaviour t Vec2)
integrate2 = integrateMap2 identity

integrate2WithImpulse
  :: forall t
   . Event t Vec2
  -> Vec2
  -> Behaviour t Vec2
  -> Game t (Behaviour t Vec2)
integrate2WithImpulse = integrateMap2WithImpulse identity

integrateMap2
  :: forall t
   . (Vec2 -> Vec2)
  -> Vec2
  -> Behaviour t Vec2
  -> Game t (Behaviour t Vec2)
integrateMap2 f = integrateMap2WithImpulse f empty

integrateMap2WithImpulse
  :: forall t
   . (Vec2 -> Vec2)
  -> Event t Vec2
  -> Vec2
  -> Behaviour t Vec2
  -> Game t (Behaviour t Vec2)
integrateMap2WithImpulse f impulse seed dxy = do
  dt <- map (\(Time dt) -> dt * 0.001) <$> stepWithDiffTime
  let
    update acc = f <<< case _ of
      Left t -> acc + t
      Right t -> t
  accumB update seed $ Right <$> impulse
    <|> Left <$> (flip scaleVec2 <$> dxy <&> dt)

scaleVec2 :: Number -> Vec2 -> Vec2
scaleVec2 s { x, y } = { x: s * x, y: s * y }

scaleRectangle :: Number -> Rectangle -> Rectangle
scaleRectangle s { x, y, width, height } =
  { x: s * x, y: s * y, width: s * width, height: s * height }

isKeyDown :: forall t. String -> Game t (Behaviour t Boolean)
isKeyDown keyCode = do
  up <- keyUp keyCode
  down <- keyDown keyCode
  stepper false $ false <$ up <|> true <$ down

keyDown :: forall t. String -> Game t (Event t Unit)
keyDown keyCode = do
  { keydownEvent } <- Game ask
  let isKey = eq keyCode <<< code
  pure $ void $ filter isKey keydownEvent

keyUp :: forall t. String -> Game t (Event t Unit)
keyUp keyCode = do
  { keyupEvent } <- Game ask
  let isKey = eq keyCode <<< code
  pure $ void $ filter isKey keyupEvent

rect
  :: forall t
   . Behaviour t String
  -> Behaviour t Dimensions
  -> Behaviour t Vec2
  -> Game t (Object t)
rect bfill dimensions position = newObject
  { position: position
  , dimensions
  , sprite: sprite <$> bfill <*> dimensions
  }
  where
  sprite fill { width, height } ctx = do
    setFillStyle ctx fill
    fillRect ctx { width, height, x: 0.0, y: 0.0 }

-------------------------------------------------------------------------------
-- Pong
-------------------------------------------------------------------------------

data Player
  = Player1
  | Player2

derive instance Eq Player

pong :: forall t. Game t Unit
pong = do
  pongBg
  paddle1 <- paddle Player1
  paddle2 <- paddle Player2
  miss <- ball paddle1 paddle2
  let { yes, no } = partition (eq Player1) miss
  score Player1 $ void no
  score Player2 $ void yes

score :: forall t. Player -> Event t Unit -> Game t Unit
score player point = do
  scoreB <- accumB (const <<< add 1) 0 point
  { width } <- askDimensions
  let
    scoreSprite s ctx = do
      setTextBaseline ctx BaselineTop
      setFont ctx "56px sans-serif"
      setFillStyle ctx "white"
      case player of
        Player1 -> setTextAlign ctx AlignLeft
        Player2 -> setTextAlign ctx AlignRight
      fillText ctx (show s) zero zero
  void $ newObject
    { position: pure
        { y: 10.0
        , x: width / 2.0 + 15.0 * case player of
            Player1 -> 1.0
            Player2 -> -1.0
        }
    , dimensions: zero
    , sprite: scoreSprite <$> scoreB
    }

ball :: forall t. Paddle t -> Paddle t -> Game t (Event t Player)
ball paddle1 paddle2 = do
  { height, width } <- askDimensions
  let ballSize = height / 30.0
  let ballDimensions = { width: ballSize, height: ballSize }
  let speed = width / 2.0
  position /\ _ /\ _ /\ miss <- mfix4 \position velocity startTurn _ -> do
    let ballRect = Record.union ballDimensions <$> position
    let fixedSize = (Record.union zero <$> _)
    let dball = fixedSize velocity
    paddle1Collision <- collision
      ballRect
      dball
      paddle1.object.hitBox
      (fixedSize paddle1.velocity)
    paddle2Collision <- collision
      ballRect
      dball
      paddle2.object.hitBox
      (fixedSize paddle2.velocity)
    topCollision <- collision
      ballRect
      dball
      (pure { x: 0.0, y: -ballSize, width, height: ballSize })
      (pure zero)
    bottomCollision <- collision
      ballRect
      dball
      (pure { x: 0.0, y: height, width, height: ballSize })
      (pure zero)
    score1Collision <- collision
      ballRect
      dball
      (pure { x: width * 1.25, y: 0.0, width: ballSize, height })
      (pure zero)
    score2Collision <- collision
      ballRect
      dball
      (pure { x: -(width * 0.25 + ballSize), y: 0.0, width: ballSize, height })
      (pure zero)
    let
      startTurn' = void $ score1Collision <|> score2Collision
      switchSides Player1 = const Player2
      switchSides Player2 = const Player1
    serving <- accumE switchSides Player1 startTurn
    let
      bumper c v = case c.direction of
        T
          | v.y == 0.0 -> v { y = -speed }
          | otherwise -> v { y = -v.y }
        B
          | v.y == 0.0 -> v { y = speed }
          | otherwise -> v { y = -v.y }
        L -> v { x = -v.x, y = 4.0 * (centre c.r1 - centre c.r2).y }
        R -> v { x = -v.x, y = 4.0 * (centre c.r1 - centre c.r2).y }
        _ -> { x: -v.x, y: -v.y }
    let
      bumperAccelleration = map bumper $ foldl alt empty
        [ paddle1Collision, paddle2Collision, topCollision, bottomCollision ]
      scoreAccelleration =
        (\player _ -> initVelocity speed player) <$> serving
      accelleration = bumperAccelleration <|> scoreAccelleration
    velocity' <- accumB applyFlipped (initVelocity speed Player1) accelleration
    position' <- integrate2WithImpulse
      (initPosition width height ballSize <$> serving)
      (initPosition width height ballSize Player1)
      velocity'
    let miss = Player1 <$ score1Collision <|> Player2 <$ score2Collision
    pure $ position' /\ velocity' /\ startTurn' /\ miss
  void $ rect
    (pure "white")
    (pure ballDimensions)
    position
  pure miss
  where
  initVelocity speed player =
    { x: case player of
        Player1 -> speed
        Player2 -> -speed
    , y: 0.0
    }
  initPosition width height ballSize player =
    { y: 0.5 * (height - ballSize)
    , x: 0.5 * width + case player of
        Player1 -> ballSize
        Player2 -> -ballSize
    }

data CollisionDirection
  = TL
  | T
  | TR
  | R
  | BR
  | B
  | BL
  | L

type Collision =
  { point :: Vec2
  , direction :: CollisionDirection
  , r1 :: Rectangle
  , r2 :: Rectangle
  }

collision
  :: forall t
   . Behaviour t Rectangle
  -> Behaviour t Rectangle
  -> Behaviour t Rectangle
  -> Behaviour t Rectangle
  -> Game t (Event t Collision)
collision box1B dbox1B box2B dbox2B = do
  stepDt <- stepWithDiffTime
  pure $ stepDt ~> \(Time dt) -> do
    let dt' = dt * 0.001
    box1 <- sample box1B
    dbox1 <- sample $ scaleRectangle dt' <$> dbox1B
    box2 <- sample box2B
    dbox2 <- sample $ scaleRectangle dt' <$> dbox2B
    let box1' = box1 + dbox1
    let box2' = box2 + dbox2
    let mOverlap = overlap box1 box2
    let mOverlap' = overlap box1' box2'
    pure case mOverlap of
      Just _ -> Nothing
      Nothing -> do
        overlapRect <- mOverlap'
        let
          point = centre overlapRect
          direction = case compare overlapRect.width overlapRect.height of
            GT
              | dbox1.y - (dbox2.y + dbox2.height) < 0.0 -> B
              | otherwise -> T
            LT
              | dbox1.x - (dbox2.x + dbox2.width) < 0.0 -> L
              | otherwise -> R
            EQ
              | dbox1.y - (dbox2.y + dbox2.height) < 0.0 ->
                  if dbox1.x - (dbox2.x + dbox2.width) < 0.0 then BR else BL
              | otherwise ->
                  if dbox1.x - (dbox2.x + dbox2.width) < 0.0 then TR else TL
        pure { point, direction, r1: box1, r2: box2 }

overlap :: Rectangle -> Rectangle -> Maybe Rectangle
overlap r1 r2 = do
  let x = max r1.x r2.x
  let x' = min (r1.x + r1.width) (r2.x + r2.width)
  guard $ x <= x'
  let y = max r1.y r2.y
  let y' = min (r1.y + r1.height) (r2.y + r2.height)
  guard $ y <= y'
  pure { x, y, width: x' - x, height: y' - y }

asum :: forall t f a. Foldable t => Alternative f => t (f a) -> f a
asum = foldl alt empty

centre :: Rectangle -> Vec2
centre { x, y, width, height } =
  { x: x + (0.5 * width), y: y + (0.5 * height) }

towards :: Vec2 -> Vec2 -> Vec2
towards p0 p1 =
  let
    x = p1.x - p0.y
    y = p1.y - p0.y
  in
    unitVector { x, y }

unitVector :: Vec2 -> Vec2
unitVector v = scaleVec2 (1.0 / magnitude v) v

magnitude :: Vec2 -> Number
magnitude { x, y } = sqrt $ x * x + y * y

type Paddle t =
  { object :: Object t
  , velocity :: Behaviour t Vec2
  }

paddle :: forall t. Player -> Game t (Paddle t)
paddle player = do
  upPressed <- isKeyDown case player of
    Player1 -> "KeyK"
    Player2 -> "KeyD"
  downPressed <- isKeyDown case player of
    Player1 -> "KeyJ"
    Player2 -> "KeyF"
  { width, height } <- askDimensions
  let
    speed :: Behaviour t Number
    speed = pure height

    vspeed =
      speed * unwrap (fold [ negSign upPressed, posSign downPressed ])

    paddleWidth = height / 30.0
    paddleHeight = height / 6.0
    paddleDimensions = { width: paddleWidth, height: paddleHeight }

    x = case player of
      Player1 -> width - paddleWidth - paddleWidth
      Player2 -> paddleWidth

    yStart = 0.5 * (height - paddleHeight)

    yMin = paddleWidth
    yMax = height - paddleWidth - paddleHeight

  y <- integrateMap (clamp yMin yMax) yStart vspeed

  let position = { x, y: _ } <$> y
  object <- rect (pure "white") (pure paddleDimensions) position
  pure { object, velocity: { x: 0.0, y: _ } <$> vspeed }

negSign :: forall t. Behaviour t Boolean -> Additive (Behaviour t Number)
negSign = Additive <<< map if _ then -1.0 else 0.0

posSign :: forall t. Behaviour t Boolean -> Additive (Behaviour t Number)
posSign = Additive <<< map if _ then 1.0 else 0.0

pongBg :: forall t. Game t Unit
pongBg = do
  solidBackground "black"
  { height, width } <- askDimensions
  let netSegments = 15
  let sectHeight = height / toNumber netSegments
  let rectDim = sectHeight * 0.5
  let x = 0.5 * (width - rectDim)
  for_ (range 0 $ netSegments - 1) \i -> rect
    (pure "white")
    (pure { width: rectDim, height: rectDim })
    (pure { x, y: toNumber i * sectHeight })

solidBackground :: forall t. String -> Game t Unit
solidBackground fill = do
  { width, height } <- askDimensions
  void $ newObject
    { position: pure { x: 0.0, y: 0.0 }
    , dimensions: pure { width, height }
    , sprite: pure \ctx -> do
        setFillStyle ctx fill
        fillRect ctx { x: 0.0, y: 0.0, width, height }
    }
