module Stopwatch where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Fix (mfix)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Reactive
  ( class MonadRaff
  , Behaviour
  , Event
  , Raff
  , accumB
  , animateWithSetup
  , launchRaff_
  , makeEvent
  , timeB
  , (<&)
  )
import Effect.Reactive.Internal (Time(..), subTime)
import Web.DOM.ChildNode (remove)
import Web.DOM.Document (createElement)
import Web.DOM.Element (Element)
import Web.DOM.Element as E
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild, setTextContent)
import Web.Event.Event (EventType)
import Web.Event.Event as Web
import Web.Event.EventTarget
  ( EventTarget
  , addEventListener
  , eventListener
  , removeEventListener
  )
import Web.HTML (window)
import Web.HTML.Event.EventTypes (click)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as HE
import Web.HTML.Window (document)

-------------------------------------------------------------------------------
-- The business logic
-------------------------------------------------------------------------------

data State
  = NotStarted
  | Running Time
  | Stopped Time Time

cycleState :: State -> Time -> State
cycleState NotStarted now = Running now
cycleState (Running startedAt) now = Stopped startedAt now
cycleState _ _ = NotStarted

fmtTime :: Time -> String
fmtTime (Time ms) = show (toNumber (round ms) / 1000.0) <> "s"

renderTime :: Time -> State -> String
renderTime _ NotStarted = "0s"
renderTime now (Running started) = fmtTime $ now `subTime` started
renderTime _ (Stopped started stopped) = fmtTime $ stopped `subTime` started

renderButton :: State -> String
renderButton NotStarted = "Start"
renderButton (Running _) = "Stop"
renderButton _ = "Reset"

-------------------------------------------------------------------------------
-- The application
-------------------------------------------------------------------------------

main :: Effect Unit
main = launchAff_ $ launchRaff_ app

app :: forall t a. Raff t (Event t a)
app = do
  -- mfix allows us to "forward declare" an event before it is defined.
  -- `click` is an event that fires when the button is clicked.
  _ <- mfix \click -> do
    -- Tag the click events with the current time
    let timedClicks = timeB <& click
    -- Accumulate the stopwatch state by cycling through the 3 states every
    -- time the button is clicked.
    bState <- accumB cycleState NotStarted $ timedClicks
    -- Render the state at the current time in a paragraph
    paragraphB $ renderTime <$> timeB <*> bState
    -- Render the button
    buttonOut <- button { text: renderButton <$> bState }
    -- Return the button's click event (will initialize the click event we
    -- forward declared at the top of the `mfix` block.
    pure buttonOut.click
  -- Return an event that will terminate the application (this event never
  -- fires).
  pure empty

-------------------------------------------------------------------------------
-- Some helpers
-------------------------------------------------------------------------------

fromEventListener
  :: forall m t a
   . MonadRaff t m
  => EventType
  -> EventTarget
  -> (Web.Event -> a)
  -> m (Event t a)
fromEventListener etype target f = makeEvent \fire -> do
  listener <- eventListener $ fire <<< f
  addEventListener etype listener false target
  pure do
    removeEventListener etype listener false target

type ButtonInputs t =
  { text :: Behaviour t String
  }

type ButtonOutputs t =
  { click :: Event t Unit
  }

button
  :: forall t m
   . MonadEffect m
  => MonadRaff t m
  => ButtonInputs t
  -> m (ButtonOutputs t)
button { text } = do
  el <- liftEffect $ mkElement "button"
  click <- fromEventListener click (Element.toEventTarget el) $ const unit
  let
    setup = do
      appendElement el
    teardown _ = do
      removeElement el
  animateWithSetup text setup teardown \text' _ -> do
    setTextContent text' $ E.toNode el
  pure { click }

paragraphB
  :: forall t. Behaviour t String -> Raff t Unit
paragraphB b = animateWithSetup b (appendNewElement "p") removeElement \s p ->
  setTextContent s $ E.toNode p

mkElement :: String -> Effect Element
mkElement tag = do
  w <- window
  d <- document w
  createElement tag $ toDocument d

appendElement :: Element -> Effect Unit
appendElement el = do
  w <- window
  d <- document w
  mb <- body d
  case mb of
    Nothing -> throw "no body"
    Just bd -> do
      appendChild (E.toNode el) (HE.toNode bd)

appendNewElement :: String -> Effect Element
appendNewElement tag = do
  w <- window
  d <- document w
  mb <- body d
  case mb of
    Nothing -> throw "no body"
    Just bd -> do
      el <- createElement tag $ toDocument d
      appendChild (E.toNode el) (HE.toNode bd)
      pure el

removeElement :: Element -> Effect Unit
removeElement = remove <<< E.toChildNode
