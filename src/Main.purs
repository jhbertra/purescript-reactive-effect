module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Align (aligned)
import Data.Filterable (filter)
import Data.Int (odd)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Reactive
  ( Event
  , Raff
  , bracketReact
  , makeEvent
  , react
  , runRaff_
  , stepper
  )
import Effect.Ref as Ref
import Web.DOM.ChildNode (remove)
import Web.DOM.Document (createElement)
import Web.DOM.Element as E
import Web.DOM.Node (appendChild, setTextContent)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as HE
import Web.HTML.Window (document)

main :: Effect Unit
main = launchAff_ $ runRaff_ do
  e <- makeEvent \fire -> do
    countRef <- Ref.new 0
    fiber <- launchAff $ forever do
      delay $ Milliseconds 1000.0
      liftEffect do
        count <- Ref.read countRef
        Ref.write (count + 1) countRef
        fire count
    pure $ launchAff_ $ killFiber (error "killed") fiber
  let e1 = filter (eq 0 <<< (_ `mod` 3)) e
  let e2 = filter odd e
  let e3 = aligned e1 e2
  let e4 = aligned e1 e3
  _ <- stepper (-1) e
  _ <- writeToConsole "e3" e3
  _ <- writeToConsole "e4" e4
  bracketReact e (mkElement "p") removeElement \p a -> do
    setTextContent (show a) (E.toNode p)
  where
  writeToConsole
    :: forall t a. Show a => String -> Event t a -> Raff t (Effect Unit)
  writeToConsole prefix e =
    react e $ Console.log <<< append prefix <<< append ": " <<< show
  mkElement tag = do
    w <- window
    d <- document w
    mb <- body d
    case mb of
      Nothing -> throw "no body"
      Just bd -> do
        p <- createElement tag $ toDocument d
        appendChild (E.toNode p) (HE.toNode bd)
        pure p
  removeElement = remove <<< E.toChildNode
