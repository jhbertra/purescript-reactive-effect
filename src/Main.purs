module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Apply (lift2)
import Data.Align (aligned)
import Data.Filterable (filter)
import Data.Int (odd)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Effect.Reactive
  ( Event
  , Raff
  , accumE
  , asap
  , indexed_
  , intervalEvent
  , launchRaff_
  , liftSample2
  , performWithSetup
  , stepper
  , (<&)
  )
import Web.DOM.ChildNode (remove)
import Web.DOM.Document (createElement)
import Web.DOM.Element as E
import Web.DOM.Node (appendChild, setTextContent)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as HE
import Web.HTML.Window (document)

main :: Effect Unit
main = launchAff_ $ launchRaff_ do
  e <- indexed_ =<< (lift2 (<|>) asap $ intervalEvent $ Seconds 1.0)
  eseconds <- indexed_ =<< (lift2 (<|>) asap $ intervalEvent $ Seconds 1.0)
  let e1 = filter (eq 0 <<< (_ `mod` 3)) e
  let e2 = filter odd e
  let e3 = aligned e1 e2
  let e4 = aligned e1 e3
  _bseconds <- add one <$> stepper (-1.0) eseconds
  b <- stepper (-1) e
  ereturn <- paragraph "e" e
  _ <- paragraph "b <& e" $ b <& e
  _ <- paragraph "b + e" $ liftSample2 (+) b e
  _ <- paragraph "accumE (+) 0 e" =<< accumE (+) 0 e
  _ <- paragraph "e1" e1
  _ <- paragraph "e2" e2
  _ <- paragraph "e3" e3
  _ <- paragraph "e4" e4
  _ <- paragraph "ereturn" ereturn
  pure empty
  where
  paragraph
    :: forall t a. Show a => String -> Event t a -> Raff t (Event t String)
  paragraph name = performWithSetup (mkElement "p") removeElement <<<
    map
      \a p ->
        do
          setTextContent (name <> ": " <> show a) (E.toNode p)
          pure $ show a
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
