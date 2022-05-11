module Data.Patch where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))

class Patch patch a | a -> patch where
  applyPatch :: patch -> a -> Maybe a

class Patch patch a <= Diff patch a | a -> patch where
  diff :: a -> a -> Maybe patch

instance Patch a (Identity a) where
  applyPatch a _ = Just $ Identity a

instance Diff a (Identity a) where
  diff (Identity a) _ = Just a
