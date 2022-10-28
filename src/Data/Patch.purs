module Data.Patch
  ( class Patch
  , PatchMap(..)
  , PatchOp(..)
  , applyPatch
  , applyPatchAlways
  , copy
  , copyAndPatch
  , delete
  , insert
  , move
  , moveAndPatch
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

class Patch patch a | a -> patch where
  applyPatch :: patch -> a -> Maybe a

applyPatchAlways :: forall a patch. Patch patch a => patch -> a -> a
applyPatchAlways p a = fromMaybe a $ applyPatch p a

-------------------------------------------------------------------------------
-- Identity
-------------------------------------------------------------------------------

instance Patch a (Identity a) where
  applyPatch a _ = Just $ Identity a

-------------------------------------------------------------------------------
-- Proxy
-------------------------------------------------------------------------------

instance Patch a (Proxy a) where
  applyPatch _ _ = Nothing

-------------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------------

newtype PatchMap k v p = PatchMap (Map k (PatchOp k v p))

data PatchOp k v p
  = Insert v
  | Delete
  | Move k p

derive instance Generic (PatchOp k v p) _
derive instance (Eq k, Eq v, Eq p) => Eq (PatchOp k v p)
derive instance (Ord k, Ord v, Ord p) => Ord (PatchOp k v p)
derive instance Functor (PatchOp k v)
instance Bifunctor (PatchOp k) where
  bimap f g = case _ of
    Insert v -> Insert $ f v
    Delete -> Delete
    Move k p -> Move k $ g p

instance Semigroup p => Semigroup (PatchOp k v p) where
  append _ b = b

instance (Ord k, Semigroup p) => Semigroup (PatchMap k v p) where
  append = coerce
    ( append
        :: SemigroupMap k (PatchOp k v p)
        -> SemigroupMap k (PatchOp k v p)
        -> SemigroupMap k (PatchOp k v p)
    )

instance (Show k, Show v, Show p) => Show (PatchOp k v p) where
  show = genericShow

instance (Ord k, Patch p v) => Patch (PatchMap k v p) (Map k v) where
  applyPatch (PatchMap ops) m =
    if Map.isEmpty insertions && Map.isEmpty deletions then
      Nothing
    else
      Just $ Map.union insertions (Map.difference m deletions)
    where
    insertions = ops # Map.mapMaybeWithKey \_ -> case _ of
      Insert v -> Just v
      Delete -> Nothing
      Move k p -> applyPatchAlways p <$> Map.lookup k m
    deletions = ops # Map.mapMaybeWithKey \k -> case _ of
      Delete -> unit <$ guard (Map.member k m)
      _ -> Nothing

insert :: forall k v p. Ord k => k -> v -> PatchMap k v p -> PatchMap k v p
insert k v (PatchMap ops) = PatchMap $ Map.insert k (Insert v) ops

delete :: forall k v p. Ord k => k -> PatchMap k v p -> PatchMap k v p
delete k (PatchMap ops) = PatchMap $ Map.insert k Delete ops

move
  :: forall k v
   . Ord k
  => k
  -> k
  -> PatchMap k v (Proxy v)
  -> PatchMap k v (Proxy v)
move from to (PatchMap ops) = PatchMap
  $ Map.insert from Delete
  $ Map.insert to (Move from Proxy) ops

moveAndPatch
  :: forall k v p
   . Ord k
  => k
  -> k
  -> p
  -> PatchMap k v p
  -> PatchMap k v p
moveAndPatch from to patch (PatchMap ops) = PatchMap
  $ Map.insert from Delete
  $ Map.insert to (Move from patch) ops

copy
  :: forall k v
   . Ord k
  => k
  -> k
  -> PatchMap k v (Proxy v)
  -> PatchMap k v (Proxy v)
copy from to (PatchMap ops) = PatchMap $ Map.insert to (Move from Proxy) ops

copyAndPatch
  :: forall k v p
   . Ord k
  => k
  -> k
  -> p
  -> PatchMap k v p
  -> PatchMap k v p
copyAndPatch from to patch (PatchMap ops) = PatchMap
  $ Map.insert to (Move from patch) ops
