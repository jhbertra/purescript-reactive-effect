module Data.OrderedBag (OrderedBag, new, insert, delete, inOrder) where

import Prelude

import Data.Array (sortBy)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Witherable (wither)
import Effect (Effect)

foreign import data OrderedBag :: Type -> Type

foreign import _new :: forall a. Effect (OrderedBag a)
foreign import insert :: forall a. a -> OrderedBag a -> Effect Int
foreign import _delete :: forall a. a -> OrderedBag a -> Effect Unit
foreign import _get
  :: forall a
   . Maybe a
  -> (a -> Maybe a)
  -> a
  -> OrderedBag a
  -> Effect (Maybe Int)

new :: forall a. Effect (OrderedBag a)
new = _new

delete :: forall a. a -> OrderedBag a -> Effect Unit
delete = _delete

inOrder :: forall a b. OrderedBag a -> (b -> a) -> Array b -> Effect (Array b)
inOrder bag f as = do
  tagged <- wither
    ( \b -> do
        mpos <- _get Nothing Just (f b) bag
        pure $ Tuple <$> mpos <@> b
    )
    as
  pure $ snd <$> sortBy (comparing fst) tagged
