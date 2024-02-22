
module Sauron.UI.Draw.Util (
  guarding
  , guardJust
  , guardFetched
  ) where

import Relude
import Sauron.Types



guarding :: (Monad m, Alternative m) => Bool -> b -> m b
guarding p widget = do
  guard p
  return widget

guardJust :: (Monad m, Alternative m) => Maybe a -> (a -> m b) -> m b
guardJust val fn = do
  guard (isJust val)
  case val of
    Just x -> fn x
    _ -> error "impossible"

guardFetched :: (Monad m, Alternative m) => Fetchable a -> (a -> m b) -> m b
guardFetched fetchable fn = do
  guard (isFetched fetchable)
  case fetchable of
    Fetched x -> fn x
    _ -> error "impossible"

isFetched :: Fetchable a -> Bool
isFetched (Fetched _) = True
isFetched _ = False
