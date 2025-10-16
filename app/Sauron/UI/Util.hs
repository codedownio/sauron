{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.UI.Util where

import Brick
import Graphics.Vty.Image
import Lens.Micro
import Relude
import Sauron.Types


fixedHeightOrViewportPercent :: (Ord n, Show n) => n -> Int -> Widget n -> Widget n
fixedHeightOrViewportPercent vpName maxHeightPercent w =
  Widget Fixed Fixed $ do
    -- Render the viewport contents in advance
    result <- render w
    -- If the contents will fit in the maximum allowed rows,
    -- just return the content without putting it in a viewport.

    ctx <- getContext

    let usableHeight = ctx ^. windowHeightL

    let maxHeight = round (toRational usableHeight * (toRational maxHeightPercent / 100))

    if imageHeight (image result) <= maxHeight
      then return result
      -- Otherwise put the contents (pre-rendered) in a viewport
      -- and limit the height to the maximum allowable height.
      else render (vLimit maxHeight $
                   viewport vpName Vertical $
                   Widget Fixed Fixed $ return result)



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

guardFetchedOrHasPrevious :: (Monad m, Alternative m) => Fetchable a -> (a -> m b) -> m b
guardFetchedOrHasPrevious fetchable fn = do
  guard (isFetchedOrHasPrevious fetchable)
  case fetchableCurrent fetchable of
    Just x -> fn x
    _ -> error "impossible"

isFetched :: Fetchable a -> Bool
isFetched (Fetched _) = True
isFetched _ = False

isFetchedOrHasPrevious :: Fetchable a -> Bool
isFetchedOrHasPrevious = isJust . fetchableCurrent
