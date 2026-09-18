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
                   withVScrollBars OnRight $ withVScrollBarHandles $
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

isFetching :: Fetchable a -> Bool
isFetching (Fetching _) = True
isFetching _ = False

isFetched :: Fetchable a -> Bool
isFetched (Fetched _) = True
isFetched _ = False

isFetchedOrHasPrevious :: Fetchable a -> Bool
isFetchedOrHasPrevious = isJust . fetchableCurrent

isFetchingOrFetched :: Fetchable a -> Bool
isFetchingOrFetched (Fetched _) = True
isFetchingOrFetched (Fetching _) = True
isFetchingOrFetched _ = False


-- | Draw one of the two-line node rows, with @trailer@ (a time-ago widget) floating at the
-- right edge of the title line when it fits there alongside @titleRight@, and dropping down
-- to the right edge of the detail line when the title is too long to share its line.
twoLineNodeWithTrailer :: Widget n -> Widget n -> Widget n -> Widget n -> Widget n
twoLineNodeWithTrailer title titleRight details trailer = Widget Greedy Fixed $ do
  ctx <- getContext

  titleResult <- render title
  titleRightResult <- render titleRight
  trailerResult <- render trailer

  let widthOf = imageWidth . image
      -- Reuse the measured images rather than rendering the widgets a second time
      reuse result = Widget Fixed Fixed (return result)

      -- No separator when there's nothing on the right of the title line to separate from
      innerGap = if widthOf titleRightResult == 0 then 0 else 2
      rightGroupWidth = widthOf titleRightResult + innerGap + widthOf trailerResult
      fitsOnTitleLine = widthOf titleResult + 2 + rightGroupWidth <= ctx ^. availWidthL

      rightGroup = hBox [reuse titleRightResult, padLeft (Pad innerGap) (reuse trailerResult)]

  render $ vBox [
    hBox [reuse titleResult, padLeft Max (if fitsOnTitleLine then rightGroup else reuse titleRightResult)]
    , if fitsOnTitleLine
        then padRight Max details
        else hBox [details, padLeft Max (reuse trailerResult)]
    ]
