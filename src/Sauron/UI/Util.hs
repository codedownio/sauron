{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.UI.Util where

import Brick
import Graphics.Vty.Image
import Lens.Micro
import Relude


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
