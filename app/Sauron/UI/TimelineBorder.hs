module Sauron.UI.TimelineBorder (
  firstTimelineBorder,
  middleTimelineBorder,
  lastTimelineBorder,
  standaloneTimelineBorder
) where

import Brick
import qualified Brick.BorderMap as BM
import Brick.Widgets.Border.Style (bsVertical)
import Data.IMap (Run(..))
import Graphics.Vty (imageHeight, imageWidth)
import Lens.Micro ((^.), (.~), to)
import Relude
import Sauron.UI.AttrMap (timelineBorderAttr)


timelineHBorder :: Widget n
timelineHBorder = withAttr timelineBorderAttr $ vLimit 1 $ fill '─'

firstTimelineBorder :: Widget n -> Widget n -> Widget n
firstTimelineBorder label content =
    Widget (hSize content) (vSize content) $ do
      c <- getContext

      middleResult <- render $ hLimit (c^.availWidthL - 2)
                             $ vLimit (c^.availHeightL - 2)
                             $ content

      let topBorder = hBox [
            withAttr timelineBorderAttr $ str "┌───"
            , withAttr timelineBorderAttr $ str "─"
            , withAttr timelineBorderAttr $ str "─"
            , label
            , withAttr timelineBorderAttr $ str "─"
            , timelineHBorder
            , withAttr timelineBorderAttr $ str "┐"
            ]
          bottomBorder = hBox [
            withAttr timelineBorderAttr $ str "└───"
            , withAttr timelineBorderAttr $ str "┬"
            , timelineHBorder
            , withAttr timelineBorderAttr $ str "┘"
            ]
          middle = vBorderAttr timelineBorderAttr <+> (Widget Fixed Fixed $ return middleResult) <+> vBorderAttr timelineBorderAttr
          total = topBorder <=> middle <=> bottomBorder

      render $ hLimit (middleResult^.imageL.to imageWidth + 2)
             $ vLimit (middleResult^.imageL.to imageHeight + 2)
             $ total

middleTimelineBorder :: Widget n -> Widget n -> Widget n
middleTimelineBorder label content =
    Widget (hSize content) (vSize content) $ do
      c <- getContext

      middleResult <- render $ hLimit (c^.availWidthL - 2)
                             $ vLimit (c^.availHeightL - 2)
                             $ content

      let topBorder = hBox [
            withAttr timelineBorderAttr $ str "┌───"
            , withAttr timelineBorderAttr $ str "┴"
            , withAttr timelineBorderAttr $ str "─"
            , label
            , withAttr timelineBorderAttr $ str "─"
            , timelineHBorder
            , withAttr timelineBorderAttr $ str "┐"
            ]
          bottomBorder = hBox [
            withAttr timelineBorderAttr $ str "└───"
            , withAttr timelineBorderAttr $ str "┬"
            , timelineHBorder
            , withAttr timelineBorderAttr $ str "┘"
            ]
          middle = vBorderAttr timelineBorderAttr <+> (Widget Fixed Fixed $ return middleResult) <+> vBorderAttr timelineBorderAttr
          total = topBorder <=> middle <=> bottomBorder

      render $ hLimit (middleResult^.imageL.to imageWidth + 2)
             $ vLimit (middleResult^.imageL.to imageHeight + 2)
             $ total

lastTimelineBorder :: Widget n -> Widget n -> Widget n
lastTimelineBorder label content =
    Widget (hSize content) (vSize content) $ do
      c <- getContext

      middleResult <- render $ hLimit (c^.availWidthL - 2)
                             $ vLimit (c^.availHeightL - 2)
                             $ content

      let topBorder = hBox [
            withAttr timelineBorderAttr $ str "┌───"
            , withAttr timelineBorderAttr $ str "┴"
            , withAttr timelineBorderAttr $ str "─"
            , label
            , withAttr timelineBorderAttr $ str "─"
            , timelineHBorder
            , withAttr timelineBorderAttr $ str "┐"
            ]
          bottomBorder = hBox [
            withAttr timelineBorderAttr $ str "└───"
            , withAttr timelineBorderAttr $ str "─"
            , timelineHBorder
            , withAttr timelineBorderAttr $ str "┘"
            ]
          middle = vBorderAttr timelineBorderAttr <+> (Widget Fixed Fixed $ return middleResult) <+> vBorderAttr timelineBorderAttr
          total = topBorder <=> middle <=> bottomBorder

      render $ hLimit (middleResult^.imageL.to imageWidth + 2)
             $ vLimit (middleResult^.imageL.to imageHeight + 2)
             $ total

standaloneTimelineBorder :: Widget n -> Widget n -> Widget n
standaloneTimelineBorder label content =
    Widget (hSize content) (vSize content) $ do
      c <- getContext

      middleResult <- render $ hLimit (c^.availWidthL - 2)
                             $ vLimit (c^.availHeightL - 2)
                             $ content

      let topBorder = hBox [
            withAttr timelineBorderAttr $ str "┌─── "
            , label
            , str " "
            , timelineHBorder
            , withAttr timelineBorderAttr $ str "┐"
            ]
          bottomBorder = hBox [
            withAttr timelineBorderAttr $ str "└───"
            , timelineHBorder
            , withAttr timelineBorderAttr $ str "┘"
            ]
          middle = vBorderAttr timelineBorderAttr <+> (Widget Fixed Fixed $ return middleResult) <+> vBorderAttr timelineBorderAttr
          total = topBorder <=> middle <=> bottomBorder

      render $ hLimit (middleResult^.imageL.to imageWidth + 2)
             $ vLimit (middleResult^.imageL.to imageHeight + 2)
             $ total


-- | A vertical border. Fills all vertical space. Draws using
-- 'vBorderAttr'.
vBorderAttr :: AttrName -> Widget n
vBorderAttr attr =
    withAttr attr $ Widget Fixed Greedy $ do
      ctx <- getContext
      let bs = ctxBorderStyle ctx
          h = availHeight ctx
      db <- dynBorderFromDirections (Edges True True False False)
      let dynBorders = BM.insertV mempty (Run h db)
                     $ BM.emptyCoordinates (Edges 0 (h-1) 0 0)
      setDynBorders dynBorders $ render $ withAttr attr
                               $ hLimit 1 $ fill (bsVertical bs)

-- | Initialize a 'DynBorder'. It will be 'bsDraw'n and 'bsOffer'ing
-- in the given directions to begin with, and accept join offers from
-- all directions. We consult the context to choose the 'dbStyle' and
-- 'dbAttr'.
--
-- This is likely to be useful only for custom widgets that need more
-- complicated dynamic border behavior than 'border', 'vBorder', or
-- 'hBorder' offer.
dynBorderFromDirections :: Edges Bool -> RenderM n DynBorder
dynBorderFromDirections dirs = do
    ctx <- getContext
    return DynBorder
        { dbStyle = ctxBorderStyle ctx
        , dbAttr = attrMapLookup (ctxAttrName ctx) (ctxAttrMap ctx)
        , dbSegments = (\draw -> BorderSegment True draw draw) <$> dirs
        }

-- | Replace the 'Result'\'s dynamic borders with the given one,
-- provided the context says to use dynamic borders at all.
setDynBorders :: BM.BorderMap DynBorder -> RenderM n (Result n) -> RenderM n (Result n)
setDynBorders newBorders act = do
    dyn <- ctxDynBorders <$> getContext
    res <- act
    return $ if dyn
        then res & bordersL .~ newBorders
        else res
