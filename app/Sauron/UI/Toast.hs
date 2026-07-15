module Sauron.UI.Toast (
  toastDurationTicks
  , showToast
  , showToastWidget
  , renderToasts
  ) where

import Brick
import Brick.Widgets.Border (border, borderAttr)
import Brick.Widgets.Border.Style (unicodeBold)
import qualified Data.Text as T
import Graphics.Vty (imageHeight, imageWidth, translateX, translateY)
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap (toastBorderAttr)


-- | How long a toast stays up, in animation ticks (200ms each). ~4 seconds.
toastDurationTicks :: Int
toastDurationTicks = 20

-- | Raise a plain-text corner toast from an 'EventM' handler. From background
-- threads, write a 'ToastFired' to the event channel instead.
showToast :: ToastLevel -> Text -> EventM n AppState ()
showToast level = showToastWidget level . txt

-- | Raise a corner toast whose body is an arbitrary widget (for styled content).
-- From background threads, write a 'ToastWidgetFired' to the event channel.
showToastWidget :: ToastLevel -> Widget ClickableName -> EventM n AppState ()
showToastWidget level content = do
  counter <- gets _appAnimationCounter
  modify (appToasts %~ (<> [(level, content, counter + toastDurationTicks)]))

-- | Render the active toasts stacked in the bottom-right corner: newest at the
-- bottom, older ones pushed up. As many as fit in the viewport are shown; the
-- overflow is collapsed into a single "(N older notifications)" toast on top.
renderToasts :: AppState -> Maybe (Widget ClickableName)
renderToasts app
  | null toasts = Nothing
  | otherwise = Just $ Widget Greedy Greedy $ do
      c <- getContext
      -- Toast bodies are arbitrary widgets of varying height, so measure each box
      -- (and a summary box) by rendering it rather than assuming a fixed height.
      boxHeights <- mapM (fmap (imageHeight . (^. imageL)) . render . toastBox) toasts
      summaryHeight <- (imageHeight . (^. imageL)) <$> render (summaryBox (length toasts))
      let total = length toasts
          avail = c ^. availHeightL - 1  -- leave a bottom margin
          -- Height of a stack of the given box heights, plus a summary box when
          -- some toasts are hidden, plus the 1-row spacers between boxes.
          stackHeight hs hasOverflow =
            sum hs + (if hasOverflow then summaryHeight else 0)
                   + max 0 (length hs + (if hasOverflow then 1 else 0) - 1)
          -- Take as many of the newest toasts (from the end) as fit.
          fits k = stackHeight (drop (total - k) boxHeights) (k < total) <= avail
          shownCount = fromMaybe 0 $ find fits [total, total - 1 .. 0]
          hidden = total - shownCount
          shown = drop hidden toasts
          overflow = [summaryBox hidden | hidden > 0]
      render $ bottomRightLayer $ vBox $ intersperse (str " ") $
        map rightAlign (overflow <> map toastBox shown)
  where
    toasts = _appToasts app

    summaryBox n = box ToastDefault $ txt $
      "(" <> show n <> " older notification" <> (if n == 1 then "" else "s") <> ")"

    toastBox (level, content, deadline) =
      box level $ vBox [content, withAttr (toastBorderAttr level) (countdown deadline)]

    -- The level colour is used only for the border (and the countdown gauge); the
    -- interior keeps normal colours so its text stays readable.
    box level content = withBorderStyle unicodeBold $
      overrideAttr borderAttr (toastBorderAttr level) $ border $ padLeftRight 1 content

    -- A bar that drains from full to empty as the toast's time runs out. Uses short
    -- lower-blocks so it sits at the bottom of its row, giving a little space below the
    -- text above it.
    countdown deadline =
      txt $ T.replicate filled "▄" <> T.replicate (segments - filled) " "
      where
        remaining = max 0 (deadline - _appAnimationCounter app)
        filled = max 0 $ min segments $
          round (fromIntegral remaining * fromIntegral segments / fromIntegral toastDurationTicks :: Double)

    segments = 12 :: Int

-- | Right-align a widget within the full available width, padding the left
-- transparently so a narrow toast still hugs the right edge when a wider one is
-- stacked with it (brick's 'padLeft Max' would fill the gap opaquely instead).
rightAlign :: Widget n -> Widget n
rightAlign p =
  Widget Greedy (vSize p) $ do
    result <- render p
    c <- getContext
    let xoff = max 0 (c ^. availWidthL - imageWidth (result ^. imageL) - 1)
    return $ addResultOffset (Location (xoff, 0)) $ result & imageL %~ translateX xoff

-- | Anchor a widget to the bottom-right corner as a layer, leaving the rest
-- transparent (mirrors brick's 'centerLayer', which only offers centering).
bottomRightLayer :: Widget n -> Widget n
bottomRightLayer p =
  Widget Greedy Greedy $ do
    result <- render p
    c <- getContext
    let img = result ^. imageL
        xoff = max 0 (c ^. availWidthL - imageWidth img - 1)
        yoff = max 0 (c ^. availHeightL - imageHeight img - 1)
        off = Location (xoff, yoff)
    return $ addResultOffset off $ result & imageL %~ (translateX xoff . translateY yoff)
