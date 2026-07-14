module Sauron.UI.Toast (
  toastDurationTicks
  , showToast
  , renderToasts
  ) where

import Brick
import Brick.Widgets.Border (border)
import qualified Data.Text as T
import Graphics.Vty (imageHeight, imageWidth, translateX, translateY)
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap (toastAttrFor)


-- | How long a toast stays up, in animation ticks (200ms each). ~4 seconds.
toastDurationTicks :: Int
toastDurationTicks = 20

-- | Raise a corner toast from an 'EventM' handler. From background threads,
-- write a 'ToastFired' to the event channel instead.
showToast :: ToastLevel -> Text -> EventM n AppState ()
showToast level msg = do
  counter <- gets _appAnimationCounter
  modify (appToasts %~ (<> [(level, msg, counter + toastDurationTicks)]))

-- | Render the active toasts stacked in the bottom-right corner: newest at the
-- bottom, older ones pushed up. As many as fit in the viewport are shown; the
-- overflow is collapsed into a single "(N older notifications)" toast on top.
renderToasts :: AppState -> Maybe (Widget ClickableName)
renderToasts app
  | null toasts = Nothing
  | otherwise = Just $ Widget Greedy Greedy $ do
      c <- getContext
      let total = length toasts
          -- Box heights are fixed: border (2 rows) plus content lines, and a
          -- 1-row spacer sits between stacked boxes.
          toastH = 4  -- border + message + gauge
          summaryH = 3  -- border + one line
          heightFor k = k * toastH
                        + (if k < total then summaryH else 0)
                        + max 0 (k + (if k < total then 1 else 0) - 1)
          avail = c ^. availHeightL - 1  -- leave a bottom margin
          shownCount = fromMaybe 0 $ find (\k -> heightFor k <= avail) [total, total - 1 .. 0]
          hidden = total - shownCount
          shown = drop hidden toasts
          overflow = [summaryBox hidden | hidden > 0]
      render $ bottomRightLayer $ vBox $ intersperse (str " ") $
        map rightAlign (overflow <> map toastBox shown)
  where
    toasts = _appToasts app

    summaryBox n = box ToastDefault $ txt $
      "(" <> show n <> " older notification" <> (if n == 1 then "" else "s") <> ")"

    toastBox (level, msg, deadline) = box level $ vBox [txt msg, countdown deadline]

    box level content = modifyDefAttr (const (toastAttrFor level)) $ border $ padLeftRight 1 content

    -- A bar of blocks that fills up as the toast's time runs out.
    countdown deadline =
      txt $ T.replicate filled "█" <> T.replicate (segments - filled) "░"
      where
        remaining = max 0 (deadline - _appAnimationCounter app)
        elapsed = toastDurationTicks - remaining
        filled = max 0 $ min segments $
          round (fromIntegral elapsed * fromIntegral segments / fromIntegral toastDurationTicks :: Double)

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
