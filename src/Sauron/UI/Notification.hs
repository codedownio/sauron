module Sauron.UI.Notification (
  notificationLine
  , notificationInner
  ) where

import Brick
import GitHub
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)

notificationLine :: Bool -> Notification -> Int -> Fetchable a -> Widget n
notificationLine toggled _notification animationCounter fetchableState = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str "Notification" -- We'll expand this once we know the fields
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , padLeft Max $ str "GitHub"
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      withAttr boldText $ str "GitHub notification"
      , str " - Details to be filled"
      ]

notificationInner :: Notification -> Widget n
notificationInner _notification = vBox notificationDetails
  where
    notificationDetails = [
      withAttr normalAttr $ str "Notification details:"
      , padTop (Pad 1) $ str "More details to be added once we understand the Notification type structure"
      ]