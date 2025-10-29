module Sauron.UI.TimelineBorder (
  firstTimelineBorder,
  middleTimelineBorder,
  lastTimelineBorder
) where

import Brick
import Relude
import Sauron.UI.AttrMap (timelineBorderAttr)


timelineHBorder :: Widget n
timelineHBorder = withAttr timelineBorderAttr $ vLimit 1 $ fill '─'

firstTimelineBorder :: Widget n -> Widget n -> Widget n
firstTimelineBorder label content =
  vBox [
    hBox [
      withAttr timelineBorderAttr $ str "┌───"
      , withAttr timelineBorderAttr $ str "┴"
      , withAttr timelineBorderAttr $ str "─"
      , label
      , withAttr timelineBorderAttr $ str "─"
      , timelineHBorder
      , withAttr timelineBorderAttr $ str "┐"
    ],
    hBox [
      withAttr timelineBorderAttr $ str "│    "
      , content
      , withAttr timelineBorderAttr $ str "│"
    ],
    hBox [
      withAttr timelineBorderAttr $ str "└───"
      , withAttr timelineBorderAttr $ str "┬"
      , timelineHBorder
      , withAttr timelineBorderAttr $ str "┘"
    ]
  ]

middleTimelineBorder :: Widget n -> Widget n -> Widget n
middleTimelineBorder label content =
  vBox [
    hBox [
      withAttr timelineBorderAttr $ str "┌───"
      , withAttr timelineBorderAttr $ str "┴"
      , withAttr timelineBorderAttr $ str "─"
      , label
      , withAttr timelineBorderAttr $ str "─"
      , timelineHBorder
      , withAttr timelineBorderAttr $ str "┐"
    ],
    hBox [
      withAttr timelineBorderAttr $ str "│    "
      , content
      , withAttr timelineBorderAttr $ str "│"
    ],
    hBox [
      withAttr timelineBorderAttr $ str "└───"
      , withAttr timelineBorderAttr $ str "┬"
      , timelineHBorder
      , withAttr timelineBorderAttr $ str "┘"
    ]
  ]

lastTimelineBorder :: Widget n -> Widget n -> Widget n
lastTimelineBorder label content =
  vBox [
    hBox [
      str "┌───"
      , withAttr timelineBorderAttr $ str "┼"
      , withAttr timelineBorderAttr $ str "─"
      , label
      , withAttr timelineBorderAttr $ str "─"
      , timelineHBorder
      , withAttr timelineBorderAttr $ str "┐"
    ],
    hBox [
      withAttr timelineBorderAttr $ str "│    "
      , content
      , withAttr timelineBorderAttr $ str "│"
    ],
    hBox [
      withAttr timelineBorderAttr $ str "└───"
      , withAttr timelineBorderAttr $ str "┴"
      , timelineHBorder
      , withAttr timelineBorderAttr $ str "┘"
    ]
  ]
