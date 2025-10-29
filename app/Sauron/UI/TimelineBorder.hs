module Sauron.UI.TimelineBorder (
  firstTimelineBorder,
  middleTimelineBorder,
  lastTimelineBorder
) where

import Brick
import Brick.Widgets.Border
import Relude
import Sauron.UI.AttrMap (timelineBorderAttr)

firstTimelineBorder :: Widget n -> Widget n -> Widget n
firstTimelineBorder label content =
  vBox [
    hBox [
      withAttr timelineBorderAttr $ str "┌───"
      , withAttr timelineBorderAttr $ str "┴"
      , withAttr timelineBorderAttr $ str "─"
      , label
      , withAttr timelineBorderAttr hBorder
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
      , withAttr timelineBorderAttr hBorder
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
      , withAttr timelineBorderAttr hBorder
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
      , withAttr timelineBorderAttr hBorder
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
      , withAttr timelineBorderAttr hBorder
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
      , withAttr timelineBorderAttr hBorder
      , withAttr timelineBorderAttr $ str "┘"
    ]
  ]
