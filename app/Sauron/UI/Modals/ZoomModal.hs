module Sauron.UI.Modals.ZoomModal (
  renderZoomModal,
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Relude
import Sauron.Types
import Sauron.UI
import Sauron.UI.AttrMap

renderZoomModal :: AppState -> ModalState -> Widget ClickableName
renderZoomModal _appState (ZoomModalState {_zoomModalSomeNode=someNode, _zoomModalAppState=modalAppState}) =
  vBox [
    hCenter $ withAttr boldText $ str modalTitle
    , hBorder
    -- Scrollable content area with node content
    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport ZoomModalContent Vertical $
      vBox [
        renderNodeContent modalAppState someNode
      ]
    , hBorder
    , hCenter $ withAttr hotkeyMessageAttr $ str "Press [Esc] to close"
  ]
  & border
  & withAttr normalAttr
  & vLimitPercent 90
  & hLimitPercent 90
  & centerLayer
  where
    modalTitle = "Node Contents"
renderZoomModal _ _ = str "Invalid modal state" -- This should never happen

renderNodeContent :: AppState -> SomeNode Fixed -> Widget ClickableName
renderNodeContent appState someNode = listDrawElement' appState someNode
