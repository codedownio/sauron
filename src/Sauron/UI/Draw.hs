
module Sauron.UI.Draw (
  drawUI
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import Control.Monad
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub hiding (Status)
import Lens.Micro hiding (ix)
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Draw.Border
import Sauron.UI.Draw.BottomBar
import Sauron.UI.Draw.Repo
import Sauron.UI.Draw.Util
import Sauron.UI.Draw.WorkflowLine
import Sauron.UI.TopBox
import Sauron.UI.Util


drawUI :: AppState -> [Widget ClickableName]
drawUI app = [ui]
  where
    ui = vBox [
      topBox app
      , borderWithCounts app
      , mainList app
      , clickable InfoBar $ bottomBar app
      -- , do
      --     guarding (isJust (app ^. (listSelectedElement (app ^. appMainList))))
      --       (clickable InfoBar $ infoBar app)
      ]

mainList :: AppState -> Widget ClickableName
mainList app = hCenter $ padAll 1 $ L.renderListWithIndex listDrawElement True (app ^. appMainList)
  where
    listDrawElement :: Int -> Bool -> MainListElem -> Widget ClickableName
    listDrawElement ix isSelected x@(MainListElemHeading {..}) = render ix isSelected x [
      Just $ hBox $ catMaybes [
        Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
        , Just (hBox [str (toString _label)])
        , Just (padLeft Max (str " "))
        ]
      ]
    listDrawElement ix isSelected x@(MainListElemRepo {..}) = render ix isSelected x [
      Just $ renderRepoLine _toggled _namespaceName _repo _healthCheck
      ]
    listDrawElement ix isSelected x@(MainListElemIssues {..}) = render ix isSelected x [
      Just $ hBox $ catMaybes [
        Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
        , Just (str "Issues ")
        , Just $ case _issues of
            NotFetched -> str "(Not fetched)"
            Fetching -> str "(Fetching)"
            Errored msg -> str [i|Errored: #{msg}|]
            Fetched xs -> str [i|(#{V.length xs})|]
        , Just (padLeft Max (str " "))
        ]
      ]
    listDrawElement ix isSelected x@(MainListElemIssue {..}) = render ix isSelected x [
      Just $ case _issue of
        Fetched (Issue {issueNumber=(IssueNumber number), ..}) -> hBox [
          withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
          , str ("#" <> show number <> " ")
          , withAttr normalAttr $ str $ toString issueTitle
          , padLeft Max (str [i|#{issueCreatedAt} by #{untagName $ simpleUserLogin issueUser}, #{issueComments}|])
          ]
        _ -> str ""
      , do
          guard _toggled
          guardFetched _issue $ \(Issue {..}) -> guardJust issueBody $ \body ->
            return $ padLeft (Pad 4) $
              fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
                vBox [strWrap (toString body)]
      ]
    listDrawElement ix isSelected x@(MainListElemWorkflows {..}) = render ix isSelected x [
      Just $ hBox $ catMaybes [
        Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
        , Just (str "Workflows ")
        , Just $ case _workflows of
            NotFetched -> str "(Not fetched)"
            Fetching -> str "(Fetching)"
            Errored msg -> str [i|Errored: #{msg}|]
            Fetched xs -> str [i|(#{withTotalCountTotalCount xs})|]
        , Just (padLeft Max (str " "))
        ]
      ]
    listDrawElement ix isSelected x@(MainListElemWorkflow {..}) = render ix isSelected x [
      Just $ case _workflow of
        Fetched wf -> workflowWidget wf
        _ -> str ""
      ]

    render ix isSelected x = clickable (ListRow ix) . padLeft (Pad (4 * (_depth x))) . (if isSelected then border else id) . vBox . catMaybes


progressThing :: String
progressThing = "⠀"
