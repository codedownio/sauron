{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Pull (
  pullLine
  , pullInner
  ) where

import Brick
import Control.Monad
import Data.String.Interpolate
import Data.Time
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue (issueInner, renderTimelineItem)
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff


instance ListDrawable Fixed 'SinglePullT where
  drawLine appState (EntityData {_static=issue, ..}) =
    pullLine (_appNow appState) _toggled issue (_appAnimationCounter appState) _state

  drawInner appState (EntityData {_static=issue, _state, _ident, ..}) = do
    guard _toggled
    guardFetchedOrHasPrevious _state $ \comments ->
      return $ issueInner (_appNow appState) issue comments

pullLine :: UTCTime -> Bool -> Issue -> Int -> Fetchable a -> Widget n
pullLine now toggled' (Issue {issueNumber=(IssueNumber number), ..}) animationCounter fetchableState = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString issueTitle
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , padLeft Max $ str "" -- (if pullComments > 0 then [i|🗨  #{pullComments}|] else "")
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      withAttr hashAttr $ str "#"
      , withAttr hashNumberAttr $ str $ show number
      , str [i| opened #{timeFromNow (diffUTCTime now issueCreatedAt)} by |]
      , withAttr usernameAttr $ str $ [i|#{untagName $ simpleUserLogin issueUser}|]
      ]

pullInner :: UTCTime -> Issue -> Text -> NodeState 'SinglePullT -> Widget n
pullInner now (Issue {..}) body inner =
  allItems
  & zip [0..]
  & fmap (uncurry (renderTimelineItem now (length allItems)))
  & (++ statusMessages)
  & vBox
  where
    SimpleUser {simpleUserLogin=(N openerUsername)} = issueUser

    (commentsAndEvents, statusMessages) = case inner of
      Fetched cs -> (V.toList cs, [])
      Fetching maybeCs -> case maybeCs of
        Just cs -> (V.toList cs, [strWrap [i|Refreshing comments...|]])
        Nothing -> ([], [strWrap [i|Fetching comments...|]])
      Errored err -> ([], [strWrap [i|Failed to fetch comments: #{err}|]])
      NotFetched -> ([], [strWrap [i|Comments not fetched.|]])

    -- All timeline items including the PR description
    allItems = (Left (openerUsername, body, issueCreatedAt), "")
             : fmap (\item -> (Right item, "")) commentsAndEvents
