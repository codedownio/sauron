{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.UI.Pull (
  pullLine
  , pullInner
  ) where

import Brick
import Data.String.Interpolate
import Data.Time
import GitHub
import GitHub.Data.Name
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue (renderTimelineItem)
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.Util.TimeDiff


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

pullInner :: UTCTime -> Issue -> Text -> Fetchable (NodeState 'SinglePullT) -> Widget n
pullInner now (Issue {..}) body inner =
  allItems
  & zip [0..]
  & fmap (uncurry (renderTimelineItem now (length allItems)))
  & (++ statusMessages)
  & vBox
  where
    SimpleUser {simpleUserLogin=(N openerUsername)} = issueUser

    (commentsAndEvents, statusMessages) = case inner of
      Fetched cs -> (toList cs, [])
      Fetching maybeCs -> case maybeCs of
        Just cs -> (toList cs, [strWrap [i|Refreshing comments...|]])
        Nothing -> ([], [strWrap [i|Fetching comments...|]])
      Errored err -> ([], [strWrap [i|Failed to fetch comments: #{err}|]])
      NotFetched -> ([], [strWrap [i|Comments not fetched.|]])

    -- All timeline items including the PR description
    allItems = (Left (openerUsername, body, issueCreatedAt), "")
             : fmap (\item -> (Right item, "")) commentsAndEvents
