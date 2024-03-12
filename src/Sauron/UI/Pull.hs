{-# LANGUAGE ViewPatterns #-}

module Sauron.UI.Pull (
  pullLine
  , pullInner
  ) where

import Brick
import Brick.Widgets.Border
import Data.String.Interpolate
import Data.Time
import GitHub
import GitHub.Data.Name
import Relude
import Sauron.Types hiding (toggled)
import Sauron.UI.AttrMap
import Sauron.UI.Issue (maxCommentWidth)
import Sauron.UI.Markdown
import Sauron.UI.Util.TimeFromNow


pullLine :: UTCTime -> Bool -> SimplePullRequest -> Widget n
pullLine now toggled (SimplePullRequest {simplePullRequestNumber=(IssueNumber number), ..}) = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString simplePullRequestTitle
      , padLeft Max $ str "" -- (if pullComments > 0 then [i|🗨  #{pullComments}|] else "")
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      withAttr hashAttr $ str "#"
      , withAttr hashNumberAttr $ str $ show number
      , str [i| opened #{timeFromNow (diffUTCTime now simplePullRequestCreatedAt)} by |]
      , withAttr usernameAttr $ str $ [i|#{untagName $ simpleUserLogin simplePullRequestUser}|]
      ]

pullInner :: UTCTime -> SimplePullRequest -> Text -> Fetchable PaginatedItemInner -> Widget n
pullInner now (SimplePullRequest {..}) body inner = vBox (firstCell : comments)
  where
    SimpleUser {simpleUserLogin=(N openerUsername)} = simplePullRequestUser

    firstCell = hLimit maxCommentWidth $ borderWithLabel
      (str [i|#{openerUsername} opened #{timeFromNow (diffUTCTime now simplePullRequestCreatedAt)}|]
          & padLeftRight 1
      )
      (markdownToWidgets body)

    comments :: [Widget n]
    comments = case inner of
      Fetched (PaginatedItemInnerPull cs) -> fmap renderComment (toList cs)
      Fetched x -> [strWrap [i|Unexpected comments: #{x}|]]
      Fetching {} -> [strWrap [i|Fetching comments...|]]
      Errored err -> [strWrap [i|Failed to fetch comments: #{err}|]]
      NotFetched -> [strWrap [i|Comments not fetched.|]]

    -- TODO: use pullCommentUpdatedAt
    renderComment (IssueComment {issueCommentUser=(SimpleUser {simpleUserLogin=(N username)}), ..}) = hLimit maxCommentWidth $ borderWithLabel
      (str [i|#{username} commented #{timeFromNow (diffUTCTime now issueCommentCreatedAt)}|]
          & padLeftRight 1
      )
      (markdownToWidgets issueCommentBody)
