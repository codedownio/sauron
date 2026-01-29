{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Pull (
  pullLine
  , pullInner
  ) where

import Brick
import Brick.Forms
import Control.Monad
import Data.String.Interpolate
import Data.Time
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name
import Lens.Micro
import Relude
import Sauron.Actions (refreshOnZoom)
import Sauron.Actions.Util (findRepoParent, findPullsParent)
import Sauron.Event.CommentModal (fetchCommentsAndOpenModal)
import Sauron.Event.Helpers (withFixedElemAndParents)
import Sauron.Event.Search (ensureNonEmptySearch)
import Sauron.Fetch.Pull (fetchPullComments)
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue (issueInner, renderTimelineItem, closeReopenAndRefresh)
import Sauron.UI.Keys
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff
import UnliftIO.Async (async)


instance ListDrawable Fixed 'SinglePullT where
  drawLine appState (EntityData {_static=issue, ..}) =
    pullLine (_appNow appState) _toggled issue (_appAnimationCounter appState) _state

  drawInner appState (EntityData {_static=issue, _state, _ident, ..}) = do
    guard _toggled
    guardFetchedOrHasPrevious _state $ \comments ->
      return $ issueInner (_appNow appState) issue comments

  getExtraTopBoxWidgets _app (EntityData {_static=issue}) =
    [hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey editSearchKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "Search"
          ]
    , hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey zoomModalKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "Zoom"
          ]
    , hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey commentKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "Comment"
          ]
    , hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey closeReopenKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str (if issueState issue == StateOpen then "Close" else "Reopen")
          ]
    ]

  handleHotkey s key (EntityData {_static=issue})
    | key == editSearchKey = do
        withFixedElemAndParents s $ \_ _ parents -> do
          case findPullsParent parents of
            Just pullsNode@(PaginatedPullsNode ed) -> do
              searchText <- liftIO $ atomically $ ensureNonEmptySearch pullsNode
              modify (appForm ?~ (newForm [editTextField id TextForm (Just 1)] searchText, _ident ed))
            _ -> return ()
        return True
    | key == zoomModalKey = do
        withFixedElemAndParents s $ \(SomeNode _) (SomeNode variableEl) parents -> do
          refreshOnZoom (s ^. appBaseContext) variableEl parents
          liftIO $ atomically $ writeTVar (_appModalVariable s) (Just (ZoomModalState (SomeNode variableEl)))
        return True
    | key == commentKey = do
        withFixedElemAndParents s $ \_ _ parents -> do
          case findRepoParent parents of
            Just (RepoNode (EntityData {_static=(owner, name)})) ->
              fetchCommentsAndOpenModal (s ^. appBaseContext) issue True owner name
            _ -> return ()
        return True
    | key == closeReopenKey = do
        liftIO $ void $ async $ do
          withFixedElemAndParents s $ \_ _ parents ->
            whenJust (findRepoParent parents) $ \(RepoNode (EntityData {_static=(owner, name)})) ->
              whenJust (findPullsParent parents) $ \(PaginatedPullsNode ed) ->
                closeReopenAndRefresh (s ^. appBaseContext) owner name issue (_children ed)
                  (\(SinglePullNode e) -> (_static e, _state e))
                  (\(SinglePullNode e) iss -> SinglePullNode (e { _static = iss }))
                  fetchPullComments
        return True

  handleHotkey _ _ _ = return False

pullLine :: UTCTime -> Bool -> Issue -> Int -> Fetchable a -> Widget n
pullLine now toggled' (Issue {issueNumber=(IssueNumber number), ..}) animationCounter fetchableState = vBox [line1, line2]
  where
    markerAttr = if issueState == StateOpen then openStateMarkerAttr else closedStateMarkerAttr
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr markerAttr $ str (if issueState == StateOpen then "⊙  " else "☑  ")
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
