{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Fetch.Issue (
  fetchIssues
  , fetchMyIssues
  , fetchIssue
  , fetchIssueComments
  , fetchIssueCommentsByUrl
  , fetchIssueCommentsAndEvents
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import Network.URI (parseURI, uriPath)
import Relude
import Sauron.Actions.Util
import Sauron.Fetch.Core
import Sauron.Types

fetchIssues :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedIssuesT -> m ()
fetchIssues owner name (PaginatedIssuesNode (EntityData {..})) = do
  (search, _pageInfo, _fetchable) <- readTVarIO _state
  extraTerms <- case search of
    SearchNone -> pure []
    SearchText t -> pure $ T.words t
  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)
  fetchIssues' fullQuery _state _children _depth

fetchMyIssues :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Node Variable PaginatedIssuesT -> m ()
fetchMyIssues (PaginatedIssuesNode (EntityData {..})) = do
  (search, _pageInfo, _fetchable) <- readTVarIO _state
  let fullQuery = case search of
        SearchNone -> ""
        SearchText t -> T.intercalate "+" (T.words t)
  fetchIssues' fullQuery _state _children _depth

fetchIssues' :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Text -> TVar (Search, PageInfo, Fetchable TotalCount) -> TVar [Node Variable SingleIssueT] -> Int -> m ()
fetchIssues' fullQuery _state _children _depth = do
  bc <- ask

  fetchPaginatedWithState (searchIssuesR fullQuery) _state $ \case
    Left err -> do
      (s, p, _) <- readTVar _state
      writeTVar _state (s, p, Errored err)
      writeTVar _children []
    Right (SearchResult totalCount results, newPageInfo) -> do
      (s, _, _) <- readTVar _state
      writeTVar _state (s, newPageInfo, Fetched totalCount)
      (writeTVar _children =<<) $ forM (V.toList results) $ \issue@(Issue {..}) ->
        SingleIssueNode <$> makeEmptyElemWithState bc issue NotFetched ("/issue/" <> show issueNumber) (_depth + 1)

fetchIssue :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable Issue) -> m ()
fetchIssue owner name issueNumber issueVar = do
  bracketOnError_ (atomically $ markFetching issueVar)
                  (atomically $ writeTVar issueVar (Errored "Issue fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLogging (issueR owner name issueNumber)) >>= \case
      Left err -> atomically $ writeTVar issueVar (Errored (show err))
      Right x -> atomically $ writeTVar issueVar (Fetched x)

fetchIssueComments :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable (V.Vector TimelineEvent)) -> m ()
fetchIssueComments owner name issueNumber inner = do
  ctx <- ask
  bracketOnError_ (atomically $ markFetching inner)
                  (atomically $ writeTVar inner (Errored "Issue comments and events fetch failed with exception.")) $
    liftIO (fetchIssueCommentsAndEvents ctx owner name (unIssueNumber issueNumber)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right merged -> atomically $ writeTVar inner (Fetched merged)

-- | Fetch comments for an issue/PR using only its API URL (no owner/repo needed).
-- Constructs pagedQuery paths from the URL path segments.
fetchIssueCommentsByUrl :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => URL -> TVar (Fetchable (V.Vector TimelineEvent)) -> m ()
fetchIssueCommentsByUrl issueApiUrl inner = do
  let urlText = getUrl issueApiUrl
  case parseURI (toString urlText) of
    Nothing -> atomically $ writeTVar inner (Errored [i|Failed to parse issue URL: #{urlText}|])
    Just uri -> do
      let segments = filter (not . T.null) $ T.splitOn "/" $ toText (uriPath uri)
      ctx <- ask
      let timelineReq = pagedQuery (segments <> ["timeline"]) [] FetchAll
      bracketOnError_ (atomically $ markFetching inner)
                      (atomically $ writeTVar inner (Errored "Issue comments and events fetch failed with exception.")) $ do
        liftIO (withGithubApiSemaphore' (requestSemaphore ctx) (githubWithLogging' ctx timelineReq)) >>= \case
          Right timeline ->
            atomically $ writeTVar inner (Fetched timeline)
          Left err -> atomically $ writeTVar inner (Errored (show err))

fetchIssueCommentsAndEvents :: (HasCallStack) => BaseContext -> Name Owner -> Name Repo -> Int -> IO (Either Error (V.Vector TimelineEvent))
fetchIssueCommentsAndEvents baseContext owner name issueNumber =
  withGithubApiSemaphore' (requestSemaphore baseContext)
    (githubWithLogging' baseContext
      (timelineForIssueR owner name (IssueNumber issueNumber) FetchAll)) >>= \case
    Left err -> return $ Left err
    Right timeline -> return $ Right timeline
