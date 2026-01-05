{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Fetch.Issue (
  fetchIssues
  , fetchIssue
  , fetchIssueComments
  , fetchIssueCommentsAndEvents
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util
import Sauron.Fetch.Core
import Sauron.Types
import UnliftIO.Async

fetchIssues :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedIssuesT -> m ()
fetchIssues owner name (PaginatedIssuesNode (EntityData {..})) = do
  (search, _pageInfo, _fetchable) <- readTVarIO _state
  extraTerms <- case search of
    SearchNone -> pure []
    SearchText t -> pure $ T.words t
  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)

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
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable (V.Vector (Either IssueEvent IssueComment))) -> m ()
fetchIssueComments owner name issueNumber inner = do
  ctx <- ask
  bracketOnError_ (atomically $ markFetching inner)
                  (atomically $ writeTVar inner (Errored "Issue comments and events fetch failed with exception.")) $
    liftIO (fetchIssueCommentsAndEvents ctx owner name (unIssueNumber issueNumber)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right merged -> atomically $ writeTVar inner (Fetched merged)

fetchIssueCommentsAndEvents :: (HasCallStack) => BaseContext -> Name Owner -> Name Repo -> Int -> IO (Either Error (V.Vector (Either IssueEvent IssueComment)))
fetchIssueCommentsAndEvents baseContext owner name issueNumber = do
  let fetchComments =
        withGithubApiSemaphore' (requestSemaphore baseContext)
          (githubWithLogging' baseContext
            (commentsR owner name (IssueNumber issueNumber) FetchAll))

  let fetchEvents =
        withGithubApiSemaphore' (requestSemaphore baseContext)
          (githubWithLogging' baseContext
            (eventsForIssueR owner name (GitHub.mkId (Proxy :: Proxy Issue) issueNumber) FetchAll))

  (commentsResult, eventsResult) <- concurrently fetchComments fetchEvents

  case (commentsResult, eventsResult) of
    (Right comments, Right events) -> do
      return $ Right $ mergeCommentsAndEvents comments events
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err
  where
    mergeCommentsAndEvents :: V.Vector IssueComment -> V.Vector IssueEvent -> V.Vector (Either IssueEvent IssueComment)
    mergeCommentsAndEvents comments events = V.toList commentEntries <> V.toList eventEntries
                                           & sortOn fst -- Sort by timestamp, newest first
                                           & fmap snd
                                           & V.fromList
      where
        commentEntries = fmap (\c -> (issueCommentCreatedAt c, Right c)) comments
        eventEntries = fmap (\e -> (issueEventCreatedAt e, Left e)) events
