{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Fetch.Pull (
  fetchPulls
  , fetchPullComments
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Fetch.Core
import Sauron.Fetch.Issue (fetchIssueCommentsAndEvents)
import Sauron.Types

fetchPulls :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedPullsT -> m ()
fetchPulls owner name (PaginatedPullsNode (EntityData {..})) = do
  extraTerms <- readTVarIO _search >>= \case
    SearchNone -> pure []
    SearchText t -> pure $ T.words t
  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)

  bc <- ask

  fetchPaginated'' (searchIssuesR fullQuery) _pageInfo _state $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (sr@(SearchResult _totalCount results), newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched sr)
      (writeTVar _children =<<) $ forM (V.toList results) $ \issue@(Issue {..}) ->
        SinglePullNode <$> makeEmptyElem bc issue ("/pull/" <> show issueNumber) (_depth + 1)

fetchPullComments :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable (V.Vector (Either IssueEvent IssueComment))) -> m ()
fetchPullComments owner name issueNumber inner = do
  ctx <- ask
  bracketOnError_ (atomically $ markFetching inner)
                  (atomically $ writeTVar inner (Errored "Pull comments and events fetch failed with exception.")) $
    -- pullRequestCommentsR returns comments on the "unified diff"
    -- there are also "commit comments" and "issue comments".
    -- The last one are the most common on PRs, so we use commentsR
    liftIO (fetchIssueCommentsAndEvents ctx owner name (unIssueNumber issueNumber)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right merged -> atomically $ writeTVar inner (Fetched merged)
