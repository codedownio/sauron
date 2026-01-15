{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Mutations.Issue (
  submitComment
  , closeIssue
  , closeIssueWithComment
  ) where

import qualified Data.Text as T
import GitHub
import Relude
import Sauron.Actions.Util (withGithubApiSemaphore', githubWithLogging')
import Sauron.Types


submitComment :: BaseContext -> Name Owner -> Name Repo -> IssueNumber -> Text -> IO (Either Error Comment)
submitComment baseContext@(BaseContext {requestSemaphore}) owner name issueNumber commentBody = do
  withGithubApiSemaphore' requestSemaphore $
    githubWithLogging' baseContext (createCommentR owner name issueNumber commentBody)

closeIssue :: BaseContext -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error Issue)
closeIssue baseContext@(BaseContext {requestSemaphore}) owner name issueNumber = do
  let editIssue = EditIssue Nothing Nothing Nothing (Just StateClosed) Nothing Nothing
  withGithubApiSemaphore' requestSemaphore $
    githubWithLogging' baseContext (editIssueR owner name issueNumber editIssue)

closeIssueWithComment :: BaseContext -> Name Owner -> Name Repo -> IssueNumber -> Text -> IO (Either Error Issue)
closeIssueWithComment baseContext@(BaseContext {requestSemaphore}) owner name issueNumber commentBody = do
  unless (T.null $ T.strip commentBody) $ do
    void $ withGithubApiSemaphore' requestSemaphore $
      githubWithLogging' baseContext (createCommentR owner name issueNumber commentBody)

  closeIssue baseContext owner name issueNumber
