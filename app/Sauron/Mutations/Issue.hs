{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Mutations.Issue (
  submitComment
  , closeIssue
  , closeIssueWithComment
  , reopenIssue
  , createNewIssue
  ) where

import qualified Data.Text as T
import GitHub
import GitHub.Endpoints.Issues (newIssue)
import Relude
import Sauron.Actions.Util (withGithubApiSemaphore', githubWithLogging')
import Sauron.Types


emptyEditIssue :: EditIssue
emptyEditIssue = EditIssue {
  editIssueTitle = Nothing
  , editIssueBody = Nothing
  , editIssueAssignees = Nothing
  , editIssueState = Nothing
  , editIssueMilestone = Nothing
  , editIssueLabels = Nothing
  }

submitComment :: BaseContext -> Name Owner -> Name Repo -> IssueNumber -> Text -> IO (Either Error Comment)
submitComment baseContext@(BaseContext {requestSemaphore}) owner name issueNumber commentBody = do
  withGithubApiSemaphore' requestSemaphore $
    githubWithLogging' baseContext (createCommentR owner name issueNumber commentBody)

closeIssue :: BaseContext -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error Issue)
closeIssue baseContext@(BaseContext {requestSemaphore}) owner name issueNumber = do
  withGithubApiSemaphore' requestSemaphore $
    githubWithLogging' baseContext $ editIssueR owner name issueNumber $ emptyEditIssue {
      editIssueState = Just StateClosed
      }

closeIssueWithComment :: BaseContext -> Name Owner -> Name Repo -> IssueNumber -> Text -> IO (Either Error Issue)
closeIssueWithComment baseContext@(BaseContext {requestSemaphore}) owner name issueNumber commentBody = do
  unless (T.null $ T.strip commentBody) $ do
    void $ withGithubApiSemaphore' requestSemaphore $
      githubWithLogging' baseContext (createCommentR owner name issueNumber commentBody)

  closeIssue baseContext owner name issueNumber

reopenIssue :: BaseContext -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error Issue)
reopenIssue baseContext@(BaseContext {requestSemaphore}) owner name issueNumber = do
  withGithubApiSemaphore' requestSemaphore $
    githubWithLogging' baseContext $ editIssueR owner name issueNumber $ emptyEditIssue {
      editIssueState = Just StateOpen
      }

createNewIssue :: BaseContext -> Name Owner -> Name Repo -> Text -> Text -> IO (Either Error Issue)
createNewIssue baseContext@(BaseContext {requestSemaphore}) owner name title body = do
  let ni = (newIssue title) { newIssueBody = if T.null (T.strip body) then Nothing else Just body }
  withGithubApiSemaphore' requestSemaphore $
    githubWithLogging' baseContext (createIssueR owner name ni)
