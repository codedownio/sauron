{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Mutations.Pull (
  closePull
  , reopenPull
  ) where

import GitHub
import Relude
import Sauron.Actions.Util (withGithubApiSemaphore', githubWithLogging')
import Sauron.Types


emptyEditPullRequest :: EditPullRequest
emptyEditPullRequest = EditPullRequest {
  editPullRequestTitle = Nothing
  , editPullRequestBody = Nothing
  , editPullRequestState = Nothing
  , editPullRequestBase = Nothing
  , editPullRequestMaintainerCanModify = Nothing
  }

closePull :: BaseContext -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error PullRequest)
closePull baseContext@(BaseContext {requestSemaphore}) owner name pullNumber = do
  withGithubApiSemaphore' requestSemaphore $
    githubWithLogging' baseContext $ updatePullRequestR owner name pullNumber $ emptyEditPullRequest {
      editPullRequestState = Just StateClosed
      }

reopenPull :: BaseContext -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error PullRequest)
reopenPull baseContext@(BaseContext {requestSemaphore}) owner name pullNumber = do
  withGithubApiSemaphore' requestSemaphore $
    githubWithLogging' baseContext $ updatePullRequestR owner name pullNumber $ emptyEditPullRequest {
      editPullRequestState = Just StateOpen
      }
