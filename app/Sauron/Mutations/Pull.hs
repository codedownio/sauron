{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Mutations.Pull (
  closePull
  , reopenPull
  , mergePull
  ) where

import Data.Aeson (Value, decode, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import Data.String.Interpolate
import GitHub
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), responseStatus)
import Network.HTTP.Types.Status (statusCode, statusMessage)
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

-- | Merge a pull request with the given method and optional commit title/message
-- (Nothing means GitHub's default). Returns GitHub's confirmation message on success,
-- or its explanation of why the merge was refused.
mergePull :: BaseContext -> Name Owner -> Name Repo -> IssueNumber -> MergeMethod -> Maybe Text -> Maybe Text -> IO (Either Text Text)
mergePull baseContext@(BaseContext {requestSemaphore}) owner name pullNumber method commitTitle commitMessage =
  withGithubApiSemaphore' requestSemaphore (githubWithLogging' baseContext request) >>= \case
    Right (PullRequestMergeResult {pullRequestMergeResultMessage}) -> return $ Right pullRequestMergeResultMessage
    Left err -> return $ Left $ formatMergeError err
  where
    request = mergePullRequestWithOptionsR owner name pullNumber $ MergePullRequestOptions {
      mergePullRequestOptionsCommitTitle = commitTitle
      , mergePullRequestOptionsCommitMessage = commitMessage
      , mergePullRequestOptionsSha = Nothing
      , mergePullRequestOptionsMergeMethod = Just method
      }

-- | GitHub explains a refused merge in the response body ("Pull Request is not mergeable",
-- "Rebase merges are not allowed on this repository", ...), which is much more useful than
-- the status code by itself.
formatMergeError :: Error -> Text
formatMergeError (HTTPError (HttpExceptionRequest _ (StatusCodeException response body))) =
  fromMaybe [i|HTTP #{statusCode status} #{decodeUtf8 (statusMessage status) :: Text}|] (messageFromBody body)
  where
    status = responseStatus response
formatMergeError (HTTPError httpErr) = "Network error: " <> show httpErr
formatMergeError (ParseError msg) = "Parse error: " <> msg
formatMergeError (JsonError msg) = "JSON error: " <> msg
formatMergeError (UserError msg) = msg

messageFromBody :: ByteString -> Maybe Text
messageFromBody body = do
  value <- decode (LBS.fromStrict body) :: Maybe Value
  parseMaybe (withObject "error" (.: "message")) value
