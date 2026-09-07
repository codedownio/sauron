{-# LANGUAGE OverloadedStrings #-}

-- | The per-file "viewed" state of a pull request review (the checkboxes in the web UI's
-- "Files changed" tab) is only exposed through the GraphQL API, not REST.
module Sauron.GraphQL.PullRequestFiles (
  queryPullRequestViewedStates
  , setFileViewedState
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Map as M
import Data.String.Interpolate
import GitHub (Name, Owner, Repo, toPathPart)
import GitHub.Auth (Auth(..))
import Network.HTTP.Conduit (responseTimeoutMicro)
import Network.HTTP.Simple
import Relude
import Sauron.GraphQL (githubGraphQLEndpoint)
import Sauron.Types
import UnliftIO.Exception (try)


viewedStatesQuery :: Text
viewedStatesQuery = [i|
  query PullRequestViewedStates($owner: String!, $name: String!, $number: Int!, $cursor: String) {
    repository(owner: $owner, name: $name) {
      pullRequest(number: $number) {
        id
        files(first: 100, after: $cursor) {
          nodes {
            path
            viewerViewedState
          }
          pageInfo {
            hasNextPage
            endCursor
          }
        }
      }
    }
  }
  |]

-- | Fetch the pull request's GraphQL node id (needed for the mark/unmark mutations)
-- plus every changed file's viewed state, following pagination.
queryPullRequestViewedStates :: (
  MonadIO m
  ) => BaseContext -> Name Owner -> Name Repo -> Int -> m (Either Text (Text, Map Text FileViewedState))
queryPullRequestViewedStates bc owner name number = go Nothing mempty
  where
    go cursor acc =
      runGraphQL bc viewedStatesQuery (object [
        "owner" .= toPathPart owner
        , "name" .= toPathPart name
        , "number" .= number
        , "cursor" .= cursor
        ]) >>= \case
        Left err -> return $ Left err
        Right value -> case parseEither parseFilesPage value of
          Left err -> return $ Left $ toText err
          Right (prId, states, nextCursor) -> do
            let acc' = acc <> M.fromList states
            case nextCursor of
              Just c -> go (Just c) acc'
              Nothing -> return $ Right (prId, acc')

    parseFilesPage :: Value -> Parser (Text, [(Text, FileViewedState)], Maybe Text)
    parseFilesPage = withObject "data" $ \o -> do
      pr <- o .: "repository" >>= (.: "pullRequest")
      prId <- pr .: "id"
      files <- pr .: "files"
      nodes <- files .: "nodes"
      states <- forM nodes $ \node -> do
        path <- node .: "path"
        viewedState <- node .: "viewerViewedState" >>= parseViewedState
        return (path, viewedState)
      pageInfo <- files .: "pageInfo"
      hasNextPage <- pageInfo .: "hasNextPage"
      endCursor <- pageInfo .:? "endCursor"
      return (prId, states, if hasNextPage then endCursor else Nothing)

    parseViewedState :: Text -> Parser FileViewedState
    parseViewedState "VIEWED" = return FileViewed
    parseViewedState "UNVIEWED" = return FileUnviewed
    parseViewedState "DISMISSED" = return FileDismissed
    parseViewedState other = fail [i|Unknown viewerViewedState: #{other}|]

-- | Mark or unmark a file as viewed. The pull request is identified by its GraphQL
-- node id (from 'queryPullRequestViewedStates').
setFileViewedState :: (
  MonadIO m
  ) => BaseContext -> Text -> Text -> Bool -> m (Either Text ())
setFileViewedState bc prId path viewed =
  fmap void $ runGraphQL bc mutation $ object ["prId" .= prId, "path" .= path]
  where
    mutationField :: Text
    mutationField = if viewed then "markFileAsViewed" else "unmarkFileAsViewed"

    mutation = [i|
      mutation SetFileViewedState($prId: ID!, $path: String!) {
        #{mutationField}(input: {pullRequestId: $prId, path: $path}) {
          clientMutationId
        }
      }
      |]

-- | Run a GraphQL query/mutation against GitHub and return the "data" value.
runGraphQL :: MonadIO m => BaseContext -> Text -> Value -> m (Either Text Value)
runGraphQL bc queryText variables = liftIO $ case auth bc of
  OAuth token -> do
    result <- try $ do
      initialRequest <- parseRequest githubGraphQLEndpoint
      let httpRequest = initialRequest
                      & setRequestBodyJSON (object ["query" .= queryText, "variables" .= variables])
                      & setRequestResponseTimeout (responseTimeoutMicro (30 * 1000000))
                      & setRequestHeader "User-Agent" ["sauron-app"]
                      & setRequestHeader "Content-Type" ["application/json"]
                      & setRequestHeader "Authorization" ["Bearer " <> token]
                      & setRequestMethod "POST"
      getResponseBody <$> httpJSON httpRequest
    return $ case result of
      Left (ex :: SomeException) -> Left [i|GraphQL request failed: #{ex}|]
      Right body -> case parseEither parseResponse body of
        Left err -> Left $ toText err
        Right x -> x
  _ -> return $ Left "GraphQL requires an OAuth token"
  where
    parseResponse :: Value -> Parser (Either Text Value)
    parseResponse = withObject "response" $ \o -> do
      maybeErrors <- o .:? "errors"
      maybeData <- o .:? "data"
      case (maybeErrors :: Maybe [Object], maybeData) of
        (Just errs@(_:_), _) -> do
          messages <- forM errs (.: "message")
          return $ Left $ unwords messages
        (_, Just d) -> return $ Right d
        _ -> return $ Left "No data returned from GitHub"
