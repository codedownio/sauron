{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Sauron.GraphQL (
  queryBranchesWithCommits
  , sortBranchesByDate
  , filterBranchesByAuthor
  , prNumber
  ) where

import Control.Exception.Safe (try)
import Data.Aeson
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Network.HTTP.Conduit (responseTimeoutMicro)
import Network.HTTP.Simple
import Relude
import Sauron.Types hiding (PageInfo)

-- GitHub GraphQL API endpoint
githubGraphQLEndpoint :: String
githubGraphQLEndpoint = "https://api.github.com/graphql"

-- GraphQL query to get repository branches with commit information and associated pull requests
getBranchesQuery :: Text
getBranchesQuery = [i|
  query GetBranchesWithCommits($owner: String!, $name: String!, $first: Int!) {
    repository(owner: $owner, name: $name) {
      defaultBranchRef { name }
      refs(refPrefix: "refs/heads/", first: $first) {
        nodes {
          name
          target {
            ... on Commit {
              oid
              author {
                name
                email
                date
                user {
                  login
                }
              }
              committedDate
              statusCheckRollup {
                state
              }
            }
          }
          associatedPullRequests(states: OPEN, first: 1) {
            nodes {
              number
              title
              url
            }
          }
        }
        pageInfo {
          hasNextPage
          endCursor
        }
      }
    }
  }
  |]

data BranchResponse = BranchResponse {
  data' :: Maybe RepositoryData
  , errors :: Maybe [GraphQLError]
  } deriving (Show, Generic)

instance FromJSON BranchResponse where
  parseJSON = withObject "BranchResponse" $ \o -> BranchResponse
    <$> o .:? "data"
    <*> o .:? "errors"

data RepositoryData = RepositoryData {
  repository :: Maybe Repository
  } deriving (Show, Generic)

instance FromJSON RepositoryData

data Repository = Repository {
  refs :: Maybe Refs
  , defaultBranchRef :: Maybe DefaultBranchRef
  } deriving (Show, Generic)

instance FromJSON Repository

data DefaultBranchRef = DefaultBranchRef {
  defaultBranchName :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON DefaultBranchRef where
  parseJSON = withObject "DefaultBranchRef" $ \o -> DefaultBranchRef <$> o .:? "name"

data Refs = Refs {
  nodes :: Maybe [RefNode]
  , pageInfo :: Maybe PageInfo
  } deriving (Show, Generic)

instance FromJSON Refs

data RefNode = RefNode {
  name :: Text
  , target :: Maybe Target
  , associatedPullRequests :: Maybe AssociatedPRs
  } deriving (Show, Generic)

instance FromJSON RefNode

data AssociatedPRs = AssociatedPRs {
  prNodes :: Maybe [GraphQLPullRequest]
  } deriving (Show, Generic)

instance FromJSON AssociatedPRs where
  parseJSON = withObject "AssociatedPRs" $ \o -> AssociatedPRs <$> o .:? "nodes"

data Target = Target {
  oid :: Maybe Text
  , author :: Maybe Author
  , committedDate :: Maybe Text
  , statusCheckRollup :: Maybe StatusCheckRollup
  } deriving (Show, Generic)

instance FromJSON Target

data StatusCheckRollup = StatusCheckRollup {
  statusState :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON StatusCheckRollup where
  parseJSON = withObject "StatusCheckRollup" $ \o -> StatusCheckRollup <$> o .:? "state"

data Author = Author {
  authorName :: Maybe Text
  , email :: Maybe Text
  , date :: Maybe Text
  , user :: Maybe User
  } deriving (Show, Generic)
instance FromJSON Author where
  parseJSON = withObject "Author" $ \o -> Author
    <$> o .:? "name"
    <*> o .:? "email"
    <*> o .:? "date"
    <*> o .:? "user"

data User = User { login :: Maybe Text }
  deriving (Show, Generic)
instance FromJSON User

data PageInfo = PageInfo {
  hasNextPage :: Maybe Bool
  , endCursor :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON PageInfo

data GraphQLError = GraphQLError { message :: Text } deriving (Show, Generic)
instance FromJSON GraphQLError

-- GraphQL variables for the query
data BranchVariables = BranchVariables
  { owner :: Text
  , repositoryName :: Text  -- 'name' is a reserved keyword
  , first :: Int
  } deriving (Show, Generic)
instance ToJSON BranchVariables where
  toJSON (BranchVariables owner' name' first') = object [
    "owner" .= owner'
    , "name" .= name'
    , "first" .= first'
    ]

data GraphQLRequest = GraphQLRequest {
  query :: Text
  , variables :: BranchVariables
  } deriving (Show, Generic)
instance ToJSON GraphQLRequest

queryBranchesWithCommits :: MonadIO m => (Text -> IO ()) -> Text -> Text -> Text -> Int -> m (Either Text [BranchWithInfo])
queryBranchesWithCommits debugFn authToken owner' repoName first' = liftIO $ do
  debugFn $ "GraphQL query for " <> owner' <> "/" <> repoName <> " (first " <> show first' <> ")"
  let requestPayload = GraphQLRequest
        { query = getBranchesQuery
        , variables = BranchVariables owner' repoName first'
        }
  debugFn $ "GraphQL query: " <> T.take 200 getBranchesQuery

  result <- try $ do
    debugFn "Creating HTTP request"
    initialRequest <- parseRequest githubGraphQLEndpoint
    let httpRequest = setRequestMethod "POST"
                    $ setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 authToken]
                    $ setRequestHeader "Content-Type" ["application/json"]
                    $ setRequestHeader "User-Agent" ["sauron-app"]
                    $ setRequestResponseTimeout (responseTimeoutMicro (30 * 1000000)) -- 30 second timeout
                    $ setRequestBodyJSON requestPayload
                    $ initialRequest

    debugFn "Sending GraphQL HTTP request"
    response <- httpJSON httpRequest
    debugFn "HTTP response received, parsing JSON"
    let body = getResponseBody response :: BranchResponse
    debugFn "GraphQL response parsed successfully"
    return body

  case result of
    Left (ex :: SomeException) -> do
      debugFn $ "GraphQL HTTP request failed: " <> T.pack (show ex)
      return $ Left $ "HTTP request failed: " <> T.pack (show ex)
    Right body -> do
      debugFn "Processing GraphQL response body"
      case (data' body, errors body) of
        (Just repoData, Nothing) -> do
          debugFn "GraphQL success, processing data"
          case repository repoData >>= refs >>= nodes of
            Just refNodes -> do
              debugFn $ "Found " <> show (length refNodes) <> " branches"
              return $ Right $ mapMaybe refNodeToBranch refNodes
            Nothing -> do
              debugFn "No branches found in response"
              return $ Right []
        (_, Just errs) -> do
          debugFn $ "GraphQL errors: " <> T.intercalate ", " (map message errs)
          return $ Left $ T.intercalate ", " (map message errs)
        (Nothing, Nothing) -> do
          debugFn "No data returned from GitHub"
          return $ Left "No data returned from GitHub"

refNodeToBranch :: RefNode -> Maybe BranchWithInfo
refNodeToBranch refNode = do
  let branchName = name refNode
  target' <- target refNode
  let prInfo = associatedPullRequests refNode >>= prNodes >>= viaNonEmpty head
  return $ BranchWithInfo {
    branchWithInfoBranchName = branchName
    , branchWithInfoCommitOid = oid target'
    , branchWithInfoCommitAuthor = author target' >>= user >>= login
    , branchWithInfoAuthorEmail = author target' >>= email
    , branchWithInfoCommitDate = committedDate target'
    , branchWithInfoCheckStatus = statusCheckRollup target' >>= statusState
    , branchWithInfoAssociatedPR = prInfo
    }

filterBranchesByAuthor :: Text -> [BranchWithInfo] -> [BranchWithInfo]
filterBranchesByAuthor currentUser branches =
  filter (\branch -> branchWithInfoCommitAuthor branch == Just currentUser) branches

sortBranchesByDate :: [BranchWithInfo] -> [BranchWithInfo]
sortBranchesByDate branches = sortBy (comparing (Down . commitDateUtc)) branches
  where
    commitDateUtc :: BranchWithInfo -> Maybe UTCTime
    commitDateUtc branch = branchWithInfoCommitDate branch >>= parseISODate

    parseISODate :: Text -> Maybe UTCTime
    parseISODate dateStr = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toString dateStr)

-- TODO: Implement active/stale branch filtering
-- filterBranchesByActivity :: UTCTime -> [BranchWithInfo] -> [BranchWithInfo]
-- filterBranchesByActivity cutoffTime branches =
--   filter (isBranchActive cutoffTime) branches
--   where
--     isBranchActive :: UTCTime -> BranchWithInfo -> Bool
--     isBranchActive cutoff branch =
--       case branchWithInfoCommitDate branch of
--         Nothing -> False
--         Just dateStr ->
--           case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toString dateStr) of
--             Nothing -> False
--             Just commitTime -> commitTime > cutoff
