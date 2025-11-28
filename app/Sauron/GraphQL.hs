{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sauron.GraphQL (
  queryBranchesWithCommits
  , sortBranchesByDate
  , filterBranchesByAuthor
  , filterBranchesByActivity
  , filterBranchesByInactivity
  , prNumber
  ) where

import Control.Exception.Safe (try)
import Data.Aeson
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Network.HTTP.Conduit (responseTimeoutMicro)
import Network.HTTP.Simple
import Relude
import Sauron.Types hiding (PageInfo)
import System.IO.Unsafe (unsafePerformIO)


githubGraphQLEndpoint :: String
githubGraphQLEndpoint = "https://api.github.com/graphql"

getBranchesQuery :: Text
getBranchesQuery = [i|
  query GetBranchesWithCommits($owner: String!, $name: String!, $first: Int!, $defaultBranch: String!) {
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
          compare(headRef: $defaultBranch) {
            aheadBy
            behindBy
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
  , branchCompare :: Maybe BranchComparison
  , associatedPullRequests :: Maybe AssociatedPRs
  } deriving (Show, Generic)
instance FromJSON RefNode where
  parseJSON = withObject "RefNode" $ \o -> RefNode
    <$> o .: "name"
    <*> o .:? "target"
    <*> o .:? "compare"
    <*> o .:? "associatedPullRequests"

data AssociatedPRs = AssociatedPRs {
  prNodes :: Maybe [GraphQLPullRequest]
  } deriving (Show, Generic)
instance FromJSON AssociatedPRs where
  parseJSON = withObject "AssociatedPRs" $ \o -> AssociatedPRs <$> o .:? "nodes"

data BranchComparison = BranchComparison {
  aheadBy :: Maybe Int
  , behindBy :: Maybe Int
  } deriving (Show, Generic)
instance FromJSON BranchComparison

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
  , defaultBranch :: Text
  } deriving (Show, Generic)
instance ToJSON BranchVariables where
  toJSON (BranchVariables owner' name' first' defaultBranch') = object [
    "owner" .= owner'
    , "name" .= name'
    , "first" .= first'
    , "defaultBranch" .= defaultBranch'
    ]

data GraphQLRequest = GraphQLRequest {
  query :: Text
  , variables :: BranchVariables
  } deriving (Show, Generic)
instance ToJSON GraphQLRequest

-- First, let's create a simple query to get just the default branch name
getDefaultBranchQuery :: Text
getDefaultBranchQuery = [i|
  query GetDefaultBranch($owner: String!, $name: String!) {
    repository(owner: $owner, name: $name) {
      defaultBranchRef { name }
    }
  }
  |]

queryBranchesWithCommits :: MonadIO m => (Text -> IO ()) -> Text -> Text -> Text -> Int -> m (Either Text [BranchWithInfo])
queryBranchesWithCommits debugFn authToken owner' repoName first' = liftIO $ do
  debugFn $ "GraphQL query for " <> owner' <> "/" <> repoName <> " (first " <> show first' <> ")"

  -- First, get the default branch name
  let defaultBranchPayload = GraphQLRequest
        { query = getDefaultBranchQuery
        , variables = BranchVariables owner' repoName 1 "main"  -- default branch field is not used in this query
        }

  defaultBranchResult <- try $ do
    debugFn "Getting default branch name"
    initialRequest <- parseRequest githubGraphQLEndpoint
    let httpRequest = setRequestMethod "POST"
                    $ setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 authToken]
                    $ setRequestHeader "Content-Type" ["application/json"]
                    $ setRequestHeader "User-Agent" ["sauron-app"]
                    $ setRequestResponseTimeout (responseTimeoutMicro (30 * 1000000))
                    $ setRequestBodyJSON defaultBranchPayload
                    $ initialRequest
    response <- httpJSON httpRequest
    let body = getResponseBody response :: BranchResponse
    return body

  case defaultBranchResult of
    Left (ex :: SomeException) -> do
      debugFn $ "Default branch query failed: " <> T.pack (show ex)
      return $ Left $ "Default branch query failed: " <> T.pack (show ex)
    Right defaultBranchBody -> do
      case (data' defaultBranchBody, errors defaultBranchBody) of
        (Just repoData, Nothing) -> do
          let defaultBranchName' = fromMaybe "main" $ repository repoData >>= defaultBranchRef >>= defaultBranchName
          debugFn $ "Found default branch: " <> defaultBranchName'

          -- Now query branches with comparison to default branch
          let requestPayload = GraphQLRequest
                { query = getBranchesQuery
                , variables = BranchVariables owner' repoName first' defaultBranchName'
                }
          debugFn $ "GraphQL query: " <> T.take 200 getBranchesQuery

          result <- try $ do
            debugFn "Creating HTTP request for branches with comparison"
            initialRequest <- parseRequest githubGraphQLEndpoint
            let httpRequest = setRequestMethod "POST"
                            $ setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 authToken]
                            $ setRequestHeader "Content-Type" ["application/json"]
                            $ setRequestHeader "User-Agent" ["sauron-app"]
                            $ setRequestResponseTimeout (responseTimeoutMicro (30 * 1000000))
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
        (_, Just errs) -> do
          debugFn $ "Default branch query errors: " <> T.intercalate ", " (map message errs)
          return $ Left $ T.intercalate ", " (map message errs)
        (Nothing, Nothing) -> do
          debugFn "No data returned from GitHub for default branch query"
          return $ Left "No data returned from GitHub for default branch query"

refNodeToBranch :: RefNode -> Maybe BranchWithInfo
refNodeToBranch refNode = do
  let branchName = name refNode
  target' <- target refNode
  let prInfo = associatedPullRequests refNode >>= prNodes >>= viaNonEmpty head
  let compareInfo = branchCompare refNode
  return $ BranchWithInfo {
    branchWithInfoBranchName = branchName
    , branchWithInfoCommitOid = oid target'
    , branchWithInfoCommitAuthor = author target' >>= user >>= login
    , branchWithInfoAuthorEmail = author target' >>= email
    , branchWithInfoCommitDate = committedDate target'
    , branchWithInfoCheckStatus = statusCheckRollup target' >>= statusState
    , branchWithInfoAssociatedPR = prInfo
    , branchWithInfoAheadBy = compareInfo >>= aheadBy
    , branchWithInfoBehindBy = compareInfo >>= behindBy
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

-- Filter branches that have activity within the specified number of days
filterBranchesByActivity :: Int -> [BranchWithInfo] -> [BranchWithInfo]
filterBranchesByActivity daysCutoff branches = unsafePerformIO $ do
  currentTime <- getCurrentTime
  return $ filter (isBranchActive currentTime daysCutoff) branches
  where
    isBranchActive :: UTCTime -> Int -> BranchWithInfo -> Bool
    isBranchActive currentTime days branch =
      case branchWithInfoCommitDate branch of
        Nothing -> False
        Just dateStr ->
          case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toString dateStr) of
            Nothing -> False
            Just commitTime ->
              let cutoffTime = addUTCTime (fromIntegral (-days * 24 * 60 * 60)) currentTime
              in commitTime > cutoffTime

-- Filter branches that have NO activity within the specified number of days (stale branches)
filterBranchesByInactivity :: Int -> [BranchWithInfo] -> [BranchWithInfo]
filterBranchesByInactivity daysCutoff branches = unsafePerformIO $ do
  currentTime <- getCurrentTime
  return $ filter (isBranchStale currentTime daysCutoff) branches
  where
    isBranchStale :: UTCTime -> Int -> BranchWithInfo -> Bool
    isBranchStale currentTime days branch =
      case branchWithInfoCommitDate branch of
        Nothing -> True  -- No commit date means it's probably stale
        Just dateStr ->
          case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toString dateStr) of
            Nothing -> True  -- Can't parse date, consider stale
            Just commitTime ->
              let cutoffTime = addUTCTime (fromIntegral (-days * 24 * 60 * 60)) currentTime
              in commitTime <= cutoffTime
