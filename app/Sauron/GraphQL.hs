{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sauron.GraphQL (
  queryBranchesWithInfos
  , sortBranchesByDate
  , filterBranchesByAuthor
  , filterBranchesByActivity
  , filterBranchesByInactivity
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
      defaultBranchRef {
        name
        target {
          oid
        }
      }
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
        }
        pageInfo {
          hasNextPage
          endCursor
        }
      }
      pullRequests(states: [OPEN, CLOSED, MERGED], first: 100) {
        nodes {
          number
          title
          url
          headRefName
          state
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
  , pullRequests :: Maybe PullRequests
  } deriving (Show, Generic)
instance FromJSON Repository

data DefaultBranchRef = DefaultBranchRef {
  defaultBranchName :: Maybe Text
  , defaultBranchTarget :: Maybe Target
  } deriving (Show, Generic)
instance FromJSON DefaultBranchRef where
  parseJSON = withObject "DefaultBranchRef" $ \o -> DefaultBranchRef
    <$> o .:? "name"
    <*> o .:? "target"

data Refs = Refs {
  nodes :: Maybe [RefNode]
  , pageInfo :: Maybe PageInfo
  } deriving (Show, Generic)
instance FromJSON Refs

data PullRequests = PullRequests {
  prNodes :: Maybe [GraphQLPullRequestWithHead]
  } deriving (Show, Generic)
instance FromJSON PullRequests where
  parseJSON = withObject "PullRequests" $ \o -> PullRequests <$> o .:? "nodes"

data GraphQLPullRequestWithHead = GraphQLPullRequestWithHead {
  prWithHeadNumber :: Maybe Int
  , prWithHeadTitle :: Maybe Text
  , prWithHeadUrl :: Maybe Text
  , prWithHeadRefName :: Maybe Text
  , prWithHeadState :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON GraphQLPullRequestWithHead where
  parseJSON = withObject "GraphQLPullRequestWithHead" $ \o -> GraphQLPullRequestWithHead
    <$> o .:? "number"
    <*> o .:? "title"
    <*> o .:? "url"
    <*> o .:? "headRefName"
    <*> o .:? "state"

data RefNode = RefNode {
  name :: Text
  , target :: Maybe Target
  , branchCompare :: Maybe BranchComparison
  } deriving (Show, Generic)
instance FromJSON RefNode where
  parseJSON = withObject "RefNode" $ \o -> RefNode
    <$> o .: "name"
    <*> o .:? "target"
    <*> o .:? "compare"

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


queryBranchesWithInfos :: MonadIO m => (Text -> IO ()) -> Text -> Text -> Text -> Maybe Text -> Int -> m (Either Text [BranchWithInfo])
queryBranchesWithInfos debugFn authToken owner' repoName repoDefaultBranch first' = liftIO $ do
  debugFn $ "GraphQL query for " <> owner' <> "/" <> repoName <> " (first " <> show first' <> ")"

  let defaultBranch = fromMaybe "main" repoDefaultBranch
  let requestPayload = GraphQLRequest {
        query = getBranchesQuery
        , variables = BranchVariables owner' repoName first' defaultBranch
        }

  result :: Either SomeException BranchResponse <- try $ do
    initialRequest <- parseRequest githubGraphQLEndpoint
    let httpRequest = setRequestMethod "POST"
                    $ setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 authToken]
                    $ setRequestHeader "Content-Type" ["application/json"]
                    $ setRequestHeader "User-Agent" ["sauron-app"]
                    $ setRequestResponseTimeout (responseTimeoutMicro (30 * 1000000))
                    $ setRequestBodyJSON requestPayload
                    $ initialRequest

    getResponseBody <$> httpJSON httpRequest

  case result of
    Left (ex :: SomeException) -> do
      debugFn $ "GraphQL HTTP request failed: " <> T.pack (show ex)
      return $ Left $ "HTTP request failed: " <> T.pack (show ex)
    Right body -> do
      debugFn "Processing GraphQL response body"
      case (data' body, errors body) of
        (Just repoData, Nothing) -> do
          case (repository repoData >>= refs >>= nodes, repository repoData >>= pullRequests >>= prNodes) of
            (Just refNodes, maybePullRequests) -> do
              let pullRequestList = fromMaybe [] maybePullRequests
              debugFn $ "Found " <> show (length refNodes) <> " branches and " <> show (length pullRequestList) <> " pull requests"
              return $ Right $ mapMaybe (refNodeToBranchWithComparison defaultBranch pullRequestList) refNodes
            (Nothing, _) -> do
              debugFn "No branches found in response"
              return $ Right []
        (_, Just errs) -> do
          debugFn $ "GraphQL errors: " <> T.intercalate ", " (map message errs)
          return $ Left $ T.intercalate ", " (map message errs)
        (Nothing, Nothing) -> do
          debugFn "No data returned from GitHub"
          return $ Left "No data returned from GitHub"

refNodeToBranchWithComparison :: Text -> [GraphQLPullRequestWithHead] -> RefNode -> Maybe BranchWithInfo
refNodeToBranchWithComparison defaultBranchName pullRequests refNode = do
  let branchName = name refNode
  target' <- target refNode
  let prInfo = findPullRequestForBranch branchName pullRequests
  let compareInfo = branchCompare refNode

  -- Extract ahead/behind counts from the GraphQL comparison
  let (aheadCount, behindCount) = case compareInfo of
        Just comparison -> (aheadBy comparison, behindBy comparison)
        Nothing ->
          -- Fallback: if this is the default branch, it's up to date
          if branchName == defaultBranchName
            then (Just 0, Just 0)
            else (Nothing, Nothing)

  return $ BranchWithInfo {
    branchWithInfoBranchName = branchName
    , branchWithInfoCommitOid = oid target'
    , branchWithInfoCommitAuthor = author target' >>= user >>= login
    , branchWithInfoAuthorEmail = author target' >>= email
    , branchWithInfoCommitDate = committedDate target'
    , branchWithInfoCheckStatus = statusCheckRollup target' >>= statusState
    , branchWithInfoAssociatedPR = prInfo
    , branchWithInfoAheadBy = aheadCount
    , branchWithInfoBehindBy = behindCount
    }

findPullRequestForBranch :: Text -> [GraphQLPullRequestWithHead] -> Maybe GraphQLPullRequest
findPullRequestForBranch branchName pullRequests =
  case find (\pr -> prWithHeadRefName pr == Just branchName) pullRequests of
    Just prWithHead -> Just $ GraphQLPullRequest {
      prNumber = prWithHeadNumber prWithHead
      , prTitle = prWithHeadTitle prWithHead
      , prUrl = prWithHeadUrl prWithHead
      , prState = prWithHeadState prWithHead
    }
    Nothing -> Nothing

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
