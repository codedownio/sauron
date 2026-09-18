{-# LANGUAGE OverloadedStrings #-}

-- | Bulk workflow run statuses.
--
-- REST reports one workflow run (or one run's job list) per request, so a repo with several runs
-- in flight spends a lot of the hourly allowance just watching them. Every run also appears as a
-- check suite on its head commit, and that view is in GraphQL: one query returns every run on
-- every commit we ask about, and GraphQL charges it a single point out of its own separate budget.
module Sauron.GraphQL.WorkflowRuns (
  queryWorkflowRunStatuses
  , RunStatus(..)
  , JobStatus(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time (UTCTime)
import GitHub (Name, Owner, Repo, toPathPart)
import Relude
import Sauron.GraphQL (runGraphQL)
import Sauron.Types


-- | What GitHub currently says about a workflow run. The status and conclusion use the same
-- lowercase spellings REST does, so 'Sauron.UI.Statuses.chooseWorkflowStatus' reads them the same
-- way whichever API they came from.
data RunStatus = RunStatus {
  runStatusStatus :: Text
  , runStatusConclusion :: Maybe Text
  , runStatusUpdatedAt :: Maybe UTCTime
  , runStatusJobs :: Map Int JobStatus
  } deriving (Show, Eq)

-- | Where a single job in a run has got to, keyed in 'runStatusJobs' by job id: a job's check run
-- carries the same database id the REST jobs endpoint gives it.
data JobStatus = JobStatus {
  jobStatusStatus :: Text
  , jobStatusConclusion :: Maybe Text
  } deriving (Show, Eq)

-- | Fetch the current status of every workflow run sitting on any of the given head commits, keyed
-- by the run's REST id. A commit GitHub won't resolve for us (a fork's head, a commit that's been
-- garbage collected) just contributes nothing, so a run missing from the result means "no answer",
-- not "finished" -- the caller falls back to REST for those.
queryWorkflowRunStatuses :: (
  MonadIO m
  ) => BaseContext -> Name Owner -> Name Repo -> [(Int, Text)] -> m (Either Text (Map Int RunStatus))
queryWorkflowRunStatuses bc owner name runs = case mapMaybe sanitizeSha (ordNub (map snd runs)) of
  [] -> return (Right mempty)
  validShas ->
    runGraphQL bc (describeRuns (map fst runs)) (statusesQuery validShas) (object [
      "owner" .= toPathPart owner
      , "name" .= toPathPart name
      ]) >>= \case
      Left err -> return (Left err)
      Right value -> return $ first toText $ parseEither parseRunStatuses value

-- | The runs a batch query covered, for its line in the log pane. One query stands in for what
-- used to be a request per run, so the line says which ones -- capped, since there can be a lot.
describeRuns :: [Int] -> Text
describeRuns runIds = "runs " <> T.intercalate ", " (map show shown) <> (if null rest then "" else ", ...")
  where (shown, rest) = splitAt maxRunIdsLogged runIds

maxRunIdsLogged :: Int
maxRunIdsLogged = 10

-- | Commit ids go into the query text rather than into variables, since GraphQL aliases can't be
-- parameterized. Only hex passes, so nothing from the API can escape the string it's spliced into.
sanitizeSha :: Text -> Maybe Text
sanitizeSha sha
  | not (T.null sha), T.all C.isHexDigit sha = Just sha
  | otherwise = Nothing

-- | One aliased lookup per commit, each pulling back the check suites on it. A commit's suites
-- cover every workflow run for that commit, and the whole query still costs one point.
statusesQuery :: [Text] -> Text
statusesQuery shas = [i|
  query WorkflowRunStatuses($owner: String!, $name: String!) {
    repository(owner: $owner, name: $name) {
      #{T.intercalate "\n      " (zipWith commitField [(0 :: Int)..] shas)}
    }
  }
  |]
  where
    commitField idx sha = [i|c#{idx}: object(oid: "#{sha}") { ... on Commit { checkSuites(first: 50) { nodes { status conclusion workflowRun { databaseId updatedAt } checkRuns(first: 100) { nodes { databaseId status conclusion } } } } } }|]

parseRunStatuses :: Value -> Parser (Map Int RunStatus)
parseRunStatuses = withObject "data" $ \o -> do
  -- One key per commit we asked about, null for any the repo couldn't resolve
  maybeCommits :: Maybe (Map Text (Maybe CommitSuites)) <- o .:? "repository"
  let commits = fromMaybe mempty maybeCommits
  return $ M.fromList [
    (runId, RunStatus (lowercaseEnum status) (lowercaseEnum <$> conclusion) updatedAt (jobsOf checkRuns))
    | Just (CommitSuites suites) <- toList commits
    , CheckSuite {checkSuiteStatus=status, checkSuiteConclusion=conclusion, checkSuiteRuns=checkRuns, checkSuiteRun=Just (RunRef (Just runId) updatedAt)} <- suites
    ]
  where
    -- GraphQL spells its enums IN_PROGRESS where REST says in_progress
    lowercaseEnum = T.toLower

    jobsOf checkRuns = M.fromList [
      (jobId, JobStatus (lowercaseEnum status) (lowercaseEnum <$> conclusion))
      | CheckRun {checkRunId=Just jobId, checkRunStatus=status, checkRunConclusion=conclusion} <- checkRuns
      ]

newtype CommitSuites = CommitSuites [CheckSuite]

instance FromJSON CommitSuites where
  parseJSON = withObject "Commit" $ \o -> do
    suites <- o .: "checkSuites"
    CommitSuites <$> (suites .: "nodes")

data CheckSuite = CheckSuite {
  checkSuiteStatus :: Text
  , checkSuiteConclusion :: Maybe Text
  , checkSuiteRun :: Maybe RunRef
  , checkSuiteRuns :: [CheckRun]
  }

instance FromJSON CheckSuite where
  parseJSON = withObject "CheckSuite" $ \o -> CheckSuite
    <$> (fromMaybe "" <$> o .:? "status")
    <*> o .:? "conclusion"
    <*> o .:? "workflowRun"
    <*> (maybe (pure []) (.: "nodes") =<< o .:? "checkRuns")

data CheckRun = CheckRun {
  checkRunId :: Maybe Int
  , checkRunStatus :: Text
  , checkRunConclusion :: Maybe Text
  }

instance FromJSON CheckRun where
  parseJSON = withObject "CheckRun" $ \o -> CheckRun
    <$> o .:? "databaseId"
    <*> (fromMaybe "" <$> o .:? "status")
    <*> o .:? "conclusion"

data RunRef = RunRef (Maybe Int) (Maybe UTCTime)

instance FromJSON RunRef where
  parseJSON = withObject "WorkflowRun" $ \o -> RunRef
    <$> o .:? "databaseId"
    <*> o .:? "updatedAt"
