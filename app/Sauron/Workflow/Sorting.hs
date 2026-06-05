{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Workflow.Sorting where

import qualified Data.List as DL
import qualified Data.Ord
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import GitHub
import Relude
import Sauron.Types
import Sauron.UI.Statuses (chooseWorkflowStatus)


-- * Sorting

-- | Sort key type that provides a uniform Ord instance across sort modes.
-- Sorts lexicographically: (status grouping, runtime desc, name)
type JobSortKey = (Int, Data.Ord.Down NominalDiffTime, Text)

-- | Rank for "Sort by failures" mode, bubbling more interesting statuses to the top:
-- failed -> running -> pending -> cancelled/neutral/unknown -> success.
workflowStatusRank :: WorkflowStatus -> Int
workflowStatusRank WorkflowFailed = 0
workflowStatusRank WorkflowRunning = 1
workflowStatusRank WorkflowPending = 2
workflowStatusRank WorkflowCancelled = 3
workflowStatusRank WorkflowNeutral = 4
workflowStatusRank WorkflowUnknown = 5
workflowStatusRank WorkflowSuccess = 6

jobSortKey :: WorkflowJobSortBy -> Maybe Job -> JobSortKey
jobSortKey workflowJobSortBy mj = case workflowJobSortBy of
  SortJobsByName -> (0, Data.Ord.Down 0, name)
  -- Jobs without a runtime yet (still running / not started) sort to the bottom.
  SortJobsByRuntime -> case runtime of
    Just r -> (0, Data.Ord.Down r, name)
    Nothing -> (1, Data.Ord.Down 0, name)
  SortJobsByFailures -> (statusRank, Data.Ord.Down 0, name)
  where
    name = maybe "" (untagName . jobName) mj
    runtime = case mj of
      Just j -> (\c -> diffUTCTime c (jobStartedAt j)) <$> jobCompletedAt j
      Nothing -> Nothing
    statusRank = case mj of
      Just j -> workflowStatusRank (chooseWorkflowStatus (fromMaybe (jobStatus j) (jobConclusion j)))
      Nothing -> workflowStatusRank WorkflowUnknown

sortWorkflowJobsSTM :: WorkflowJobSortBy -> [Node Variable SingleJobT] -> STM [Node Variable SingleJobT]
sortWorkflowJobsSTM workflowJobSortBy jobs = do
  tagged <- forM jobs $ \job@(SingleJobNode (EntityData {_state})) -> do
    jobState <- readTVar _state
    let mj = fetchableCurrent (jnsJob jobState)
    let key = jobSortKey workflowJobSortBy mj
    return (key, job)
  return $ map snd $ DL.sortOn fst tagged

sortWorkflowJobsFixed :: WorkflowJobSortBy -> [Node Fixed SingleJobT] -> [Node Fixed SingleJobT]
sortWorkflowJobsFixed workflowJobSortBy = DL.sortOn getKey
  where
    getKey :: Node Fixed SingleJobT -> JobSortKey
    getKey (SingleJobNode (EntityData {_state=JobNodeState {jnsJob}})) =
      jobSortKey workflowJobSortBy (fetchableCurrent jnsJob)

-- * Pagination

paginateJobs :: Int -> [a] -> [a]
paginateJobs page xs
  | length xs <= workflowJobPageSize = xs
  | otherwise = take workflowJobPageSize $ drop ((page - 1) * workflowJobPageSize) xs

computeJobPageInfo :: Int -> Int -> PageInfo
computeJobPageInfo currentPage totalJobs =
  let totalPages = (totalJobs + workflowJobPageSize - 1) `div` workflowJobPageSize
      clampedPage = max 1 (min totalPages currentPage)
  in PageInfo {
    pageInfoCurrentPage = clampedPage
    , pageInfoFirstPage = if clampedPage > 1 then Just 1 else Nothing
    , pageInfoPrevPage = if clampedPage > 1 then Just (clampedPage - 1) else Nothing
    , pageInfoNextPage = if clampedPage < totalPages then Just (clampedPage + 1) else Nothing
    , pageInfoLastPage = if clampedPage < totalPages then Just totalPages else Nothing
    }
