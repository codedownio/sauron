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


-- * Sorting

-- | Sort key type that provides a uniform Ord instance across sort modes.
-- Sorts lexicographically: (failure grouping, runtime desc, name)
type JobSortKey = (Int, Data.Ord.Down NominalDiffTime, Text)

jobSortKey :: WorkflowJobSortBy -> Maybe Job -> JobSortKey
jobSortKey workflowJobSortBy mj = case workflowJobSortBy of
  SortJobsByName -> (0, Data.Ord.Down 0, name)
  SortJobsByRuntime -> (0, Data.Ord.Down runtime, name)
  SortJobsByFailures -> (if isFailed then 0 else 1, Data.Ord.Down 0, name)
  where
    name = maybe "" (untagName . jobName) mj
    runtime = case mj of
      Just j -> maybe 999999 (\c -> diffUTCTime c (jobStartedAt j)) (jobCompletedAt j)
      Nothing -> 0
    isFailed = case mj of
      Just j -> fromMaybe (jobStatus j) (jobConclusion j) `elem` ["failure", "timed_out"]
      Nothing -> False

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
