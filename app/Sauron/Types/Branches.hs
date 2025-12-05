{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Sauron.Types.Branches (
  GitHubBranchAuthor(..)
  , GitHubBranchInfo(..)
  , GitHubBranchesPayload(..)
  , GitHubBranchesResponse(..)
) where

import Data.Aeson.TH
import Data.Text
import GHC.Generics
import Relude
import Sauron.Aeson


data GitHubBranchAuthor = GitHubBranchAuthor {
  gitHubBranchAuthorLogin :: Text
  , gitHubBranchAuthorName :: Text
  , gitHubBranchAuthorAvatarUrl :: Text
  , gitHubBranchAuthorPath :: Text
  } deriving (Show, Eq, Generic)
deriveFromJSON toSnake4 ''GitHubBranchAuthor

data GitHubBranchInfo = GitHubBranchInfo {
  gitHubBranchInfoName :: Text
  , gitHubBranchInfoIsDefault :: Bool
  , gitHubBranchInfoMergeQueueEnabled :: Bool
  , gitHubBranchInfoPath :: Text
  , gitHubBranchInfoRulesetsPath :: Maybe Text
  , gitHubBranchInfoProtectedByBranchProtections :: Bool
  , gitHubBranchInfoAuthor :: GitHubBranchAuthor
  , gitHubBranchInfoAuthoredDate :: Text  -- ISO 8601 format
  , gitHubBranchInfoDeleteable :: Bool
  , gitHubBranchInfoDeleteProtected :: Bool
  , gitHubBranchInfoIsBeingRenamed :: Bool
  , gitHubBranchInfoRenameable :: Bool
  } deriving (Show, Eq, Generic)
deriveFromJSON toSnake4 ''GitHubBranchInfo

data GitHubBranchesPayload = GitHubBranchesPayload {
  gitHubBranchesPayloadCurrentPage :: Int
  , gitHubBranchesPayloadHasMore :: Bool
  , gitHubBranchesPayloadPerPage :: Int
  , gitHubBranchesPayloadBranches :: [GitHubBranchInfo]
  } deriving (Show, Eq, Generic)
deriveFromJSON toSnake4 ''GitHubBranchesPayload

data GitHubBranchesResponse = GitHubBranchesResponse {
  gitHubBranchesResponsePayload :: GitHubBranchesPayload
  , gitHubBranchesResponseTitle :: Text
  } deriving (Show, Eq, Generic)
deriveFromJSON toSnake4 ''GitHubBranchesResponse
