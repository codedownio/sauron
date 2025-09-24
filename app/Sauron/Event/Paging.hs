{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Event.Paging where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import Lens.Micro
import Relude hiding (Down, pi)
import Sauron.Actions
import Sauron.Event.Helpers
import Sauron.Types
import UnliftIO.STM (stateTVar)


tryNavigatePage :: MonadIO m => AppState -> (PageInfo -> PageInfo) -> m ()
tryNavigatePage s cb =
  withNthChildAndPaginationParent s $ \_fixedEl _el (SomeNode paginationEl, pageInfo) parents -> do
    didChange <- atomically $ stateTVar pageInfo $ \pi ->
      let pi' = cb pi in (pi' /= pi, pi')
    when didChange $
      refresh (s ^. appBaseContext) paginationEl parents

goNextPage :: PageInfo -> PageInfo
goNextPage pi@(PageInfo {..}) = pi {
  pageInfoCurrentPage = currentPage'
  , pageInfoNextPage = if
      | not didMove -> pageInfoNextPage
      | otherwise -> case pageInfoLastPage of
          Nothing -> Nothing
          Just lastPage -> Just (min lastPage (pageInfoCurrentPage + 2))
  , pageInfoPrevPage = if
      | not didMove -> pageInfoPrevPage
      | otherwise -> Just pageInfoCurrentPage
  }
  where
    currentPage' = case pageInfoNextPage of
      Just nextPage -> nextPage
      Nothing -> case pageInfoLastPage of
        Nothing -> pageInfoCurrentPage
        Just lastPage -> min lastPage (pageInfoCurrentPage + 1)

    didMove = currentPage' > pageInfoCurrentPage


goPrevPage :: PageInfo -> PageInfo
goPrevPage pi@(PageInfo {..}) = pi {
  pageInfoCurrentPage = currentPage'
  , pageInfoNextPage = if
      | not didMove -> pageInfoNextPage
      | otherwise -> Just pageInfoCurrentPage
  , pageInfoPrevPage = if
      | not didMove -> pageInfoPrevPage
      | otherwise -> case pageInfoFirstPage of
          Nothing -> Nothing
          Just firstPage -> Just (max firstPage (pageInfoCurrentPage - 2))
  , pageInfoLastPage = if
      | not didMove -> pageInfoLastPage
      | otherwise -> Just $ case pageInfoLastPage of
          Nothing -> pageInfoCurrentPage
          Just lastPage -> lastPage
  }
  where
    currentPage' = case pageInfoPrevPage of
      Just prevPage -> prevPage
      Nothing -> case pageInfoFirstPage of
        Nothing -> pageInfoCurrentPage
        Just firstPage -> max firstPage (pageInfoCurrentPage - 1)

    didMove = currentPage' < pageInfoCurrentPage

goFirstPage :: PageInfo -> PageInfo
goFirstPage pi@(PageInfo {..}) = pi {
  pageInfoCurrentPage = fromMaybe 1 pageInfoFirstPage
  , pageInfoPrevPage = Nothing
  , pageInfoNextPage = case pageInfoLastPage of
      Just lastPage -> Just $ min 2 lastPage
      Nothing -> if
        | pageInfoCurrentPage >= 2 -> Just 2
        | otherwise -> Just 1
  }

goLastPage :: PageInfo -> PageInfo
goLastPage pi@(PageInfo {pageInfoLastPage=(Just lastPage)}) = pi {
  pageInfoCurrentPage = lastPage
  , pageInfoNextPage = Nothing
  , pageInfoPrevPage = Just $ max 1 lastPage
  , pageInfoFirstPage = Just 1
  }
goLastPage pi@(PageInfo {pageInfoLastPage=Nothing}) = pi
