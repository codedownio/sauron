{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Event.Paging where

import Brick
import Brick.Widgets.List
import Control.Monad
import Data.Function
import qualified Data.Vector as Vec
import Lens.Micro
import Relude hiding (Down, pi)
import Sauron.Actions
import Sauron.Event.Helpers
import Sauron.Types


tryNavigatePage :: AppState -> (PageInfo -> PageInfo) -> EventM ClickableName AppState ()
tryNavigatePage s cb =
  withNthChildAndPaginationParent s $ \_fixedEl _el (SomeNode paginationEl, readPageInfo, writePageInfo) parents -> do
    didChange <- liftIO $ atomically $ do
      currentPageInfo <- readPageInfo
      let newPageInfo = cb currentPageInfo
      let hasChanged = newPageInfo /= currentPageInfo
      when hasChanged $ writePageInfo newPageInfo
      return hasChanged
    when didChange $ do
      -- Mark the pagination node selected
      expandedList <- gets (^. appMainList)
      forM_ (Vec.findIndex (\(SomeNode el) -> (_ident (getEntityData paginationEl) == _ident (getEntityData el))) (listElements expandedList)) $ \index ->
        modify (appMainList %~ listMoveTo index)

      refreshLine (s ^. appBaseContext) paginationEl parents

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
