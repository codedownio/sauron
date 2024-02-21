{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Expanding where

import Control.Monad.Writer
import qualified Data.Vector as V
import Relude
import Sauron.Types


getExpandedList :: V.Vector MainListElem -> V.Vector MainListElem
getExpandedList = V.fromList . concatMap expandNodes . V.toList
  where
    expandNodes x@(MainListElemHeading {}) = [x]
    expandNodes x@(MainListElemRepo {..}) = execWriter $ do
      tell [x]
      when _toggled $ do
        tell (expandNodes _issuesChild)
        tell (expandNodes _workflowsChild)
    expandNodes x@(MainListElemIssues {..}) = execWriter $ do
      tell [x]
      when _toggled $ tell _children
    expandNodes x@(MainListElemIssue {}) = [x]
    expandNodes x@(MainListElemWorkflows {..}) = execWriter $ do
      tell [x]
      when _toggled $ tell _children
    expandNodes x@(MainListElemWorkflow {}) = [x]


-- * Computing nth child in the presence of expanding

nthChildVector :: Int -> V.Vector MainListElemVariable -> STM (Maybe MainListElemVariable)
nthChildVector n elems = nthChildList n (V.toList elems) >>= \case
  Left _ -> pure Nothing
  Right x -> pure (Just x)

nthChildList :: Int -> [MainListElemVariable] -> STM (Either Int MainListElemVariable)
nthChildList n (x:xs) = nthChild n x >>= \case
  Right el -> pure $ Right el
  Left n' -> nthChildList n' xs
nthChildList n [] = pure $ Left n

nthChild :: Int -> MainListElemVariable -> STM (Either Int MainListElemVariable)
nthChild 0 el = pure $ Right el
nthChild n (MainListElemIssues {..}) = readTVar _toggled >>= \case
  True -> readTVar _children >>= nthChildList (n - 1)
  False -> pure $ Left (n - 1)
nthChild n (MainListElemWorkflows {..}) = readTVar _toggled >>= \case
  True -> readTVar _children >>= nthChildList (n - 1)
  False -> pure $ Left (n - 1)
nthChild n (MainListElemRepo {..}) = readTVar _toggled >>= \case
  True -> do
    ic <- readTVar _issuesChild
    wc <- readTVar _workflowsChild
    nthChildList (n - 1) [ic, wc]
  False -> pure $ Left (n - 1)
nthChild n _ = pure $ Left (n - 1)
