{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Sauron.Expanding (
  getExpandedList
  , nthChildVector
  ) where

import Control.Monad.Writer
import qualified Data.Vector as V
import Relude
import Sauron.Types


getExpandedList :: V.Vector MainListElem -> V.Vector MainListElem
getExpandedList = V.fromList . concatMap expandNodes . V.toList
  where
    expandNodes :: MainListElem -> [MainListElem]
    expandNodes x@(SomeMainListElem item) = execWriter $ do
      tell [x]
      when (_toggled (getEntityData item)) $
        case item of
          PaginatedIssuesNode (EntityData {..}) -> expandTyped _children
          PaginatedPullsNode (EntityData {..}) -> expandTyped _children
          PaginatedWorkflowsNode (EntityData {..}) -> expandTyped _children
          SingleIssueNode (EntityData {..}) -> expandChildless _children
          SinglePullNode (EntityData {..}) -> expandChildless _children
          SingleWorkflowNode (EntityData {..}) -> expandTyped _children
          SingleJobNode (EntityData {..}) -> expandTyped _children
          JobLogGroupNode (EntityData {..}) -> expandTyped _children
          HeadingNode (EntityData {..}) -> expandWrapped _children
          RepoNode (EntityData {..}) -> forM_ _children $ \y -> tell (expandNodes y)

    expandTyped :: (Foldable t, MonadWriter [MainListElem] m) => t (MainListElem' Fixed a) -> m ()
    expandTyped xs = forM_ xs $ \y -> tell (expandNodes (SomeMainListElem y))

    expandWrapped :: (Foldable t, MonadWriter [MainListElem] m) => t MainListElem -> m ()
    expandWrapped xs = forM_ xs $ \y -> tell (expandNodes y)

    expandChildless :: Monad m => [()] -> m ()
    expandChildless _xs = return ()

-- * Computing nth child in the presence of expanding

nthChildVector :: Int -> V.Vector MainListElemVariable -> STM (Maybe (NonEmpty MainListElemVariable))
nthChildVector n elems = nthChildList n (V.toList elems) >>= \case
  Left _ -> pure Nothing
  Right x -> pure (Just x)

nthChildList :: Int -> [MainListElemVariable] -> STM (Either Int (NonEmpty MainListElemVariable))
nthChildList n (x:xs) = nthChild n x >>= \case
  Right els -> pure $ Right els
  Left n' -> nthChildList n' xs
nthChildList n [] = pure $ Left n

nthChild :: Int -> MainListElemVariable -> STM (Either Int (NonEmpty MainListElemVariable))
nthChild 0 el = pure $ Right (el :| [])
nthChild n _el@(SomeMainListElem (getEntityData -> (EntityData {..}))) = readTVar _toggled >>= \case
  True -> pure $ Left (n - 1) -- Simplified for now - expand later when needed
  False -> pure $ Left (n - 1)
