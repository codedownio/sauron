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


getExpandedList :: V.Vector (SomeMainListElem Fixed) -> V.Vector (SomeMainListElem Fixed)
getExpandedList = V.fromList . concatMap expandNodes . V.toList
  where
    expandNodes :: (SomeMainListElem Fixed) -> [(SomeMainListElem Fixed)]
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

    expandTyped :: (
      Foldable t, MonadWriter [SomeMainListElem Fixed] m, Eq (MainListElem' Fixed a), Typeable a
      ) => t (MainListElem' Fixed a) -> m ()
    expandTyped xs = forM_ xs $ \y -> tell (expandNodes (SomeMainListElem y))

    expandWrapped :: (Foldable t, MonadWriter [SomeMainListElem Fixed] m) => t (SomeMainListElem Fixed) -> m ()
    expandWrapped xs = forM_ xs $ \y -> tell (expandNodes y)

    expandChildless :: Monad m => [()] -> m ()
    expandChildless _xs = return ()

-- * Computing nth child in the presence of expanding

nthChildVector :: Int -> V.Vector (SomeMainListElem Variable) -> STM (Maybe (NonEmpty (SomeMainListElem Variable)))
nthChildVector n elems = nthChildList n (V.toList elems) >>= \case
  Left _ -> pure Nothing
  Right x -> pure (Just x)

nthChildList :: Int -> [SomeMainListElem Variable] -> STM (Either Int (NonEmpty (SomeMainListElem Variable)))
nthChildList n (x:xs) = nthChild n x >>= \case
  Right els -> pure $ Right els
  Left n' -> nthChildList n' xs
nthChildList n [] = pure $ Left n

nthChild :: Int -> SomeMainListElem Variable -> STM (Either Int (NonEmpty (SomeMainListElem Variable)))
nthChild 0 el = pure $ Right (el :| [])
nthChild n _el@(SomeMainListElem (getEntityData -> (EntityData {..}))) = readTVar _toggled >>= \case
  True -> pure $ Left (n - 1) -- Simplified for now - expand later when needed
  False -> pure $ Left (n - 1)
