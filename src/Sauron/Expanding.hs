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


getExpandedList :: V.Vector (SomeNode Fixed) -> V.Vector (SomeNode Fixed)
getExpandedList = V.fromList . concatMap expandNodes . V.toList
  where
    expandNodes :: (SomeNode Fixed) -> [(SomeNode Fixed)]
    expandNodes x@(SomeNode item) = execWriter $ do
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
      Foldable t, MonadWriter [SomeNode Fixed] m, Eq (Node Fixed a), Typeable a
      ) => t (Node Fixed a) -> m ()
    expandTyped xs = forM_ xs $ \y -> tell (expandNodes (SomeNode y))

    expandWrapped :: (Foldable t, MonadWriter [SomeNode Fixed] m) => t (SomeNode Fixed) -> m ()
    expandWrapped xs = forM_ xs $ \y -> tell (expandNodes y)

    expandChildless :: Monad m => [()] -> m ()
    expandChildless _xs = return ()

-- * Computing nth child in the presence of expanding

nthChildVector :: Int -> V.Vector (SomeNode Variable) -> STM (Maybe (NonEmpty (SomeNode Variable)))
nthChildVector n elems = nthChildList n (V.toList elems) >>= \case
  Left _ -> pure Nothing
  Right x -> pure (Just x)

nthChildList :: Int -> [SomeNode Variable] -> STM (Either Int (NonEmpty (SomeNode Variable)))
nthChildList n (x:xs) = nthChild n x >>= \case
  Right els -> pure $ Right els
  Left n' -> nthChildList n' xs
nthChildList n [] = pure $ Left n

nthChild :: Int -> SomeNode Variable -> STM (Either Int (NonEmpty (SomeNode Variable)))
nthChild 0 el = pure $ Right (el :| [])
nthChild n el@(SomeNode item@(getEntityData -> (EntityData {..}))) = readTVar _toggled >>= \case
  True -> do
    wrappedChildren <- getExistentialChildrenWrapped item
    (fmap ((el :|) . toList)) <$> (nthChildList (n - 1) wrappedChildren)
  False -> pure $ Left (n - 1)
