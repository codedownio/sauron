{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

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
    expandNodes x@(MainListElemHeading {}) = [x]
    expandNodes x@(MainListElemRepo {..}) = execWriter $ do
      tell [x]
      when _toggled $ do
        tell (expandNodes _issuesChild)
        tell (expandNodes _workflowsChild)
    expandNodes x@(MainListElemPaginated {..}) = execWriter $ do
      tell [x]
      when _toggled $ tell _children

    expandNodes x@(MainListElemItem {}) = [x]


-- * Computing nth child in the presence of expanding

-- TODO: make this also return the sequence of parent nodes

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
nthChild n el@(MainListElemPaginated {..}) = readTVar _toggled >>= \case
  True -> (fmap ((el :|) . toList)) <$> (readTVar _children >>= nthChildList (n - 1))
  False -> pure $ Left (n - 1)
nthChild n el@(MainListElemRepo {..}) = readTVar _toggled >>= \case
  True -> do
    ic <- readTVar _issuesChild
    wc <- readTVar _workflowsChild
    (fmap ((el :|) . toList)) <$> nthChildList (n - 1) [ic, wc]
  False -> pure $ Left (n - 1)
nthChild n _ = pure $ Left (n - 1)
