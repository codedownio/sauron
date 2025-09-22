{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
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
    expandNodes x@(SomeMainListElem (getEntityData -> (EntityData {}))) = execWriter $ do
      tell [x]
      -- Note: We can't directly access _children here since it's wrapped in TVar for Variable types
      -- This function appears to be designed for Fixed types, so we'll keep it simple for now
      pure ()

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
