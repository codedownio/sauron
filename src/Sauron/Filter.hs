
module Sauron.Filter where

import Sauron.Types

-- import qualified Data.Vector as V


updateFilteredTree :: AppState -> AppState
updateFilteredTree s = s
  -- & appMainList %~ listReplace elems (listSelected $ s ^. appMainList)
  -- where filteredTree = filterRunTree (s ^. appVisibilityThreshold) (s ^. appRunTree)
  --       elems :: V.Vector MainListElem = V.fromList $ concatMap treeToList (zip filteredTree (s ^. appRunTreeBase))
