
module Sauron.UI.BottomBar (
  bottomBar
  ) where

import Brick
import Brick.Widgets.List
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import GitHub hiding (Status)
import Lens.Micro hiding (ix)
import Relude
import Sauron.Types


bottomBar :: AppState -> Widget n
bottomBar s = Widget Greedy Fixed $ do
  _c <- getContext
  case listSelectedElement (s ^. appMainList) of
    Nothing -> render $ hBox [str ""]

    Just (_, MainListElemHeading {}) -> render $ hBox [str ""]

    Just (_, MainListElemRepo {_repo=(Fetched r)}) -> render $ hBox [str (T.unpack (T.intercalate ", " phrases))]
      where
        issuesPhrase = case repoOpenIssuesCount r of
          0 -> Nothing
          1 -> Just [i|1 issue|]
          n -> Just [i|#{n} issues|]

        -- prsPhrase = case ? r of
        --   0 -> Nothing
        --   1 -> Just [i|1 PR|]
        --   n -> Just [i|#{n} PRs|]

        phrases = catMaybes [issuesPhrase]
    Just (_, MainListElemRepo {_repo=_}) -> render $ hBox [str ""]

    Just (_, MainListElemItem {}) -> render $ hBox [str ""]
