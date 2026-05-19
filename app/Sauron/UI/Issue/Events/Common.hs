module Sauron.UI.Issue.Events.Common (
  wrapWidgets
  ) where

import Relude


-- | Greedily wrap sized widgets into lines that fit within the given width.
wrapWidgets :: Int -> [(Int, a)] -> [[(Int, a)]]
wrapWidgets _ [] = []
wrapWidgets maxW items =
  let (line, rest) = takeLine maxW items
  in line : wrapWidgets maxW rest
  where
    takeLine _ [] = ([], [])
    takeLine remaining (item@(w, _):xs)
      | w <= remaining = let (moreLine, leftover) = takeLine (remaining - w) xs
                         in (item : moreLine, leftover)
      -- Item doesn't fit but nothing else has been taken yet — take it anyway
      -- to guarantee progress. Without this, maxW <= 0 causes an infinite loop.
      | remaining == maxW = ([item], xs)
      | otherwise = ([], item : xs)
