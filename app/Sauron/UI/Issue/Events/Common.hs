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
      | otherwise = ([], item : xs)
