
module Sauron.Aeson (
  toSnake0
  , toSnake1
  , toSnake2
  , toSnake3
  , toSnake4
  , toSnake5
  ) where

import Data.Aeson as A
import Data.Char
import qualified Data.List as L
import Relude


baseOptions :: A.Options
baseOptions = A.defaultOptions { A.omitNothingFields = True }


toSnake0, toSnake1, toSnake2, toSnake3, toSnake4, toSnake5 :: A.Options
toSnake0 = baseOptions { A.fieldLabelModifier = toSnake . dropLeadingUnderscore }
toSnake1 = baseOptions { A.fieldLabelModifier = toSnakeAndDropFirstWord . dropLeadingUnderscore }
toSnake2 = baseOptions { A.fieldLabelModifier = toSnakeAndDropTwoWords . dropLeadingUnderscore }
toSnake3 = baseOptions { A.fieldLabelModifier = toSnakeAndDropThreeWords . dropLeadingUnderscore }
toSnake4 = baseOptions { A.fieldLabelModifier = toSnakeAndDropFourWords . dropLeadingUnderscore }
toSnake5 = baseOptions { A.fieldLabelModifier = toSnakeAndDropFiveWords . dropLeadingUnderscore }


toSnake :: String -> String
toSnake = fmap toLower . L.concat . underscores . splitR isUpper
  where
    underscores :: [String] -> [String]
    underscores [] = []
    underscores (h:t) = h : fmap ('_':) t

toSnakeAndDropFirstWord :: String -> String
toSnakeAndDropFirstWord = L.drop 1 . L.dropWhile (/= '_') . toSnake

toSnakeAndDropTwoWords :: String -> String
toSnakeAndDropTwoWords = L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . toSnake

toSnakeAndDropThreeWords :: String -> String
toSnakeAndDropThreeWords = L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . toSnake

toSnakeAndDropFourWords :: String -> String
toSnakeAndDropFourWords = L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . toSnake

toSnakeAndDropFiveWords :: String -> String
toSnakeAndDropFiveWords = L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . toSnake

splitR :: (Char -> Bool) -> String -> [String]
splitR _ [] = []
splitR p s =
  let
    go :: Char -> String -> [String]
    go m s' = case L.break p s' of
      (b', [])     -> [ m:b' ]
      (b', x:xs) -> ( m:b' ) : go x xs
  in case L.break p s of
    (b,  [])    -> [ b ]
    ([], h:t) -> go h t
    (b,  h:t) -> b : go h t

dropLeadingUnderscore :: [Char] -> [Char]
dropLeadingUnderscore ('_':xs) = xs
dropLeadingUnderscore xs = xs
