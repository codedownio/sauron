-- | Word-level diffing of a removed line against the added line that replaced it, so a
-- changed line can point at the words that actually changed.
module Sauron.UI.Diff.Words (
  DiffSegment(..)
  , wordDiffSegments
  ) where

import Data.Char (isAlphaNum, isSpace)
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude


-- | A run of a line, flagged with whether the word diff considers it changed.
data DiffSegment = DiffSegment {
  diffSegmentText :: Text
  , diffSegmentChanged :: Bool
  } deriving (Eq, Show)

-- | Split a removed line and the added line that replaced it into runs of unchanged and
-- changed text, the way GitHub boxes the individual words that differ within a line.
-- Returns Nothing when the two lines have nothing but whitespace in common, where
-- boxing all of both lines would just be noise.
wordDiffSegments :: Text -> Text -> Maybe ([DiffSegment], [DiffSegment])
wordDiffSegments removed added
  | removed == added = Nothing
  | everythingChanged removedSegments && everythingChanged addedSegments = Nothing
  | otherwise = Just (removedSegments, addedSegments)
  where
    removedTokens = tokenize removed
    addedTokens = tokenize added
    (removedMatches, addedMatches) = commonTokens removedTokens addedTokens
    removedSegments = toSegments removedTokens removedMatches
    addedSegments = toSegments addedTokens addedMatches

    everythingChanged = all diffSegmentChanged . filter (not . T.all isSpace . diffSegmentText)

-- | Collapse a token list into runs of the same changed-ness
toSegments :: [Text] -> [Bool] -> [DiffSegment]
toSegments tokens matches = [
  DiffSegment (mconcat (map fst run)) (not (all snd run))
  | run <- L.groupBy ((==) `on` snd) (zip tokens matches)
  ]

data CharClass = WordChar | SpaceChar | OtherChar
  deriving (Eq)

-- | Split a line into word-diff tokens: runs of word characters, runs of whitespace, and
-- a token per character for everything else, so that a word is one token and punctuation
-- is fine-grained.
tokenize :: Text -> [Text]
tokenize = T.groupBy sameClass
  where
    sameClass a b = charClass a == charClass b && charClass a /= OtherChar

    charClass c
      | isAlphaNum c || c == '_' = WordChar
      | isSpace c = SpaceChar
      | otherwise = OtherChar

-- | Which tokens on each side belong to a longest common subsequence of the two token
-- lists. Common leading and trailing tokens are paired up directly, which is both
-- cheaper and a better pairing than the subsequence search would find.
commonTokens :: [Text] -> [Text] -> ([Bool], [Bool])
commonTokens removed added = (
  prefixMatches <> removedMiddle <> suffixMatches
  , prefixMatches <> addedMiddle <> suffixMatches
  )
  where
    prefixLength = length $ takeWhile id $ zipWith (==) removed added
    removedRest = drop prefixLength removed
    addedRest = drop prefixLength added
    suffixLength = length $ takeWhile id $ zipWith (==) (reverse removedRest) (reverse addedRest)

    prefixMatches = replicate prefixLength True
    suffixMatches = replicate suffixLength True

    (removedMiddle, addedMiddle) = middleMatches
      (take (length removedRest - suffixLength) removedRest)
      (take (length addedRest - suffixLength) addedRest)

-- | How many tokens a line may have left after trimming the common prefix and suffix
-- before we stop looking for a subsequence. The search is quadratic and the patch of a
-- minified file isn't worth the work, so past this the whole middle counts as changed.
middleTokenLimit :: Int
middleTokenLimit = 150

middleMatches :: [Text] -> [Text] -> ([Bool], [Bool])
middleMatches removed added
  | length removed > middleTokenLimit || length added > middleTokenLimit =
      (False <$ removed, False <$ added)
  | otherwise = lcsMatches (V.fromList removed) (V.fromList added)

-- | Mark the tokens on each side that belong to a longest common subsequence
lcsMatches :: V.Vector Text -> V.Vector Text -> ([Bool], [Bool])
lcsMatches removed added = (
  matchFlags (V.length removed) (map fst matches)
  , matchFlags (V.length added) (map snd matches)
  )
  where
    -- lcsLength i j is the length of the longest common subsequence of the first i
    -- tokens of the removed line and the first j of the added one
    rows = V.scanl' nextRow (V.replicate (V.length added + 1) 0) removed
    nextRow previous token = V.scanl' step 0 (V.zip3 added (V.init previous) (V.tail previous))
      where step left (other, diagonal, up) = if token == other then diagonal + 1 else max up left

    lcsLength i j = rows V.! i V.! j

    matches = walk (V.length removed) (V.length added)
    walk 0 _ = []
    walk _ 0 = []
    walk i j
      | removed V.! (i - 1) == added V.! (j - 1) = (i - 1, j - 1) : walk (i - 1) (j - 1)
      | lcsLength (i - 1) j >= lcsLength i (j - 1) = walk (i - 1) j
      | otherwise = walk i (j - 1)

    matchFlags len matched = [Set.member ix matchedSet | ix <- [0 .. len - 1]]
      where matchedSet = Set.fromList matched
