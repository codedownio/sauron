
module Sauron.Fetch.ParseJobLogs (
  parseJobLogs
  ) where

import qualified Data.Text as T
import Data.Time
import Relude
import Sauron.Types (JobLogGroup(..))

-- SAMPLE OUTPUT:
-- 2025-09-19T09:30:24.6153700Z Actions: asdfasdf
-- 2025-09-19T09:30:24.6152890Z ##[group]Outer group name
-- 2025-09-19T09:30:24.6153700Z Actions: write
-- 2025-09-19T09:30:24.6153950Z Attestations: write
-- 2025-09-19T09:30:24.6152890Z ##[group]Inner group name
-- 2025-09-19T09:30:24.6154190Z Checks: write
-- 2025-09-19T09:30:24.6154440Z Contents: write
-- 2025-09-19T09:30:24.6157280Z ##[endgroup]
-- 2025-09-19T09:30:24.6154700Z Deployments: write
-- 2025-09-19T09:30:24.6152890Z ##[group]Inner group name 2
-- 2025-09-19T09:30:24.6154940Z Discussions: write
-- 2025-09-19T09:30:24.6157280Z ##[endgroup]
-- 2025-09-19T09:30:24.6155170Z Issues: write
-- 2025-09-19T09:30:24.6157280Z ##[endgroup]
-- 2025-09-19T09:30:24.6155410Z Metadata: read
-- 2025-09-19T09:30:24.6155630Z Models: read
-- 2025-09-19T09:30:24.6152890Z ##[group]Outer group name 2
-- 2025-09-19T09:30:24.6155860Z Packages: write
-- 2025-09-19T09:30:24.6156090Z Pages: write


parseJobLogs :: [Text] -> [JobLogGroup]
parseJobLogs logLines = fst $ parseLines logLines []

parseLines :: [Text] -> [(UTCTime, Text)] -> ([JobLogGroup], [Text])
parseLines [] groupStack = (closeAllGroups groupStack, [])
parseLines inputLines groupStack = parseWithAccumulation inputLines groupStack []

parseWithAccumulation :: [Text] -> [(UTCTime, Text)] -> [(UTCTime, Text)] -> ([JobLogGroup], [Text])
parseWithAccumulation [] groupStack accumulatedLines = 
  let result = if null accumulatedLines 
               then []
               else [createJobLogLines accumulatedLines]
  in (result <> closeAllGroups groupStack, [])

parseWithAccumulation (line:rest) groupStack accumulatedLines
  | Just (timestamp, groupTitle) <- parseGroupStart line =
      let result = if null accumulatedLines 
                   then []
                   else [createJobLogLines accumulatedLines]
          (children, remainingLines) = parseWithAccumulation rest ((timestamp, groupTitle) : groupStack) []
      in case remainingLines of
           [] -> (result <> [JobLogGroup timestamp groupTitle children], [])
           _ -> let (siblings, finalLines) = parseWithAccumulation remainingLines groupStack []
                in (result <> [JobLogGroup timestamp groupTitle children] <> siblings, finalLines)
  
  | isGroupEnd line = 
      let result = if null accumulatedLines 
                   then []
                   else [createJobLogLines accumulatedLines]
      in (result, rest)
  
  | otherwise =
      let (timestamp, content) = parseTimestampAndContent line
      in parseWithAccumulation rest groupStack (accumulatedLines <> [(timestamp, content)])

createJobLogLines :: [(UTCTime, Text)] -> JobLogGroup
createJobLogLines [] = JobLogLines (UTCTime (fromGregorian 1970 1 1) 0) []
createJobLogLines ((timestamp, content):rest) = JobLogLines timestamp (content : map snd rest)

parseGroupStart :: Text -> Maybe (UTCTime, Text)
parseGroupStart line =
  let (timestamp, content) = parseTimestampAndContent line
  in case T.stripPrefix "##[group]" content of
       Just title -> Just (timestamp, T.strip title)
       Nothing -> Nothing

isGroupEnd :: Text -> Bool
isGroupEnd line = "##[endgroup]" `T.isInfixOf` (snd $ parseTimestampAndContent line)

parseTimestampAndContent :: Text -> (UTCTime, Text)
parseTimestampAndContent line =
  case T.break (== ' ') line of
    (timestampText, rest) | "T" `T.isInfixOf` timestampText && "Z" `T.isInfixOf` timestampText ->
      case T.uncons rest of
        Just (' ', content) -> 
          case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (toString timestampText) of
            Just timestamp -> (timestamp, content)
            Nothing -> (UTCTime (fromGregorian 1970 1 1) 0, line)  -- fallback
        _ -> (UTCTime (fromGregorian 1970 1 1) 0, line)  -- fallback
    _ -> (UTCTime (fromGregorian 1970 1 1) 0, line)  -- fallback


closeAllGroups :: [(UTCTime, Text)] -> [JobLogGroup]
closeAllGroups [] = []
closeAllGroups ((timestamp, title):rest) = [JobLogGroup timestamp title (closeAllGroups rest)]
