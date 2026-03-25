
module Sauron.Fetch.ParseWorkflowRunLogs (
  extractJobLogsFromZip
  , listZipEntryPaths
  , ZipJobLogs(..)
  ) where

import Codec.Archive.Zip (Entry(..), toArchive, fromEntry)
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import qualified Data.Text as T
import Relude
import Sauron.Fetch.ParseJobLogs (parseJobLogs)
import Sauron.Types (JobLogGroup(..))


-- | Result of extracting logs for a job from the zip.
data ZipJobLogs
  = ZipJobStepLogs [(Int, Text, [JobLogGroup])]
  -- ^ Per-step logs from "JobName/N_StepName.txt" entries, sorted by step number.
  | ZipJobFlatLogs [JobLogGroup]
  -- ^ Flat logs from "N_JobName.txt" (single combined file, needs timestamp-based splitting).
  deriving (Show)

-- | List all entry paths in a zip archive (for debugging).
listZipEntryPaths :: BL.ByteString -> [String]
listZipEntryPaths zipBytes =
  let archive = toArchive zipBytes
  in map eRelativePath (Zip.zEntries archive)

-- | Extract logs for a specific job from a workflow run logs zip archive.
--
-- The zip may contain:
--   "JobName/N_StepName.txt" — per-step log files (preferred, for multi-step jobs)
--   "N_JobName.txt"          — flat combined log file (fallback, for single-step jobs)
--   "JobName/system.txt"     — system metadata (ignored)
--
-- Returns 'ZipJobStepLogs' when per-step files are available, otherwise 'ZipJobFlatLogs'.
-- Returns Nothing if neither format is found.
extractJobLogsFromZip :: BL.ByteString -> Text -> Maybe ZipJobLogs
extractJobLogsFromZip zipBytes jobName =
  let archive = toArchive zipBytes
      allEntries = Zip.zEntries archive
      -- GitHub replaces invalid filename chars with '_' (on Linux, just \0 and '/')
      -- See: runner/src/Runner.Sdk/Util/IOUtil.cs ReplaceInvalidFileNameChars
      normalizedName = T.replace "/" "_" jobName
      jobDir = toString normalizedName <> "/"

      -- Per-step entries: "JobName/N_StepName.txt" (exclude "JobName/system.txt")
      stepEntries = filter (\e ->
        let p = eRelativePath e
        in jobDir `isPrefixOf` p
           && p /= jobDir <> "system.txt"
           && ".txt" `isSuffixOf` p
        ) allEntries

      parsedSteps = mapMaybe (parseStepEntry jobDir) stepEntries

      -- Flat entry: "N_JobName.txt"
      flatEntry = find (isFlatJobEntry normalizedName) allEntries

  in if not (null parsedSteps)
     then Just $ ZipJobStepLogs (sortOn (\(n, _, _) -> n) parsedSteps)
     else case flatEntry of
       Just entry ->
         let content = decodeUtf8 (BL.toStrict (fromEntry entry))
             logLines = T.splitOn "\n" content
         in Just $ ZipJobFlatLogs (parseJobLogs logLines)
       Nothing -> Nothing
  where
    parseStepEntry :: String -> Entry -> Maybe (Int, Text, [JobLogGroup])
    parseStepEntry prefix entry =
      let path = eRelativePath entry
          -- Strip the "JobName/" prefix to get "N_StepName.txt"
          fileName = toText (drop (length prefix) path)
      in case parseNumberedFileName fileName of
        Nothing -> Nothing
        Just (stepNum, stepName) ->
          let content = decodeUtf8 (BL.toStrict (fromEntry entry))
              logLines = T.splitOn "\n" content
              parsedLogs = parseJobLogs logLines
          in Just (stepNum, stepName, parsedLogs)

    isFlatJobEntry :: Text -> Entry -> Bool
    isFlatJobEntry name entry =
      let path = toText (eRelativePath entry)
      in case parseFlatJobFileName path of
        Just parsedName -> parsedName == name
        Nothing -> False

    -- Parse "N_StepName.txt" -> (N, "StepName")
    parseNumberedFileName :: Text -> Maybe (Int, Text)
    parseNumberedFileName fileName = do
      baseName <- T.stripSuffix ".txt" fileName
      -- Must not contain '/'
      guard (not ("/" `T.isInfixOf` baseName))
      case T.break (== '_') baseName of
        (numText, rest)
          | not (T.null rest)
          , Just n <- readMaybe (toString numText)
          -> Just (n, T.drop 1 rest)
        _ -> Nothing

    -- Parse "N_JobName.txt" -> "JobName" (top-level flat file)
    parseFlatJobFileName :: Text -> Maybe Text
    parseFlatJobFileName path = do
      baseName <- T.stripSuffix ".txt" path
      guard (not ("/" `T.isInfixOf` baseName))
      case T.break (== '_') baseName of
        (numText, rest)
          | not (T.null rest)
          , Just (_ :: Int) <- readMaybe (toString numText)
          -> Just (T.drop 1 rest)
        _ -> Nothing
