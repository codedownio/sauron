{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Sauron.Options (
  CliArgs(..)
  , cliArgsParser
  , parseCliArgs

  , RepoSettings(..)
  , RepoSettingsResult(..)

  , PeriodSpec(..)

  , Config(..)
  , ConfigSection(..)
  , ConfigRepo(..)
  ) where

import Data.Aeson as A
import Data.Aeson.TH
import Data.String.Interpolate
import Data.Text as T
import Options.Applicative
import Relude
import Sauron.Aeson


-- * CLI

data CliArgs = CliArgs {
  cliOAuthToken :: Maybe Text
  , cliConfigFile :: Maybe FilePath
  , cliConcurrentGithubApiLimit :: Int
  } deriving (Show)

cliArgsParser :: Parser CliArgs
cliArgsParser = CliArgs
  <$> optional (strOption (long "token" <> help "OAuth token to auth to GitHub" <> metavar "STRING"))
  <*> optional (strOption (long "config" <> short 'c' <> help "Config file path" <> metavar "STRING"))
  <*> option auto (long "github-concurrent-requests" <> short 'r' <> showDefault <> help "Maximum number of concurrent requests to GitHub" <> value 10 <> metavar "INT")

parseCliArgs :: IO CliArgs
parseCliArgs = execParser opts
  where
    opts = info (cliArgsParser <**> helper) (
      fullDesc
      <> progDesc "A dark lord's-eye view of your GitHub repos"
      <> header "hello - a test for optparse-applicative"
      )

-- * Config file

newtype PeriodSpec = PeriodSpec Int
  deriving (Show, Eq)
instance FromJSON PeriodSpec where
  parseJSON _ = undefined

data RepoSettings = RepoSettings {
  repoSettingsCheckPeriod :: Maybe PeriodSpec
  } deriving (Show, Eq)
$(deriveFromJSON toSnake2 ''RepoSettings)

data RepoSettingsResult =
  NoSettings
  | HasSettings RepoSettings
  | ParseError Text
  deriving (Show, Eq)

data ConfigRepo = ConfigRepoSingle {
  configRepoOwner :: Text
  , configRepoName :: Text
  , configRepoSettings :: RepoSettingsResult
  } | ConfigRepoWildcard {
        configRepoOwner :: Text
      }
  deriving (Show, Eq)
instance FromJSON ConfigRepo where
  parseJSON (A.String x) = case T.splitOn "/" x of
    [owner, "*"] -> pure (ConfigRepoWildcard owner)
    [owner, name] -> pure (ConfigRepoSingle owner name NoSettings)
    _ -> fail [i|Expected repo format to be "owner/name" or "owner/*". Got: "#{x}"|]
  parseJSON (A.Object obj@(aesonLookup "name" -> Just (A.String x))) = case T.splitOn "/" x of
    [owner, "*"] -> pure (ConfigRepoWildcard owner)
    [owner, name] -> pure (ConfigRepoSingle owner name settingsResult)
      where
        settingsResult = case aesonLookup "settings" obj of
          Nothing -> NoSettings
          Just settingsObj@(A.Object _) -> case A.fromJSON settingsObj of
            Error err -> ParseError [i|Error parsing settings: #{err}|]
            A.Success y -> HasSettings y
          Just y -> ParseError [i|Error parsing settings: expected object but got '#{A.encode y}'|]
    _ -> fail [i|Expected repo format to be "owner/name" or "owner/*". Got: "#{x}"|]

  parseJSON _ = fail "Failed to read repo"

data ConfigSection = ConfigSection {
  sectionDisplayName :: Maybe Text
  , sectionRepos :: [ConfigRepo]
  } deriving (Show)
$(deriveFromJSON toSnake1 ''ConfigSection)

data Config = Config {
  configSettings :: Maybe RepoSettings
  , configSections :: Maybe [ConfigSection]
  } deriving (Show)
$(deriveFromJSON toSnake1 ''Config)
