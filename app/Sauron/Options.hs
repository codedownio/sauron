{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sauron.Options (
  CliArgs(..)
  , cliArgsParser
  , parseCliArgs

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

data ConfigRepo = ConfigRepoSingle {
  configRepoOwner :: Text
  , configRepoName :: Text
  } | ConfigRepoWildcard {
        configRepoOwner :: Text
      }
  deriving (Show)
instance FromJSON ConfigRepo where
  parseJSON (A.String x) = case T.splitOn "/" x of
    [owner, "*"] -> pure (ConfigRepoWildcard owner)
    [owner, name] -> pure (ConfigRepoSingle owner name)
    xs -> fail [i|Expected repo format to be "owner/name" or "owner/*". Got: "#{x}"|]
  parseJSON _ = fail "Failed to read IP"

data ConfigSection = ConfigSection {
  sectionDisplayName :: Maybe Text
  , sectionRepos :: [ConfigRepo]
  } deriving (Show)
$(deriveFromJSON toSnake1 ''ConfigSection)

data Config = Config {
  configSections :: Maybe [ConfigSection]
  } deriving (Show)
$(deriveFromJSON toSnake1 ''Config)
