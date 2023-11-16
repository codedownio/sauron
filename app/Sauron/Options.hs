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
import Data.Text
import Options.Applicative
import Relude
import Sauron.Aeson


data CliArgs = CliArgs {
  cliOAuthToken :: Maybe Text
  , cliConfigFile :: Maybe FilePath
  } deriving (Show)

cliArgsParser :: Parser CliArgs
cliArgsParser = CliArgs
  <$> optional (strOption (long "token" <> help "OAuth token to auth to GitHub" <> metavar "STRING"))
  <*> optional (strOption (long "config" <> short 'c' <> help "Config file path" <> metavar "STRING"))

parseCliArgs :: IO CliArgs
parseCliArgs = execParser opts
  where
    opts = info (cliArgsParser <**> helper) (
      fullDesc
      <> progDesc "A dark lord's-eye view of your GitHub repos"
      <> header "hello - a test for optparse-applicative"
      )


newtype ConfigRepo = ConfigRepo Text
  deriving (Show, ToJSON, FromJSON)

data ConfigSection = ConfigSection {
  sectionDisplayName :: Maybe Text
  , sectionRepos :: [ConfigRepo]
  } deriving (Show)
$(deriveJSON toSnake1 ''ConfigSection)

data Config = Config {
  configSections :: Maybe [ConfigSection]
  } deriving (Show)
$(deriveJSON toSnake1 ''Config)
