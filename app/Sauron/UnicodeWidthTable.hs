{-# LANGUAGE LambdaCase #-}

module Sauron.UnicodeWidthTable (
  buildAndSaveWidthTable
  , loadWidthTable
  , widthTablePath
  ) where

import Data.String.Interpolate
import Graphics.Vty.UnicodeWidthTable.IO (readUnicodeWidthTable, writeUnicodeWidthTable)
import Graphics.Vty.UnicodeWidthTable.Install (installUnicodeWidthTable)
import Graphics.Vty.UnicodeWidthTable.Query (buildUnicodeWidthTable, defaultUnicodeTableUpperBound)
import Relude
import Sauron.OAuth (getConfigDir)
import System.Console.ANSI (getCursorPosition)
import System.FilePath ((</>), takeDirectory)
import System.IO (hIsTerminalDevice, hPutStrLn)
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)
import UnliftIO.Exception (bracket_)


widthTablePath :: MonadIO m => m FilePath
widthTablePath = do
  configDir <- getConfigDir
  suffix <- lookupEnv "TERM" >>= \case
    Just term -> pure [i|width_table_#{term}.dat|]
    Nothing -> pure "width_table.dat"
  pure $ configDir </> suffix

charWidth :: Char -> IO Int
charWidth c = do
  putStr $ '\r':[c]
  getCursorPosition >>= \case
    Just (_, col) -> pure col
    Nothing -> pure 0

buildAndSaveWidthTable :: IO ()
buildAndSaveWidthTable = do
  isTTY <- hIsTerminalDevice stdout
  unless isTTY $
    fail "Must be run in an interactive terminal"

  path <- widthTablePath
  createDirectoryIfMissing True (takeDirectory path)

  oldBuffering <- hGetBuffering stdout
  let setRaw = hSetBuffering stdout NoBuffering >> hSetBuffering stdin NoBuffering
  let restore = hSetBuffering stdout oldBuffering

  hPutStrLn stderr [i|Building Unicode width table...|]
  bracket_ setRaw restore $ do
    table <- buildUnicodeWidthTable charWidth defaultUnicodeTableUpperBound
    writeUnicodeWidthTable path table
  hPutStrLn stderr [i|\nWritten to #{path}|]

loadWidthTable :: IO Bool
loadWidthTable = do
  path <- widthTablePath
  doesFileExist path >>= \case
    False -> pure False
    True ->
      readUnicodeWidthTable path >>= \case
        Left _err -> pure False
        Right table -> do
          installUnicodeWidthTable table
          pure True
