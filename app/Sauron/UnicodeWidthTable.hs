{-# LANGUAGE LambdaCase #-}

module Sauron.UnicodeWidthTable (
  WidthTableMode(..)
  , buildAndSaveWidthTable
  , loadWidthTable
  , widthTablePath
  ) where

import Data.Char (generalCategory, GeneralCategory(..))
import Data.String.Interpolate
import Graphics.Vty.UnicodeWidthTable.IO (readUnicodeWidthTable, writeUnicodeWidthTable)
import Graphics.Vty.UnicodeWidthTable.Install (installUnicodeWidthTable)
import Graphics.Vty.UnicodeWidthTable.Types (UnicodeWidthTable(..), WidthTableRange(..))
import Relude
import Sauron.OAuth (getConfigDir)
import System.Console.ANSI (getCursorPosition)
import System.FilePath ((</>), takeDirectory)
import System.IO (hIsTerminalDevice, hPutStrLn)
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)
import UnliftIO.Exception (bracket_)


-- | Character ranges where terminals commonly disagree with the built-in
-- wcwidth table. Characters outside these ranges fall back to vty's built-in.
emojiAndSymbolRanges :: [(Char, Char)]
emojiAndSymbolRanges =
  [ ('\x2000', '\x206F')   -- General Punctuation
  , ('\x2190', '\x21FF')   -- Arrows
  , ('\x2300', '\x23FF')   -- Miscellaneous Technical (⌛, ⌚, etc.)
  , ('\x2460', '\x24FF')   -- Enclosed Alphanumerics
  , ('\x2500', '\x257F')   -- Box Drawing
  , ('\x2580', '\x259F')   -- Block Elements
  , ('\x25A0', '\x25FF')   -- Geometric Shapes
  , ('\x2600', '\x27BF')   -- Misc Symbols + Dingbats (☀, ✓, ✗, etc.)
  , ('\x2900', '\x297F')   -- Supplemental Arrows-B
  , ('\x2B00', '\x2BFF')   -- Misc Symbols and Arrows (⭐, etc.)
  , ('\x3000', '\x303F')   -- CJK Symbols and Punctuation
  , ('\xFE00', '\xFE0F')   -- Variation Selectors
  , ('\x1F000', '\x1F02F') -- Mahjong Tiles
  , ('\x1F0A0', '\x1F0FF') -- Playing Cards
  , ('\x1F100', '\x1F1FF') -- Enclosed Alphanumeric Supplement + Regional Indicators
  , ('\x1F200', '\x1F2FF') -- Enclosed Ideographic Supplement
  , ('\x1F300', '\x1F5FF') -- Misc Symbols and Pictographs
  , ('\x1F600', '\x1F64F') -- Emoticons
  , ('\x1F680', '\x1F6FF') -- Transport and Map Symbols
  , ('\x1F700', '\x1F77F') -- Alchemical Symbols
  , ('\x1F780', '\x1F7FF') -- Geometric Shapes Extended
  , ('\x1F800', '\x1F8FF') -- Supplemental Arrows-C
  , ('\x1F900', '\x1F9FF') -- Supplemental Symbols and Pictographs
  , ('\x1FA00', '\x1FA6F') -- Chess Symbols
  , ('\x1FA70', '\x1FAFF') -- Symbols and Pictographs Extended-A
  , ('\x1FB00', '\x1FBFF') -- Symbols for Legacy Computing
  ]

charsToQuery :: [Char]
charsToQuery = filter shouldConsider $ concatMap (\(lo, hi) -> [lo..hi]) emojiAndSymbolRanges
  where
    shouldConsider c = case generalCategory c of
      Control -> False
      NotAssigned -> False
      Surrogate -> False
      _ -> True

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

-- | Run-length encode a sorted list of (codepoint, width) pairs into ranges.
mkRanges :: [(Word32, Word8)] -> [WidthTableRange]
mkRanges = go Nothing []
  where
    go Nothing finished [] = finished
    go (Just r) finished [] = r : finished
    go Nothing finished ((c, w):rest) =
      go (Just $ WidthTableRange c 1 w) finished rest
    go (Just r@(WidthTableRange start sz prevW)) finished ((c, w):rest)
      | c == start + sz && w == prevW =
          go (Just $ WidthTableRange start (sz + 1) prevW) finished rest
      | otherwise =
          go (Just $ WidthTableRange c 1 w) (r : finished) rest

data WidthTableMode = EmojiOnly | FullUnicode

buildWidthTable :: WidthTableMode -> IO UnicodeWidthTable
buildWidthTable mode = do
  let chars = case mode of
        EmojiOnly -> charsToQuery
        FullUnicode -> filter shouldConsider ['\0'..'\xe0000']
  pairs <- forM chars $ \c -> do
    w <- charWidth c
    pure (fromIntegral (fromEnum c) :: Word32, fromIntegral w :: Word8)
  pure $ UnicodeWidthTable { unicodeWidthTableRanges = reverse $ mkRanges pairs }
  where
    shouldConsider c = case generalCategory c of
      Control -> False
      NotAssigned -> False
      Surrogate -> False
      _ -> True

buildAndSaveWidthTable :: WidthTableMode -> IO ()
buildAndSaveWidthTable mode = do
  isTTY <- hIsTerminalDevice stdout
  unless isTTY $
    fail "Must be run in an interactive terminal"

  path <- widthTablePath
  createDirectoryIfMissing True (takeDirectory path)

  oldBuffering <- hGetBuffering stdout
  let setRaw = hSetBuffering stdout NoBuffering >> hSetBuffering stdin NoBuffering
  let restore = hSetBuffering stdout oldBuffering

  let label = case mode of
        EmojiOnly -> [i|emoji/symbols|]
        FullUnicode -> [i|full Unicode (this may take a while)|] :: String
  hPutStrLn stderr [i|Building #{label} width table...|]
  bracket_ setRaw restore $ do
    table <- buildWidthTable mode
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
          hPutStrLn stderr [i|Loaded character width table from #{path}|]
          installUnicodeWidthTable table
          pure True
