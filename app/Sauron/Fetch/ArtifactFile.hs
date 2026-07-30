{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Fetch a single named file out of a workflow run's artifacts using HTTP range
-- requests, without downloading whole artifacts. GitHub stores each artifact as a
-- zip; we range-read only the zip's central directory to see whether it contains the
-- target file, and if so range-read just that entry's bytes and inflate them.
module Sauron.Fetch.ArtifactFile (
  scanArtifactsForFile
  , fetchFileFromArtifact
  ) where

import qualified Codec.Compression.Zlib.Raw as Raw
import Control.Exception.Safe (MonadMask, throwString, try)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64)
import GitHub
import Network.HTTP.Client (Manager, Request(..), httpLbs, parseRequest, responseBody, responseHeaders)
import Network.URI (uriToString)
import Relude
import Sauron.Actions.Util (githubWithLogging, withGithubApiSemaphore)
import Sauron.Logging
import Sauron.Types
import UnliftIO.Async (pooledForConcurrentlyN)


-- | Scan a workflow run's non-expired artifacts and return those that contain @target@
-- (e.g. @"speedscope.json"@). Each artifact is probed by range-reading only its zip
-- central directory — no full download. Matching prefers an exact entry name, then a
-- basename match. Returns 'Left' with a human-readable reason if the artifact list itself
-- can't be fetched.
scanArtifactsForFile :: (
  MonadReader BaseContext m, MonadUnliftIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> Text -> m (Either Text [Artifact])
scanArtifactsForFile owner name runId target =
  withGithubApiSemaphore (githubWithLogging (artifactsForWorkflowRunR owner name runId FetchAll)) >>= \case
    Left err -> pure $ Left [i|Couldn't list artifacts: #{err}|]
    Right wtc -> do
      let artifacts = filter (not . artifactExpired) (V.toList (withTotalCountItems wtc))
      matches <- pooledForConcurrentlyN 8 artifacts $ \a ->
        (,) a <$> artifactContainsFile owner name a target
      pure $ Right [a | (a, True) <- matches]

-- | True if this one artifact's zip contains @target@ (central-directory probe only).
artifactContainsFile :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Artifact -> Text -> m Bool
artifactContainsFile owner name a target = withSignedUrl owner name a $ \case
  Nothing -> pure False
  Just (bc, url) -> liftIO (try (hasFileInZip (manager bc) url target)) >>= \case
    Left (e :: SomeException) -> do
      logError' bc [i|speedscope: error probing artifact #{artifactName a}: #{e}|]
      pure False
    Right has -> pure has

-- | Fetch @target@'s decompressed bytes from a specific artifact.
fetchFileFromArtifact :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Artifact -> Text -> m (Either Text BL.ByteString)
fetchFileFromArtifact owner name a target = withSignedUrl owner name a $ \case
  Nothing -> pure $ Left [i|Couldn't get a download URL for artifact "#{artifactName a}".|]
  Just (bc, url) -> liftIO (try (findInZip (manager bc) url target)) >>= \case
    Left (e :: SomeException) -> pure $ Left [i|Error reading artifact "#{artifactName a}": #{e}|]
    Right Nothing -> pure $ Left [i|Artifact "#{artifactName a}" no longer contains "#{target}".|]
    Right (Just bytes) -> pure $ Right bytes

-- | Resolve an artifact's short-lived signed blob URL and pass it (with the context) to the
-- continuation. Passes 'Nothing' if the redirect request fails.
withSignedUrl :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Artifact -> (Maybe (BaseContext, String) -> m r) -> m r
withSignedUrl owner name a k = do
  bc <- ask
  withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes (manager bc) (auth bc) (downloadArtifactR owner name (artifactId a))) >>= \case
    Left err -> do
      logError' bc [i|speedscope: couldn't get download URL for artifact #{artifactName a}: #{err}|]
      k Nothing
    Right resp -> k (Just (bc, uriToString id (responseBody resp) ""))

-- | True if the remote zip's central directory lists @target@ (no extraction).
hasFileInZip :: Manager -> String -> Text -> IO Bool
hasFileInZip mgr url target = do
  total <- remoteSize mgr url
  (cdOff, cdSize) <- locateCentralDir mgr url total
  cd <- rangeGet mgr url cdOff (cdOff + cdSize - 1)
  pure $ isJust (matchEntry target (parseCentralDir (BL.fromStrict cd)))

-- | Locate the target in a single remote zip and, if present, return its decompressed bytes.
findInZip :: Manager -> String -> Text -> IO (Maybe BL.ByteString)
findInZip mgr url target = do
  total <- remoteSize mgr url
  (cdOff, cdSize) <- locateCentralDir mgr url total
  cd <- rangeGet mgr url cdOff (cdOff + cdSize - 1)
  case matchEntry target (parseCentralDir (BL.fromStrict cd)) of
    Nothing -> pure Nothing
    Just e -> Just <$> extractEntry mgr url e

-- * Range reads against the signed blob URL

-- | Total object size, learned from a one-byte ranged GET (Content-Range: bytes 0-0/N).
-- Preferred over HEAD/suffix ranges, which Azure blob SAS URLs don't always honor.
remoteSize :: Manager -> String -> IO Word64
remoteSize mgr url = do
  req0 <- parseRequest url
  resp <- httpLbs (req0 { requestHeaders = ("Range", "bytes=0-0") : requestHeaders req0 }) mgr
  case L.lookup "Content-Range" (responseHeaders resp) of
    Just cr | Just n <- readMaybe (drop 1 (dropWhile (/= '/') (BS8.unpack cr))) -> pure n
    _ -> throwString "server did not return a usable Content-Range; ranged reads unsupported"

-- | Inclusive byte range [start, end].
rangeGet :: Manager -> String -> Word64 -> Word64 -> IO BS.ByteString
rangeGet mgr url start end = do
  req0 <- parseRequest url
  let hdr = BS8.pack [i|bytes=#{start}-#{end}|]
  BL.toStrict . responseBody <$> httpLbs (req0 { requestHeaders = ("Range", hdr) : requestHeaders req0 }) mgr

-- * Zip parsing

data CDEntry = CDEntry {
  cdName :: !BS.ByteString
  , cdMethod :: !Word16
  , cdCompSize :: !Word64
  , cdLocalOffset :: !Word64
  }

eocdSig, eocd64Sig, eocd64LocSig, cdSig :: Word32
eocdSig      = 0x06054b50  -- PK\5\6  end of central directory
eocd64Sig    = 0x06064b50  -- PK\6\6  zip64 end of central directory
eocd64LocSig = 0x07064b50  -- PK\6\7  zip64 EOCD locator
cdSig        = 0x02014b50  -- PK\1\2  central directory file header

-- | Max EOCD size = 22 fixed bytes + up to 65535 bytes of trailing comment.
eocdSearchLen :: Word64
eocdSearchLen = 22 + 65535

w16 :: BS.ByteString -> Int -> Word16
w16 bs o = fromIntegral (BS.index bs o) + fromIntegral (BS.index bs (o + 1)) * 256

w32 :: BS.ByteString -> Int -> Word32
w32 bs o = sum [fromIntegral (BS.index bs (o + i)) * (256 ^ i) | i <- [0 .. 3]]

lastIndexOf :: BS.ByteString -> BS.ByteString -> Maybe Int
lastIndexOf needle hay = go (BS.length hay - BS.length needle)
  where
    go i | i < 0 = Nothing
         | needle `BS.isPrefixOf` BS.drop i hay = Just i
         | otherwise = go (i - 1)

sigBytes :: Word32 -> BS.ByteString
sigBytes s = BS.pack [fromIntegral (s `div` (256 ^ i)) | i <- [0 .. 3 :: Int]]

-- | (offset, size) of the central directory, resolving zip64 when present.
locateCentralDir :: Manager -> String -> Word64 -> IO (Word64, Word64)
locateCentralDir mgr url total = do
  let tailStart = total - min eocdSearchLen total
  buf <- rangeGet mgr url tailStart (total - 1)
  eocdRel <- maybe (throwString "no EOCD record found (not a zip?)") pure (lastIndexOf (sigBytes eocdSig) buf)
  if w32 buf (eocdRel + 16) /= 0xFFFFFFFF && w32 buf (eocdRel + 12) /= 0xFFFFFFFF
    then pure (fromIntegral (w32 buf (eocdRel + 16)), fromIntegral (w32 buf (eocdRel + 12)))
    else locateCentralDir64 mgr url buf eocdRel

-- | Follow the zip64 EOCD locator (20 bytes before the EOCD) to the zip64 EOCD record,
-- which carries 8-byte offset/size fields.
locateCentralDir64 :: Manager -> String -> BS.ByteString -> Int -> IO (Word64, Word64)
locateCentralDir64 mgr url buf eocdRel = do
  let locRel = eocdRel - 20
  when (locRel < 0 || w32 buf locRel /= eocd64LocSig) $
    throwString "zip64 markers present but EOCD64 locator missing"
  let eocd64Off = runGet getWord64le (BL.fromStrict (BS.take 8 (BS.drop (locRel + 8) buf)))
  rec <- rangeGet mgr url eocd64Off (eocd64Off + 55)
  when (w32 rec 0 /= eocd64Sig) $ throwString "zip64 EOCD record signature mismatch"
  pure ( runGet getWord64le (BL.fromStrict (BS.take 8 (BS.drop 48 rec)))
       , runGet getWord64le (BL.fromStrict (BS.take 8 (BS.drop 40 rec))) )

parseCentralDir :: BL.ByteString -> [CDEntry]
parseCentralDir = runGet go
  where
    go = isEmpty >>= \case
      True -> pure []
      False -> do
        sig <- getWord32le
        if sig /= cdSig then pure [] else do
          skip 4                          -- version made by / needed
          _flags <- getWord16le
          method <- getWord16le
          skip 4                          -- mod time + date
          _crc <- getWord32le
          compSize32 <- getWord32le
          uncmp32 <- getWord32le
          fnLen <- fromIntegral <$> getWord16le
          exLen <- fromIntegral <$> getWord16le
          cmLen <- fromIntegral <$> getWord16le
          skip 8                          -- disk# + internal + external attrs
          loff32 <- getWord32le
          nm <- getByteString fnLen
          extra <- getByteString exLen
          _comment <- getByteString cmLen
          let (compSize, loff) = resolveZip64 extra uncmp32 compSize32 loff32
          (CDEntry nm method compSize loff :) <$> go

-- | Pull compressed size and local-header offset out of the zip64 extra field (id 0x0001)
-- for whichever 32-bit fields were set to 0xFFFFFFFF. The values appear in the fixed order:
-- uncompressed, compressed, offset, diskStart.
resolveZip64 :: BS.ByteString -> Word32 -> Word32 -> Word32 -> (Word64, Word64)
resolveZip64 extra uncmp32 comp32 loff32 =
  case findExtra 0x0001 extra of
    Nothing -> (fromIntegral comp32, fromIntegral loff32)
    Just dat -> runGet parse (BL.fromStrict dat)
  where
    parse = do
      _ <- if uncmp32 == 0xFFFFFFFF then getWord64le else pure 0
      c <- if comp32 == 0xFFFFFFFF then getWord64le else pure (fromIntegral comp32)
      o <- if loff32 == 0xFFFFFFFF then getWord64le else pure (fromIntegral loff32)
      pure (c, o)

findExtra :: Word16 -> BS.ByteString -> Maybe BS.ByteString
findExtra wanted = go
  where
    go bs
      | BS.length bs < 4 = Nothing
      | w16 bs 0 == wanted = Just (BS.take (fromIntegral (w16 bs 2)) (BS.drop 4 bs))
      | otherwise = go (BS.drop (4 + fromIntegral (w16 bs 2)) bs)

-- | Read the local header (whose filename/extra lengths can differ from the central
-- directory's), then range-read and inflate the entry's compressed bytes.
extractEntry :: Manager -> String -> CDEntry -> IO BL.ByteString
extractEntry mgr url e = do
  hdr <- rangeGet mgr url (cdLocalOffset e) (cdLocalOffset e + 29)
  let dataOff = cdLocalOffset e + 30 + fromIntegral (w16 hdr 26) + fromIntegral (w16 hdr 28)
  raw <- rangeGet mgr url dataOff (dataOff + cdCompSize e - 1)
  case cdMethod e of
    0 -> pure (BL.fromStrict raw)                  -- stored
    8 -> pure (Raw.decompress (BL.fromStrict raw)) -- deflate
    m -> throwString [i|unsupported zip compression method #{m}|]

-- | Prefer an exact entry-name match, then fall back to a basename match.
matchEntry :: Text -> [CDEntry] -> Maybe CDEntry
matchEntry target entries =
  find ((== tgt) . cdName) entries <|> find ((("/" <> tgt) `BS.isSuffixOf`) . cdName) entries
  where
    tgt = encodeUtf8 target
