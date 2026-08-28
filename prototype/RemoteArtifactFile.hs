{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Prototype: pull a single file out of a GitHub Actions artifact zip using HTTP
-- range requests, without downloading the whole artifact.
--
-- Run from the sauron project root so the github git-dep is on the package path:
--
--   GITHUB_TOKEN=$(gh auth token) \
--     stack runghc prototype/RemoteArtifactFile.hs OWNER REPO RUN_ID speedscope.json
--
-- It lists the run's artifacts, and for each one range-reads only the zip central
-- directory to check whether it contains the target. The first artifact that does
-- gets that one file extracted (only its compressed bytes are downloaded) and
-- written to ./OUT-<name>.
module Main (main) where

import Codec.Compression.Zlib.Raw qualified as Raw
import Control.Monad (forM_, when)
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Proxy qualified as P
import Data.String (fromString)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64)
import GitHub
import Network.HTTP.Client (newManager, responseBody)
import Network.HTTP.Conduit (tlsManagerSettings)
import Network.HTTP.Simple
import Network.URI (URI, uriToString)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.Process (readProcess)

--------------------------------------------------------------------------------
-- Zip structures we care about
--------------------------------------------------------------------------------

data CDEntry = CDEntry
  { cdName        :: !BS.ByteString
  , cdMethod      :: !Word16
  , cdCompSize    :: !Word64
  , cdLocalOffset :: !Word64
  } deriving (Show)

eocdSig, eocd64Sig, eocd64LocSig, cdSig :: Word32
eocdSig      = 0x06054b50  -- PK\5\6  end of central directory
eocd64Sig    = 0x06064b50  -- PK\6\6  zip64 end of central directory
eocd64LocSig = 0x07064b50  -- PK\6\7  zip64 EOCD locator
cdSig        = 0x02014b50  -- PK\1\2  central directory file header

-- Max EOCD size = 22 fixed bytes + up to 65535 bytes of trailing comment.
eocdSearchLen :: Int
eocdSearchLen = 22 + 65535

--------------------------------------------------------------------------------
-- HTTP range reads against the signed blob URL
--------------------------------------------------------------------------------

-- | Total object size, learned from a one-byte ranged GET (Content-Range: bytes 0-0/N).
-- Preferred over HEAD/suffix ranges, which Azure blob SAS URLs don't always honor.
remoteSize :: String -> IO Word64
remoteSize url = do
  req <- setRequestHeader "Range" ["bytes=0-0"] <$> parseRequest url
  resp <- httpLBS req
  case getResponseHeader "Content-Range" resp of
    (cr:_) -> pure (read (drop 1 (dropWhile (/= '/') (BS8.unpack cr))))
    []     -> die "server did not return Content-Range; ranged reads unsupported"

-- | Inclusive byte range [start, end].
rangeGet :: String -> Word64 -> Word64 -> IO BS.ByteString
rangeGet url start end = do
  let hdr = BS8.pack ("bytes=" <> show start <> "-" <> show end)
  req <- setRequestHeader "Range" [hdr] <$> parseRequest url
  BL.toStrict . getResponseBody <$> httpLBS req

--------------------------------------------------------------------------------
-- Little-endian helpers over strict ByteStrings
--------------------------------------------------------------------------------

w16 :: BS.ByteString -> Int -> Word16
w16 bs o = fromIntegral (BS.index bs o) + fromIntegral (BS.index bs (o+1)) * 256

w32 :: BS.ByteString -> Int -> Word32
w32 bs o = sum [ fromIntegral (BS.index bs (o+i)) * (256 ^ i) | i <- [0..3] ]

-- | Index of the last occurrence of a byte pattern.
lastIndexOf :: BS.ByteString -> BS.ByteString -> Maybe Int
lastIndexOf needle hay = go (BS.length hay - BS.length needle)
  where
    go i | i < 0                                  = Nothing
         | needle `BS.isPrefixOf` BS.drop i hay   = Just i
         | otherwise                              = go (i - 1)

sigBytes :: Word32 -> BS.ByteString
sigBytes s = BS.pack [ fromIntegral (s `div` (256 ^ i)) | i <- [0..3 :: Int] ]

--------------------------------------------------------------------------------
-- Central directory: find its offset/size, fetch it, parse the entries
--------------------------------------------------------------------------------

-- | (offset, size) of the central directory, resolving zip64 when present.
locateCentralDir :: String -> Word64 -> IO (Word64, Word64)
locateCentralDir url total = do
  let tailLen  = fromIntegral (min (fromIntegral eocdSearchLen) total)
      tailStart = total - tailLen
  buf <- rangeGet url tailStart (total - 1)
  eocdRel <- maybe (die "no EOCD record found (not a zip?)") pure (lastIndexOf (sigBytes eocdSig) buf)
  let cdOff32  = w32 buf (eocdRel + 16)
      cdSize32 = w32 buf (eocdRel + 12)
  if cdOff32 /= 0xFFFFFFFF && cdSize32 /= 0xFFFFFFFF
    then pure (fromIntegral cdOff32, fromIntegral cdSize32)
    else locateCentralDir64 url buf eocdRel

-- | Follow the zip64 EOCD locator (sits 20 bytes before the EOCD) to the zip64
-- EOCD record, which carries 8-byte offset/size fields.
locateCentralDir64 :: String -> BS.ByteString -> Int -> IO (Word64, Word64)
locateCentralDir64 url buf eocdRel = do
  let locRel = eocdRel - 20
  when (locRel < 0 || w32 buf locRel /= eocd64LocSig) $
    die "zip64 markers present but EOCD64 locator missing"
  let eocd64Off = runGet getWord64le (BL.fromStrict (BS.take 8 (BS.drop (locRel + 8) buf)))
  rec <- rangeGet url eocd64Off (eocd64Off + 55)
  when (w32 rec 0 /= eocd64Sig) $ die "zip64 EOCD record signature mismatch"
  let cdSize = runGet getWord64le (BL.fromStrict (BS.take 8 (BS.drop 40 rec)))
      cdOff  = runGet getWord64le (BL.fromStrict (BS.take 8 (BS.drop 48 rec)))
  pure (cdOff, cdSize)

parseCentralDir :: BL.ByteString -> [CDEntry]
parseCentralDir = runGet go
  where
    go = isEmpty >>= \case
      True  -> pure []
      False -> do
        sig <- getWord32le
        if sig /= cdSig then pure [] else do
          skip 4                          -- version made by / needed
          _flags     <- getWord16le
          method     <- getWord16le
          skip 4                          -- mod time + date
          _crc       <- getWord32le
          compSize32 <- getWord32le
          uncmp32    <- getWord32le
          fnLen      <- fromIntegral <$> getWord16le
          exLen      <- fromIntegral <$> getWord16le
          cmLen      <- fromIntegral <$> getWord16le
          skip 8                          -- disk# + internal + external attrs
          loff32     <- getWord32le
          name       <- getByteString fnLen
          extra      <- getByteString exLen
          _comment   <- getByteString cmLen
          let (compSize, loff) = resolveZip64 extra uncmp32 compSize32 loff32
          (CDEntry name method compSize loff :) <$> go

-- | Pull compressed size and local-header offset out of the zip64 extra field
-- (id 0x0001) for whichever of the 32-bit fields were set to 0xFFFFFFFF. The
-- values appear in the fixed order: uncompressed, compressed, offset, diskStart.
resolveZip64 :: BS.ByteString -> Word32 -> Word32 -> Word32 -> (Word64, Word64)
resolveZip64 extra uncmp32 comp32 loff32 =
  case findExtra 0x0001 extra of
    Nothing  -> (fromIntegral comp32, fromIntegral loff32)
    Just dat -> runGet parse (BL.fromStrict dat)
  where
    parse = do
      _ <- if uncmp32 == 0xFFFFFFFF then getWord64le else pure 0
      c <- if comp32  == 0xFFFFFFFF then getWord64le else pure (fromIntegral comp32)
      o <- if loff32  == 0xFFFFFFFF then getWord64le else pure (fromIntegral loff32)
      pure (c, o)

findExtra :: Word16 -> BS.ByteString -> Maybe BS.ByteString
findExtra wanted = go
  where
    go bs
      | BS.length bs < 4 = Nothing
      | otherwise =
          let hid = w16 bs 0
              sz  = fromIntegral (w16 bs 2)
          in if hid == wanted
               then Just (BS.take sz (BS.drop 4 bs))
               else go (BS.drop (4 + sz) bs)

--------------------------------------------------------------------------------
-- Extract one entry's bytes (reads the local header, then the compressed data)
--------------------------------------------------------------------------------

extractEntry :: String -> CDEntry -> IO BL.ByteString
extractEntry url e = do
  -- The local header's filename/extra lengths can differ from the central
  -- directory's, so read them from the local header itself.
  hdr <- rangeGet url (cdLocalOffset e) (cdLocalOffset e + 29)
  let fnLen = fromIntegral (w16 hdr 26)
      exLen = fromIntegral (w16 hdr 28)
      dataOff = cdLocalOffset e + 30 + fnLen + exLen
  raw <- rangeGet url dataOff (dataOff + cdCompSize e - 1)
  case cdMethod e of
    0 -> pure (BL.fromStrict raw)                -- stored
    8 -> pure (Raw.decompress (BL.fromStrict raw)) -- deflate
    m -> die ("unsupported compression method " <> show m)

--------------------------------------------------------------------------------
-- Matching: exact path at the zip root, else basename match
--------------------------------------------------------------------------------

matchEntry :: String -> [CDEntry] -> Maybe CDEntry
matchEntry target entries =
  find ((== tgt) . cdName) entries
    `orElse` find (basenameIs tgt . cdName) entries
  where
    tgt = BS8.pack target
    basenameIs t n = ("/" <> t) `BS.isSuffixOf` n
    orElse a b = maybe b Just a

--------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= \case
  ["--zip", url, target] -> do
    total <- remoteSize url
    putStrLn ("zip size: " <> show total <> " bytes")
    (cdOff, cdSize) <- locateCentralDir url total
    putStrLn ("central dir at " <> show cdOff <> " (" <> show cdSize <> " bytes)")
    cd <- rangeGet url cdOff (cdOff + cdSize - 1)
    let entries = parseCentralDir (BL.fromStrict cd)
    putStrLn (show (length entries) <> " entries")
    case matchEntry target entries of
      Nothing -> die ("no entry matching " <> target)
      Just e -> do
        putStrLn ("matched " <> BS8.unpack (cdName e)
                  <> " method=" <> show (cdMethod e) <> " comp=" <> show (cdCompSize e))
        bytes <- extractEntry url e
        BL.writeFile "OUT.bin" bytes
        putStrLn ("wrote " <> show (BL.length bytes) <> " bytes to OUT.bin")

  [owner, repo, runIdStr, target] -> do
    token <- getToken
    let auth = OAuth (BS8.pack token)
        owner' = fromString owner :: Name Owner
        repo'  = fromString repo  :: Name Repo
        runId' = mkId (P.Proxy :: P.Proxy WorkflowRun) (read runIdStr)

    arts <- runReq (artifactsForWorkflowRunR owner' repo' runId' FetchAll) auth >>= \case
      Left err -> die ("listing artifacts failed: " <> show err)
      Right wtc -> pure (V.toList (withTotalCountItems wtc))

    putStrLn ("Run has " <> show (length arts) <> " artifact(s).")
    forM_ arts $ \a ->
      putStrLn ("  - " <> T.unpack (artifactName a)
                <> " (" <> show (artifactSizeInBytes a) <> " bytes, id="
                <> show (untagId (artifactId a)) <> ")")

    hits <- mapMaybeM (tryArtifact auth owner' repo' target) arts
    case hits of
      [] -> die ("No artifact in this run contains " <> target)
      ((name, bytes):_) -> do
        let out = "OUT-" <> T.unpack name
        BL.writeFile out bytes
        putStrLn ("Wrote " <> show (BL.length bytes) <> " bytes to " <> out
                  <> " (from artifact " <> T.unpack name <> ")")
  _ -> die "usage: RemoteArtifactFile OWNER REPO RUN_ID FILENAME"

-- | Check one artifact's central directory (cheap) and, if the target is there,
-- extract just that file.
tryArtifact :: Auth -> Name Owner -> Name Repo -> String -> Artifact
            -> IO (Maybe (T.Text, BL.ByteString))
tryArtifact auth owner repo target a = do
  runReq (downloadArtifactR owner repo (artifactId a)) auth >>= \case
    Left err -> do
      putStrLn ("  ! " <> T.unpack (artifactName a) <> ": download URL failed: " <> show err)
      pure Nothing
    Right (uri :: URI) -> do
      let url = uriToString id uri ""
      total <- remoteSize url
      (cdOff, cdSize) <- locateCentralDir url total
      cd <- rangeGet url cdOff (cdOff + cdSize - 1)
      let entries = parseCentralDir (BL.fromStrict cd)
      case matchEntry target entries of
        Nothing -> do
          putStrLn ("  · " <> T.unpack (artifactName a) <> ": "
                    <> show (length entries) <> " entries, no match")
          pure Nothing
        Just e -> do
          putStrLn ("  ✓ " <> T.unpack (artifactName a) <> ": found "
                    <> BS8.unpack (cdName e) <> " (comp " <> show (cdCompSize e) <> " bytes)")
          bytes <- extractEntry url e
          pure (Just (artifactName a, bytes))

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

runReq :: (AuthMethod am, ParseResponse mt a) => GenRequest mt rw a -> am -> IO (Either Error a)
runReq req auth = do
  mgr <- newManager tlsManagerSettings
  fmap (fmap responseBody) (executeRequestWithMgrAndRes mgr auth req)

getToken :: IO String
getToken = lookupEnv "GITHUB_TOKEN" >>= \case
  Just t | not (null t) -> pure t
  _ -> fmap (filter (/= '\n')) (readProcess "gh" ["auth", "token"] "")

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap catMaybes . mapM f
