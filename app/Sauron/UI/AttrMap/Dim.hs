module Sauron.UI.AttrMap.Dim (
  dimAttrMappings
  , dimColor
  , DimAmount
  ) where

import Brick (AttrName)
import qualified Graphics.Vty as V
import Relude


-- | Dim amount from 0.0 (unchanged) to 1.0 (fully black).
type DimAmount = Double

-- | Dim all colors in an attribute mapping list by the given amount.
dimAttrMappings :: DimAmount -> [(AttrName, V.Attr)] -> [(AttrName, V.Attr)]
dimAttrMappings amount = map (second (dimAttr amount))

dimAttr :: DimAmount -> V.Attr -> V.Attr
dimAttr amount attr = attr
  { V.attrForeColor = dimMaybeColor amount (V.attrForeColor attr)
  , V.attrBackColor = dimMaybeColor amount (V.attrBackColor attr)
  }

dimMaybeColor :: DimAmount -> V.MaybeDefault V.Color -> V.MaybeDefault V.Color
dimMaybeColor amount (V.SetTo c) = V.SetTo (dimColor amount c)
dimMaybeColor _ x = x

dimColor :: DimAmount -> V.Color -> V.Color
dimColor amount (V.RGBColor r g b) = V.RGBColor (d r) (d g) (d b)
  where d = dimByte (1.0 - amount)
dimColor amount (V.Color240 idx) =
  let (r, g, b) = color240ToRGB idx
  in V.Color240 (rgbToColor240 (d r) (d g) (d b))
  where d = dimByte (1.0 - amount)
dimColor amount (V.ISOColor idx) =
  let (r, g, b) = isoColorToRGB idx
  in V.RGBColor (d r) (d g) (d b)
  where d = dimByte (1.0 - amount)

dimByte :: Double -> Word8 -> Word8
dimByte factor x =
  let result = round (fromIntegral x * factor) :: Int
  in fromIntegral (max 0 (min 255 result))

-- * ISOColor (ANSI 16) to approximate RGB

-- | Convert an ANSI 16-color index to approximate RGB values (xterm defaults).
isoColorToRGB :: Word8 -> (Word8, Word8, Word8)
isoColorToRGB 0  = (0,   0,   0)    -- black
isoColorToRGB 1  = (205, 0,   0)    -- red
isoColorToRGB 2  = (0,   205, 0)    -- green
isoColorToRGB 3  = (205, 205, 0)    -- yellow
isoColorToRGB 4  = (0,   0,   238)  -- blue
isoColorToRGB 5  = (205, 0,   205)  -- magenta
isoColorToRGB 6  = (0,   205, 205)  -- cyan
isoColorToRGB 7  = (229, 229, 229)  -- white
isoColorToRGB 8  = (127, 127, 127)  -- bright black
isoColorToRGB 9  = (255, 0,   0)    -- bright red
isoColorToRGB 10 = (0,   255, 0)    -- bright green
isoColorToRGB 11 = (255, 255, 0)    -- bright yellow
isoColorToRGB 12 = (92,  92,  255)  -- bright blue
isoColorToRGB 13 = (255, 0,   255)  -- bright magenta
isoColorToRGB 14 = (0,   255, 255)  -- bright cyan
isoColorToRGB 15 = (255, 255, 255)  -- bright white
isoColorToRGB _  = (255, 255, 255)  -- fallback

-- * Color240 palette conversions

-- | Convert a Color240 palette index (0-239) to RGB.
-- Indices 0-215 are a 6x6x6 color cube, 216-239 are a grayscale ramp.
color240ToRGB :: Word8 -> (Word8, Word8, Word8)
color240ToRGB idx
  | idx < 216 =
      let r = idx `div` 36
          g = (idx `mod` 36) `div` 6
          b = idx `mod` 6
      in (cubeVal r, cubeVal g, cubeVal b)
  | otherwise =
      let gray = fromIntegral (8 + 10 * fromIntegral (idx - 216) :: Int)
      in (gray, gray, gray)

-- | Convert a 6x6x6 cube component index (0-5) to an RGB byte value.
cubeVal :: Word8 -> Word8
cubeVal 0 = 0
cubeVal n = fromIntegral (55 + 40 * fromIntegral n :: Int)

-- | Convert an RGB triple to the nearest Color240 palette index,
-- choosing whichever is closer between the 6x6x6 cube and the grayscale ramp.
rgbToColor240 :: Word8 -> Word8 -> Word8 -> Word8
rgbToColor240 r g b =
  let -- Nearest 6x6x6 cube match
      ri = nearestCubeIdx r
      gi = nearestCubeIdx g
      bi = nearestCubeIdx b
      cubeColor = (cubeVal ri, cubeVal gi, cubeVal bi)
      cubeErr = colorDistSq (r, g, b) cubeColor
      -- Nearest grayscale ramp match
      avg = (fromIntegral r + fromIntegral g + fromIntegral b) `div` 3 :: Int
      grayStep = max 0 (min 23 (round ((fromIntegral avg - 8 :: Double) / 10)))
      grayLevel = fromIntegral (8 + 10 * grayStep) :: Word8
      grayErr = colorDistSq (r, g, b) (grayLevel, grayLevel, grayLevel)
  in if grayErr < cubeErr
     then fromIntegral grayStep + 216
     else 36 * ri + 6 * gi + bi

-- | Find the nearest 6x6x6 cube component index (0-5) for a byte value.
nearestCubeIdx :: Word8 -> Word8
nearestCubeIdx v
  | v < 48    = 0
  | v < 115   = 1
  | v < 155   = 2
  | v < 195   = 3
  | v < 235   = 4
  | otherwise = 5

-- | Squared Euclidean distance between two RGB colors.
colorDistSq :: (Word8, Word8, Word8) -> (Word8, Word8, Word8) -> Int
colorDistSq (r1, g1, b1) (r2, g2, b2) =
  let dr = fromIntegral r1 - fromIntegral r2 :: Int
      dg = fromIntegral g1 - fromIntegral g2 :: Int
      db = fromIntegral b1 - fromIntegral b2 :: Int
  in dr*dr + dg*dg + db*db
