-- | Foreground/background readability using WCAG 2 relative luminance
-- and contrast ratio. The formulas are defined in the W3C Recommendation:
--
--   * <https://www.w3.org/TR/WCAG21/#dfn-relative-luminance>
--   * <https://www.w3.org/TR/WCAG21/#dfn-contrast-ratio>
--
-- The returned ratio ranges from @1.0@ (identical colours) to @21.0@
-- (pure black on pure white). Common WCAG thresholds:
--
--   * @>= 4.5@ — AA, normal-size text
--   * @>= 7.0@ — AAA, normal-size text
--   * @>= 3.0@ — AA, large or bold text
--
-- For terminal use, WCAG is conservative — terminal text is generally
-- already large and bold-ish — so 4.5 is a reasonable floor and 7 a
-- comfortable target.

module Sauron.Contrast (
  -- * Types
  ContrastRatio
  , Luminance
  , RGB

  -- * Luminance & contrast
  , relativeLuminance
  , contrast

  -- * Choosing colours
  , bestForeground
  , foregroundForRatio
  ) where

import Data.List (maximumBy)
import Relude

-- | WCAG 2 contrast ratio. Always @>= 1@.
type ContrastRatio = Double

-- | Relative luminance, in @[0, 1]@.
type Luminance = Double

-- | sRGB triple, each channel in @[0, 255]@.
type RGB = (Double, Double, Double)

-- | WCAG 2 relative luminance of an sRGB colour.
relativeLuminance :: RGB -> Luminance
relativeLuminance (r, g, b) =
  0.2126 * lin r + 0.7152 * lin g + 0.0722 * lin b
  where
    lin c8 =
      let c = c8 / 255
      in if c <= 0.03928
         then c / 12.92
         else ((c + 0.055) / 1.055) ** 2.4

-- | WCAG 2 contrast ratio between two sRGB colours. Symmetric in its
-- arguments. Result is in @[1, 21]@.
contrast :: RGB -> RGB -> ContrastRatio
contrast a b = (hi + 0.05) / (lo + 0.05)
  where
    la = relativeLuminance a
    lb = relativeLuminance b
    hi = max la lb
    lo = min la lb

-- | From a list of candidate foreground colours, pick the one with
-- the highest contrast ratio against @bg@. Useful for 16- or
-- 256-colour palette cases.
--
-- > bestForeground bg [(0,0,0), (255,255,255)]
bestForeground :: RGB -> [RGB] -> RGB
bestForeground bg candidates =
  snd $ maximumBy (comparing fst) [(contrast fg bg, fg) | fg <- candidates]

-- | Find a greyscale foreground that hits (approximately) the given
-- target contrast ratio against @bg@. Always returns a colour: if the
-- target is unreachable, falls back to whichever pure extreme (black
-- or white) gives the highest contrast.
--
-- Polarity (lighter or darker than @bg@) is chosen automatically by
-- whichever direction has more headroom — i.e. dark text on light
-- backgrounds, light text on dark backgrounds.
foregroundForRatio :: ContrastRatio -> RGB -> RGB
foregroundForRatio target bg
  | maxLight > maxDark =
      if target >= maxLight
         then (255, 255, 255)
         else greyOfLuminance (target * (lBg + 0.05) - 0.05)
  | otherwise =
      if target >= maxDark
         then (0, 0, 0)
         else greyOfLuminance ((lBg + 0.05) / target - 0.05)
  where
    lBg = relativeLuminance bg
    -- Max ratio achievable going lighter (Lfg = 1) or darker (Lfg = 0):
    maxLight = 1.05 / (lBg + 0.05)
    maxDark  = (lBg + 0.05) / 0.05

-- | Construct a greyscale sRGB triple with a given relative luminance.
-- For greyscale, linear-luminance equals the linear channel value, so
-- we just invert the sRGB transfer curve.
greyOfLuminance :: Luminance -> RGB
greyOfLuminance l = (c8, c8, c8)
  where
    l' = max 0 (min 1 l)
    c  = if l' <= 0.0031308
         then l' * 12.92
         else 1.055 * (l' ** (1 / 2.4)) - 0.055
    c8 = 255 * c
