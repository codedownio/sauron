
module Sauron.UI.AnsiUtil (
  parseAnsiText
) where

import Brick
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Relude


data AnsiSegment = AnsiSegment {
  segText :: Text
  , segForeground :: Maybe V.Color
  , segBackground :: Maybe V.Color
  , segBold :: Bool
  , segItalic :: Bool
  , segUnderline :: Bool
  } deriving (Show)

defaultSegment :: AnsiSegment
defaultSegment = AnsiSegment {
  segText = ""
  , segForeground = Nothing
  , segBackground = Nothing
  , segBold = False
  , segItalic = False
  , segUnderline = False
  }

parseAnsiText :: Text -> [Widget n]
parseAnsiText input = input
  & toString
  & flip parseAnsiSegments defaultSegment
  & map segmentToWidget

segmentToWidget :: AnsiSegment -> Widget n
segmentToWidget seg
  | T.null (segText seg) = str ""
  | otherwise = raw $ V.text' (buildAttr seg) (segText seg)
  where
    buildAttr s =
      let attr = V.defAttr
               & applyForeground (segForeground s)
               & applyBackground (segBackground s)
      in foldr ($) attr (styleModifiers s)

    styleModifiers s =
      (if segBold s then [(`V.withStyle` V.bold)] else []) ++
      (if segItalic s then [(`V.withStyle` V.italic)] else []) ++
      (if segUnderline s then [(`V.withStyle` V.underline)] else [])

    applyForeground Nothing attr = attr
    applyForeground (Just color) attr = attr `V.withForeColor` color

    applyBackground Nothing attr = attr
    applyBackground (Just color) attr = attr `V.withBackColor` color

parseAnsiSegments :: String -> AnsiSegment -> [AnsiSegment]
parseAnsiSegments [] currentSeg = [currentSeg | not (T.null $ segText currentSeg)]
parseAnsiSegments ('\ESC':'[':rest) currentSeg =
  case parseEscapeSequence rest of
    (newSeg, remaining) ->
      let segments = [currentSeg | not (T.null $ segText currentSeg)]
      in segments ++ parseAnsiSegments remaining newSeg
parseAnsiSegments (c:rest) currentSeg =
  parseAnsiSegments rest currentSeg { segText = segText currentSeg <> T.singleton c }

parseEscapeSequence :: String -> (AnsiSegment, String)
parseEscapeSequence input = (applyCodes parsedCodes defaultSegment, remaining')
  where
    (codes, remaining) = span (/= 'm') input
    remaining' = drop 1 remaining
    parsedCodes = parseCodes codes

parseCodes :: String -> [Int]
parseCodes "" = []
parseCodes s =
  case reads s of
    [(n, "")] -> [n]
    [(n, ';':rest)] -> n : parseCodes rest
    _ -> []

applyCodes :: [Int] -> AnsiSegment -> AnsiSegment
applyCodes [] seg = seg
applyCodes (0:rest) _ = applyCodes rest defaultSegment
applyCodes (1:rest) seg = applyCodes rest seg { segBold = True }
applyCodes (3:rest) seg = applyCodes rest seg { segItalic = True }
applyCodes (4:rest) seg = applyCodes rest seg { segUnderline = True }
applyCodes (22:rest) seg = applyCodes rest seg { segBold = False }
applyCodes (23:rest) seg = applyCodes rest seg { segItalic = False }
applyCodes (24:rest) seg = applyCodes rest seg { segUnderline = False }
applyCodes (30:rest) seg = applyCodes rest seg { segForeground = Just V.black }
applyCodes (31:rest) seg = applyCodes rest seg { segForeground = Just V.red }
applyCodes (32:rest) seg = applyCodes rest seg { segForeground = Just V.green }
applyCodes (33:rest) seg = applyCodes rest seg { segForeground = Just V.yellow }
applyCodes (34:rest) seg = applyCodes rest seg { segForeground = Just V.blue }
applyCodes (35:rest) seg = applyCodes rest seg { segForeground = Just V.magenta }
applyCodes (36:rest) seg = applyCodes rest seg { segForeground = Just V.cyan }
applyCodes (37:rest) seg = applyCodes rest seg { segForeground = Just V.white }
applyCodes (38:2:r:g:b:rest) seg = applyCodes rest seg { segForeground = Just (V.rgbColor r g b) }
applyCodes (39:rest) seg = applyCodes rest seg { segForeground = Nothing }
applyCodes (40:rest) seg = applyCodes rest seg { segBackground = Just V.black }
applyCodes (41:rest) seg = applyCodes rest seg { segBackground = Just V.red }
applyCodes (42:rest) seg = applyCodes rest seg { segBackground = Just V.green }
applyCodes (43:rest) seg = applyCodes rest seg { segBackground = Just V.yellow }
applyCodes (44:rest) seg = applyCodes rest seg { segBackground = Just V.blue }
applyCodes (45:rest) seg = applyCodes rest seg { segBackground = Just V.magenta }
applyCodes (46:rest) seg = applyCodes rest seg { segBackground = Just V.cyan }
applyCodes (47:rest) seg = applyCodes rest seg { segBackground = Just V.white }
applyCodes (48:2:r:g:b:rest) seg = applyCodes rest seg { segBackground = Just (V.rgbColor r g b) }
applyCodes (49:rest) seg = applyCodes rest seg { segBackground = Nothing }
applyCodes (90:rest) seg = applyCodes rest seg { segForeground = Just V.brightBlack }
applyCodes (91:rest) seg = applyCodes rest seg { segForeground = Just V.brightRed }
applyCodes (92:rest) seg = applyCodes rest seg { segForeground = Just V.brightGreen }
applyCodes (93:rest) seg = applyCodes rest seg { segForeground = Just V.brightYellow }
applyCodes (94:rest) seg = applyCodes rest seg { segForeground = Just V.brightBlue }
applyCodes (95:rest) seg = applyCodes rest seg { segForeground = Just V.brightMagenta }
applyCodes (96:rest) seg = applyCodes rest seg { segForeground = Just V.brightCyan }
applyCodes (97:rest) seg = applyCodes rest seg { segForeground = Just V.brightWhite }
applyCodes (_:rest) seg = applyCodes rest seg
