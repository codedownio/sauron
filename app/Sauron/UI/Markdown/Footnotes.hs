module Sauron.UI.Markdown.Footnotes (
  FootnoteState(..)
  , emptyFootnoteState
  , addFootnote
  , renderFootnotes
  , FootnoteRef(..)
  ) where

import Brick
import Relude
import Sauron.UI.AttrMap
import qualified Text.Pandoc.Builder as B

-- Footnote reference information
data FootnoteRef = FootnoteRef {
  footnoteNumber :: Int,
  footnoteContent :: [B.Block]
} deriving (Show, Eq)

-- State for collecting footnotes during rendering
data FootnoteState = FootnoteState {
  footnotes :: [FootnoteRef],
  nextNumber :: Int
} deriving (Show, Eq)

-- Create empty footnote state
emptyFootnoteState :: FootnoteState
emptyFootnoteState = FootnoteState [] 1

-- Add a footnote and return the reference number
addFootnote :: [B.Block] -> FootnoteState -> (Int, FootnoteState)
addFootnote content footnoteState =
  let num = nextNumber footnoteState
      footnote = FootnoteRef num content
      newState = footnoteState {
        footnotes = footnotes footnoteState ++ [footnote],
        nextNumber = num + 1
      }
  in (num, newState)

-- Render all collected footnotes at the bottom
renderFootnotes :: (Maybe (Widget n) -> Int -> B.Block -> Widget n) -> Int -> FootnoteState -> Widget n
renderFootnotes renderBlockFn width footnoteState =
  if null (footnotes footnoteState)
  then emptyWidget
  else vBox [
    padTop (Pad 1) $ withAttr horizontalRuleAttr $ str (replicate width '─'),
    padTop (Pad 1) $ vBox $ map (renderFootnote renderBlockFn width) (footnotes footnoteState)
  ]

-- Render a single footnote
renderFootnote :: (Maybe (Widget n) -> Int -> B.Block -> Widget n) -> Int -> FootnoteRef -> Widget n
renderFootnote renderBlockFn width (FootnoteRef num content) =
  let numberPrefix = withAttr hashNumberAttr $ str ("[" <> Relude.show num <> "] ")
      renderedContent = vBox $ map (renderBlockFn Nothing (width - 4)) content
  in hBox [numberPrefix, renderedContent]
