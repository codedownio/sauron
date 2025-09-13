module Sauron.UI.Markdown.Table (
  renderTableWith
  , renderTableRowWith
  , renderTableCellWith
  ) where

import Brick
import Relude
import Sauron.UI.AttrMap
import qualified Text.Pandoc.Builder as B

-- Render a table with header and body rows (parameterized to avoid circular imports)
renderTableWith :: (Maybe (Widget n) -> Int -> B.Block -> Widget n) -> Int -> B.TableHead -> [B.TableBody] -> Widget n
renderTableWith renderBlockFn width (B.TableHead _ theadRows) tbodyList = 
  let extractBodyRows (B.TableBody _ _ rows1 rows2) = rows1 ++ rows2
      allBodyRows = concatMap extractBodyRows tbodyList
      allRows = theadRows ++ allBodyRows
      numCols = case allRows of
                  (B.Row _ cells : _) -> length cells
                  [] -> 0
      colWidth = if numCols > 0 then (width - numCols + 1) `div` numCols else width
  in vBox $ map (renderTableRowWith renderBlockFn colWidth numCols) allRows

-- Render a single table row (parameterized to avoid circular imports)
renderTableRowWith :: (Maybe (Widget n) -> Int -> B.Block -> Widget n) -> Int -> Int -> B.Row -> Widget n
renderTableRowWith renderBlockFn colWidth numCols (B.Row _ cells) =
  let paddedCells = take numCols (cells ++ repeat (B.Cell ("", [], []) B.AlignDefault (B.RowSpan 1) (B.ColSpan 1) []))
      renderedCells = map (renderTableCellWith renderBlockFn colWidth) paddedCells
      cellsWithSeparators = intersperse (withAttr horizontalRuleAttr $ str "│") renderedCells
  in hBox cellsWithSeparators

-- Render a single table cell (parameterized to avoid circular imports)
renderTableCellWith :: (Maybe (Widget n) -> Int -> B.Block -> Widget n) -> Int -> B.Cell -> Widget n
renderTableCellWith renderBlockFn colWidth (B.Cell _ _ _ _ blocks) =
  let cellContent = case blocks of
                      [] -> str " "
                      _ -> vBox $ map (renderBlockFn Nothing colWidth) blocks
  in hLimit colWidth $ padRight Max cellContent