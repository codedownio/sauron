module Sauron.UI.Markdown.Table (
  renderTableWith
  ) where

import Brick
import qualified Data.Text as T
import Relude
import Sauron.UI.AttrMap
import qualified Text.Pandoc.Builder as B

renderTableWith :: (Maybe (Widget n) -> Int -> B.Block -> Widget n) -> Int -> B.TableHead -> [B.TableBody] -> Widget n
renderTableWith renderBlockFn width (B.TableHead _ theadRows) tbodyList =
  let extractBodyRows (B.TableBody _ _ rows1 rows2) = rows1 ++ rows2
      allBodyRows = concatMap extractBodyRows tbodyList
      allRows = theadRows ++ allBodyRows
      colWidths = calculateColumnWidths width allRows
      renderedRows = map (renderTableRowWithWidths renderBlockFn colWidths) allRows
      -- Add borders
      topBorder = renderTopBorder colWidths
      bottomBorder = renderBottomBorder colWidths
      -- Special separator after header if we have headers
      headerSeparator = if not (null theadRows)
                       then [renderHeaderSeparator colWidths]
                       else []
      -- Combine everything
      allElements = [topBorder] ++
                   take (length theadRows) renderedRows ++
                   headerSeparator ++
                   drop (length theadRows) renderedRows ++
                   [bottomBorder]
  in vBox allElements

renderTableCellWith :: (Maybe (Widget n) -> Int -> B.Block -> Widget n) -> Int -> B.Cell -> Widget n
renderTableCellWith renderBlockFn colWidth (B.Cell _ _ _ _ blocks) =
  let cellContent = case blocks of
                      [] -> str " "
                      _ -> vBox $ map (renderBlockFn Nothing colWidth) blocks
  in hLimit colWidth $ padRight Max cellContent

calculateColumnWidths :: Int -> [B.Row] -> [Int]
calculateColumnWidths totalWidth rows =
  let numCols = case rows of
                  (B.Row _ cells : _) -> length cells
                  [] -> 0
  in if numCols == 0
     then []
     else
       let -- Get content widths for each column
           colContentWidths = map (getColumnContentWidth rows) [0..numCols-1]
           -- Reserve space for separators (│) - one less separator than columns
           separatorSpace = if numCols > 1 then numCols - 1 else 0
           availableWidth = max (numCols * 3) (totalWidth - separatorSpace) -- ensure minimum space
           -- For debugging: ensure we have reasonable minimum widths
           minColWidths = map (max 3) colContentWidths
           totalContentWidth = sum minColWidths
           scaledWidths = if totalContentWidth > availableWidth
                         then -- Content is too wide, scale down proportionally but keep minimums
                              map (\w -> max 3 (w * availableWidth `div` totalContentWidth)) minColWidths
                         else -- Content fits, use actual widths
                              minColWidths
       in scaledWidths

getColumnContentWidth :: [B.Row] -> Int -> Int
getColumnContentWidth rows colIndex =
  let cellWidths = mapMaybe (getCellWidth colIndex) rows
  in if null cellWidths then 3 else max 3 (foldl' max 0 cellWidths)
  where
    getCellWidth :: Int -> B.Row -> Maybe Int
    getCellWidth idx (B.Row _ cells) =
      case drop idx cells of
        (cell:_) -> Just (getCellTextWidth cell)
        [] -> Nothing

getCellTextWidth :: B.Cell -> Int
getCellTextWidth (B.Cell _ _ _ _ blocks) =
  foldl' max 3 $ map getBlockTextWidth blocks
  where
    getBlockTextWidth :: B.Block -> Int
    getBlockTextWidth (B.Para inlines) = sum $ map getInlineTextWidth inlines
    getBlockTextWidth (B.Plain inlines) = sum $ map getInlineTextWidth inlines
    getBlockTextWidth _ = 5 -- fallback for other block types

    getInlineTextWidth :: B.Inline -> Int
    getInlineTextWidth (B.Str text) = T.length text
    getInlineTextWidth (B.Code _ text) = T.length text
    getInlineTextWidth B.Space = 1
    getInlineTextWidth (B.Emph inlines) = sum $ map getInlineTextWidth inlines
    getInlineTextWidth (B.Strong inlines) = sum $ map getInlineTextWidth inlines
    getInlineTextWidth (B.Link _ inlines _) = sum $ map getInlineTextWidth inlines
    getInlineTextWidth _ = 1 -- fallback

renderTableRowWithWidths :: (Maybe (Widget n) -> Int -> B.Block -> Widget n) -> [Int] -> B.Row -> Widget n
renderTableRowWithWidths renderBlockFn colWidths (B.Row _ cells) =
  let paddedCells = take (length colWidths) (cells ++ repeat (B.Cell ("", [], []) B.AlignDefault (B.RowSpan 1) (B.ColSpan 1) []))
      renderedCells = zipWith (renderTableCellWith renderBlockFn) colWidths paddedCells
      cellsWithSeparators = intersperse (withAttr horizontalRuleAttr $ str "│") renderedCells
      leftBorder = withAttr horizontalRuleAttr $ str "│"
      rightBorder = withAttr horizontalRuleAttr $ str "│"
  in hBox $ [leftBorder] ++ cellsWithSeparators ++ [rightBorder]

renderTopBorder :: [Int] -> Widget n
renderTopBorder colWidths =
  let borderParts = map (\w -> withAttr horizontalRuleAttr $ str (replicate w '─')) colWidths
      bordersWithJoins = intersperse (withAttr horizontalRuleAttr $ str "┬") borderParts
  in hBox $ [withAttr horizontalRuleAttr $ str "┌"] ++ bordersWithJoins ++ [withAttr horizontalRuleAttr $ str "┐"]

renderBottomBorder :: [Int] -> Widget n
renderBottomBorder colWidths =
  let borderParts = map (\w -> withAttr horizontalRuleAttr $ str (replicate w '─')) colWidths
      bordersWithJoins = intersperse (withAttr horizontalRuleAttr $ str "┴") borderParts
  in hBox $ [withAttr horizontalRuleAttr $ str "└"] ++ bordersWithJoins ++ [withAttr horizontalRuleAttr $ str "┘"]

renderHeaderSeparator :: [Int] -> Widget n
renderHeaderSeparator colWidths =
  let separatorParts = map (\w -> withAttr horizontalRuleAttr $ str (replicate w '═')) colWidths
      separatorsWithJoins = intersperse (withAttr horizontalRuleAttr $ str "╪") separatorParts
  in hBox $ [withAttr horizontalRuleAttr $ str "├"] ++ separatorsWithJoins ++ [withAttr horizontalRuleAttr $ str "┤"]
