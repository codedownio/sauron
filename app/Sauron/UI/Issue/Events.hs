module Sauron.UI.Issue.Events (
  -- * Types
  TimelineItem(..)

  -- * Consolidation
  , consolidateEvents

  -- * Rendering
  , renderEvent
  , renderLabelGroup
  , labelPill

  -- * Layout
  , adaptiveWidth
  , maxCommentWidth
  ) where

import Brick
import Control.Lens ((^.))
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time
import GitHub
import GitHub.Data.Name
import qualified Graphics.Vty as Vty
import Numeric (readHex)
import Relude
import Sauron.Contrast (RGB, bestForeground)
import Sauron.UI.AttrMap
import Sauron.UI.Event (getEventDescription, getEventIconWithColor)
import Sauron.UI.Util.TimeDiff


-- * Types

-- | A timeline item is either a single comment/event or a group of label events
-- that share the same timestamp and actor.
data TimelineItem
  = SingleItem (Either IssueEvent IssueComment)
  | LabelGroup IssueEvent [IssueEvent] [IssueEvent]  -- ^ representative event, added events, removed events


-- * Layout

maxCommentWidth :: Int
maxCommentWidth = 120

-- | Render with width capped at maxCommentWidth but shrinking for narrow terminals
adaptiveWidth :: (Int -> Widget n) -> Widget n
adaptiveWidth f = Widget Greedy Fixed $ do
  c <- getContext
  let w = min maxCommentWidth ((c ^. availWidthL) - 1)
  render (hLimit w (f w))


-- * Consolidation

-- | Consolidate consecutive label events with the same timestamp and actor
-- into 'LabelGroup' items.
consolidateEvents :: [Either IssueEvent IssueComment] -> [TimelineItem]
consolidateEvents [] = []
consolidateEvents (Right c : rest) = SingleItem (Right c) : consolidateEvents rest
consolidateEvents (Left e : rest)
  | isLabelEvent e =
      let (labelRun, remaining) = span (isMatchingLabel e) rest
          labelEvents = e : [ev | Left ev <- labelRun]
          added = filter (\ev -> issueEventType ev == Labeled) labelEvents
          removed = filter (\ev -> issueEventType ev == Unlabeled) labelEvents
      in if length labelEvents > 1
         then LabelGroup e added removed : consolidateEvents remaining
         else SingleItem (Left e) : consolidateEvents rest
  | otherwise = SingleItem (Left e) : consolidateEvents rest
  where
    isMatchingLabel ref (Left ev) =
      isLabelEvent ev
      && abs (diffUTCTime (issueEventCreatedAt ev) (issueEventCreatedAt ref)) < 10
      && simpleUserLogin (issueEventActor ev) == simpleUserLogin (issueEventActor ref)
    isMatchingLabel _ _ = False

isLabelEvent :: IssueEvent -> Bool
isLabelEvent e = issueEventType e `elem` [Labeled, Unlabeled]


-- * Rendering

renderEvent :: UTCTime -> Bool -> IssueEvent -> Widget n
renderEvent now isLast issueEvent =
  let actorName :: Text = case simpleUserLogin (issueEventActor issueEvent) of
        N username -> username
      (labelWidget, labelSuffix) = case issueEventLabel issueEvent of
        Just (IssueLabel {labelName=(N name), labelColor=hexColor}) -> (str " the " <+> labelPill hexColor name, " label")
        Nothing -> (emptyWidget, if issueEventType issueEvent `elem` [Labeled, Unlabeled] then " a label" else "")
      eventText = getEventDescription (issueEventType issueEvent)
      iconWidget = getEventIconWithColor (issueEventType issueEvent)
      timeAgo = timeFromNow (diffUTCTime now (issueEventCreatedAt issueEvent))
      eventLine = adaptiveWidth $ \_ ->
        padLeft (Pad 4) $ hBox [
          iconWidget
          , str "  "
          , withAttr usernameAttr $ str (toString actorName)
          , str " "
          , str eventText
          , labelWidget
          , str (labelSuffix <> " ")
          , withAttr italicText $ str timeAgo
        ]
      continuationLine = if isLast
        then emptyWidget
        else adaptiveWidth $ \_ -> padLeft (Pad 4) $ withAttr timelineBorderAttr $ str "│"
  in vBox [eventLine, continuationLine]

renderLabelGroup :: UTCTime -> Bool -> IssueEvent -> [IssueEvent] -> [IssueEvent] -> Widget n
renderLabelGroup now isLast rep added removed =
  let actorName :: Text = case simpleUserLogin (issueEventActor rep) of
        N username -> username
      timeAgo = timeFromNow (diffUTCTime now (issueEventCreatedAt rep))
      iconWidget = getEventIconWithColor (issueEventType rep)
      addedItems = mapMaybe eventLabelPillWithWidth added
      removedItems = mapMaybe eventLabelPillWithWidth removed
      totalLabels = length added + length removed
      labelWord = if totalLabels > 1 then "labels " else "label "
      timeAgoWidget = (length timeAgo, withAttr italicText $ str timeAgo)
      descriptionItems = case (addedItems, removedItems) of
        ([], rs) -> (8, str "removed ") : rs ++ [(length labelWord, str labelWord), timeAgoWidget]
        (as, []) -> (6, str "added ") : as ++ [(length labelWord, str labelWord), timeAgoWidget]
        (as, rs) -> (6, str "added ") : as ++ [(13, str " and removed ")] ++ rs ++ [(length labelWord, str labelWord), timeAgoWidget]
      -- Prefix on first line: 1 icon + 2 spaces + username + 1 space
      prefixWidth = 1 + 2 + T.length actorName + 1
      eventLine = adaptiveWidth $ \w ->
        let availableWidth = w - 4  -- subtract outer padding
            wrappedLines = wrapWidgets (availableWidth - prefixWidth) descriptionItems
            prefix = hBox [iconWidget, str "  ", withAttr usernameAttr $ str (toString actorName), str " "]
            firstLine = case wrappedLines of
              [] -> prefix
              (l:_) -> hBox [prefix, hBox (map snd l)]
            restLines = case wrappedLines of
              [] -> []
              (_:ls) -> [padLeft (Pad 3) $ hBox (map snd l) | l <- ls]
        in padLeft (Pad 4) $ vBox (firstLine : restLines)
      continuationLine = if isLast
        then emptyWidget
        else adaptiveWidth $ \_ -> padLeft (Pad 4) $ withAttr timelineBorderAttr $ str "│"
  in vBox [eventLine, continuationLine]


-- * Internal helpers

-- | A label pill with its known width (space + name + space + trailing gap)
eventLabelPillWithWidth :: IssueEvent -> Maybe (Int, Widget n)
eventLabelPillWithWidth e = case issueEventLabel e of
  Just (IssueLabel {labelName=(N name), labelColor=hexColor}) -> Just (T.length name + 3, labelPill hexColor name <+> str " ")
  Nothing -> Nothing

-- | Greedily wrap sized widgets into lines that fit within the given width.
wrapWidgets :: Int -> [(Int, a)] -> [[(Int, a)]]
wrapWidgets _ [] = []
wrapWidgets maxW items =
  let (line, rest) = takeLine maxW items
  in line : wrapWidgets maxW rest
  where
    takeLine _ [] = ([], [])
    takeLine remaining (item@(w, _):xs)
      | w <= remaining = let (moreLine, leftover) = takeLine (remaining - w) xs
                         in (item : moreLine, leftover)
      | otherwise = ([], item : xs)

labelPill :: Text -> Text -> Widget n
labelPill hexColor name = modifyDefAttr (const pillAttr) $ str [i| #{name} |]
  where
    bg' = parseHexColor hexColor
    (fr, fg', fb) = bestForeground bg' [(0, 0, 0), (255, 255, 255)]
    (br, bg'', bb) = bg'
    bgColor = Vty.rgbColor (round br :: Int) (round bg'' :: Int) (round bb :: Int)
    fgColor = Vty.rgbColor (round fr :: Int) (round fg' :: Int) (round fb :: Int)
    pillAttr = Vty.Attr Vty.Default (Vty.SetTo fgColor) (Vty.SetTo bgColor) Vty.Default

parseHexColor :: Text -> RGB
parseHexColor hex = case mapMaybe parseComponent [T.take 2 s, T.take 2 (T.drop 2 s), T.take 2 (T.drop 4 s)] of
  [r, g, b] -> (r, g, b)
  _ -> (128, 128, 128)
  where
    s = T.dropWhile (== '#') hex
    parseComponent t = case readHex (toString t) of
      [(n, "")] -> Just (fromIntegral (n :: Int) :: Double)
      _ -> Nothing
