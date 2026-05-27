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

  -- * Re-exports from Common
  , wrapWidgets
  ) where

import Brick
import Control.Lens ((^.))
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time
import GitHub
import GitHub.Data.Name
import Graphics.Text.Width (safeWcswidth, safeWctwidth)
import qualified Graphics.Vty as Vty
import Numeric (readHex)
import Relude
import Sauron.Contrast (RGB, bestForeground)
import Sauron.UI.AttrMap
import Sauron.UI.Event (getEventDescription, getEventIconWithColor)
import Sauron.UI.Issue.Events.Common (wrapWidgets)
import Sauron.UI.Util.TimeDiff


-- * Types

-- | A timeline item is either a single comment/event or a consolidated group.
data TimelineItem
  = SingleItem TimelineEvent
  | LabelGroup IssueEvent [IssueEvent] [IssueEvent]  -- ^ representative event, added events, removed events
  | ReviewRequestGroup IssueEvent [IssueEvent]        -- ^ representative event, all review_requested events


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

-- | Consolidate consecutive events of the same kind, timestamp, and actor
-- into grouped items.
consolidateEvents :: [TimelineEvent] -> [TimelineItem]
consolidateEvents [] = []
consolidateEvents (TimelineComment c : rest) = SingleItem (TimelineComment c) : consolidateEvents rest
consolidateEvents (TimelineIssueEvent e : rest)
  | isLabelEvent e =
      let (run, remaining) = span (isMatchingEvent isLabelEvent e) rest
          events = e : [ev | TimelineIssueEvent ev <- run]
          added = filter (\ev -> issueEventType ev == Labeled) events
          removed = filter (\ev -> issueEventType ev == Unlabeled) events
      in if length events > 1
         then LabelGroup e added removed : consolidateEvents remaining
         else SingleItem (TimelineIssueEvent e) : consolidateEvents rest
  | isReviewRequestEvent e =
      let (run, remaining) = span (isMatchingEvent isReviewRequestEvent e) rest
          events = e : [ev | TimelineIssueEvent ev <- run]
      in if length events > 1
         then ReviewRequestGroup e events : consolidateEvents remaining
         else SingleItem (TimelineIssueEvent e) : consolidateEvents rest
  | otherwise = SingleItem (TimelineIssueEvent e) : consolidateEvents rest

isMatchingEvent :: (IssueEvent -> Bool) -> IssueEvent -> TimelineEvent -> Bool
isMatchingEvent predicate ref (TimelineIssueEvent ev) =
  predicate ev
  && abs (diffUTCTime (issueEventCreatedAt ev) (issueEventCreatedAt ref)) < 10
  && fmap simpleUserLogin (issueEventActor ev) == fmap simpleUserLogin (issueEventActor ref)
isMatchingEvent _ _ _ = False

isLabelEvent :: IssueEvent -> Bool
isLabelEvent e = issueEventType e `elem` [Labeled, Unlabeled]

isReviewRequestEvent :: IssueEvent -> Bool
isReviewRequestEvent e = issueEventType e == ReviewRequested


-- * Rendering

renderEvent :: UTCTime -> Bool -> IssueEvent -> Widget n
renderEvent now isLast issueEvent =
  let actorName :: Text = case issueEventActor issueEvent of
        Just (SimpleUser {simpleUserLogin=(N username)}) -> username
        Nothing -> fromMaybe "ghost" (issueEventAuthorName issueEvent)
      -- Each sized item is (width, widget)
      eventSuffixItems :: [(Int, Widget n)]
      eventSuffixItems = case issueEventType issueEvent of
        Labeled -> case issueEventLabel issueEvent of
          Just (IssueLabel {labelName=(N name), labelColor=hexColor}) -> [(5, str " the "), (safeWctwidth name + 2, labelPill hexColor name), (7, str " label")]
          Nothing -> [(8, str " a label")]
        Unlabeled -> case issueEventLabel issueEvent of
          Just (IssueLabel {labelName=(N name), labelColor=hexColor}) -> [(5, str " the "), (safeWctwidth name + 2, labelPill hexColor name), (7, str " label")]
          Nothing -> [(8, str " a label")]
        Referenced -> case issueEventCommitId issueEvent of
          Just sha -> let short = T.take 7 sha in [(11, str " in commit "), (safeWctwidth short, withAttr hashAttr (str (toString short)))]
          Nothing -> []
        CrossReferenced -> case issueEventSourceIssue issueEvent of
          Just sourceIssue ->
            let num = show (unIssueNumber (issueNumber sourceIssue))
                ref = "#" <> num
                title = toString (issueTitle sourceIssue)
                quoted = "\"" <> title <> "\""
            in [(5, str " from "), (safeWcswidth ref, withAttr hashNumberAttr (str ref)), (1 + safeWcswidth quoted, str " " <+> withAttr normalAttr (str quoted))]
          Nothing -> []
        Committed -> case issueEventCommitId issueEvent of
          Just sha -> let short = T.take 7 sha in [(1 + safeWctwidth short, str " " <+> withAttr hashAttr (str (toString short)))]
          Nothing -> []
        ReviewRequested -> case issueEventRequestedReviewer issueEvent of
          Just (SimpleUser {simpleUserLogin=(N reviewer)}) -> [(1 + safeWctwidth reviewer, str " " <+> withAttr usernameAttr (str (toString reviewer)))]
          Nothing -> []
        _ -> []
      eventText = getEventDescription (issueEventType issueEvent)
      iconWidget = getEventIconWithColor (issueEventType issueEvent)
      timeAgo = timeFromNow (diffUTCTime now (issueEventCreatedAt issueEvent))
      timeAgoWidget = (safeWcswidth timeAgo, withAttr italicText $ str timeAgo)
      descriptionItems = [(safeWcswidth eventText, str eventText)] ++ eventSuffixItems ++ [(1, str " "), timeAgoWidget]
      prefixWidth = 1 + 2 + safeWctwidth actorName + 1  -- icon + spaces + username + space
      eventLine = adaptiveWidth $ \w ->
        let availableWidth = w - 4
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

renderLabelGroup :: UTCTime -> Bool -> IssueEvent -> [IssueEvent] -> [IssueEvent] -> Widget n
renderLabelGroup now isLast rep added removed =
  let actorName :: Text = case issueEventActor rep of
        Just (SimpleUser {simpleUserLogin=(N username)}) -> username
        Nothing -> "ghost"
      timeAgo = timeFromNow (diffUTCTime now (issueEventCreatedAt rep))
      iconWidget = getEventIconWithColor (issueEventType rep)
      addedItems = mapMaybe eventLabelPillWithWidth added
      removedItems = mapMaybe eventLabelPillWithWidth removed
      totalLabels = length added + length removed
      labelWord = if totalLabels > 1 then "labels " else "label "
      timeAgoWidget = (safeWcswidth timeAgo, withAttr italicText $ str timeAgo)
      descriptionItems = case (addedItems, removedItems) of
        ([], rs) -> (8, str "removed ") : rs ++ [(safeWcswidth labelWord, str labelWord), timeAgoWidget]
        (as, []) -> (6, str "added ") : as ++ [(safeWcswidth labelWord, str labelWord), timeAgoWidget]
        (as, rs) -> (6, str "added ") : as ++ [(13, str " and removed ")] ++ rs ++ [(safeWcswidth labelWord, str labelWord), timeAgoWidget]
      -- Prefix on first line: 1 icon + 2 spaces + username + 1 space
      prefixWidth = 1 + 2 + safeWctwidth actorName + 1
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
  Just (IssueLabel {labelName=(N name), labelColor=hexColor}) -> Just (safeWctwidth name + 3, labelPill hexColor name <+> str " ")
  Nothing -> Nothing

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
