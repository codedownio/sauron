module Sauron.UI.Syntax (
  syntaxMap
  , syntaxForFilename
  , syntaxForLanguage
  ) where

import qualified Data.Map as M
import Relude
import qualified Skylighting as Sky


-- | Skylighting's bundled syntaxes, plus a few extensions it doesn't claim pointed at
-- the closest syntax it does have. The one that actually comes up is .tsx: there's no
-- TSX definition at all, and the JSX one handles both the tags and most of the
-- TypeScript around them.
syntaxMap :: Sky.SyntaxMap
syntaxMap = foldl' addAlias Sky.defaultSyntaxMap aliases
  where
    addAlias m (ext, name) = case Sky.lookupSyntax name m of
      Nothing -> m
      Just syntax -> M.insert ext (syntax { Sky.sExtensions = ("*." <> toString ext) : Sky.sExtensions syntax }) m

aliases :: [(Text, Text)]
aliases = [
  ("tsx", "JavaScript React (JSX)")
  , ("mdx", "Markdown")
  , ("gql", "GraphQL")
  , ("jsonc", "JSON")
  ]

syntaxForFilename :: Text -> Maybe Sky.Syntax
syntaxForFilename = listToMaybe . Sky.syntaxesByFilename syntaxMap . toString

syntaxForLanguage :: Text -> Maybe Sky.Syntax
syntaxForLanguage = flip Sky.lookupSyntax syntaxMap
