
module ToolUse.HtmlParser
  ( cleanHtml
  ) where

import Text.HTML.TagSoup

removeSections :: (Tag String -> Bool) -> [Tag String] -> [Tag String]
removeSections _ [] = []
removeSections isStartTag (tag:tags)
  | isStartTag tag = removeSections isStartTag (dropWhile (not . isEndTag tag) tags)
  | otherwise = tag : removeSections isStartTag tags
  where
    isEndTag startTag t =
      case (startTag, t) of
        (TagOpen name _, TagClose endName) -> name == endName
        _ -> False

filterTags :: [Tag String] -> [Tag String]
filterTags = filter (not . isUnwantedTag)
  . removeSections (\t -> isTagOpenName "script" t || isTagOpenName "style" t)
  . concatMap (takeWhile (~/= "</body>"))
  . sections (~== "<body>")

unwantedTags :: [String]
unwantedTags = ["script", "style", "noscript", "head", "link", "meta", "iframe", "object", "embed", "applet", "form", "input", "button", "select", "option", "footer", "header", "nav", "aside", "section", "canvas", "svg", "blockquote", "q"]

isUnwantedTag :: Tag String -> Bool
isUnwantedTag (TagOpen name _) = name `elem` unwantedTags
isUnwantedTag (TagClose name) = name `elem` unwantedTags
isUnwantedTag _               = False

extractText :: [Tag String] -> [String]
extractText = map fromTagText . filter isTagText

cleanHtml :: Maybe String -> Maybe String
cleanHtml str = 
  case str of
    Nothing -> Nothing
    Just s -> 
      let tags = parseTags s
          filteredTags = filterTags tags
          readableText = extractText filteredTags
      in Just $ unlines readableText
