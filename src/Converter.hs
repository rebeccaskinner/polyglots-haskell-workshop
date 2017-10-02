module Converter where

import Data.Text

data InputType = Input_Markdown
               | Input_Latex
               | Input_MediaWiki
               | Input_CommonMark deriving (Eq)

instance Show InputType where
  show Input_Markdown = "markdown"
  show Input_Latex = "LaTeX"
  show Input_MediaWiki = "MediaWiki"
  show Input_CommonMark = "CommonMark"


convertToHTML :: InputType -> Text -> Either Text Text
convertToHTML _ input = Right input
