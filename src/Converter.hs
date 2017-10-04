{-# LANGUAGE OverloadedStrings #-}
module Converter where
import Data.Text.Lazy
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy.Encoding
import Text.Pandoc
import Text.Pandoc.Readers.CommonMark
import Text.Pandoc.Readers.MediaWiki
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.LaTeX

data InputType = InputMarkdown
               | InputLatex
               | InputMediaWiki
               | InputCommonMark deriving (Eq)

instance Show InputType where
  show InputMarkdown = "markdown"
  show InputLatex = "LaTeX"
  show InputMediaWiki = "MediaWiki"
  show InputCommonMark = "CommonMark"

class ShowText a where
  showText :: a -> Text

instance ShowText InputType where
  showText InputMarkdown = "markdown"
  showText InputLatex = "LaTeX"
  showText InputMediaWiki = "MediaWiki"
  showText InputCommonMark = "CommonMark"

inputTypes :: [Text]
inputTypes = ["markdown" , "latex" , "mediawiki" , "commonmark"]

convertToHTML :: InputType -> B.ByteString -> Either Text Text
convertToHTML iType input =
  let input' = show input
      readerF = getReaderFunction iType
  in case readerF def input' of
    Left err -> Left $ handlePandocFailure err
    Right converted -> Right $ pack $ writeHtmlString def converted
  where
    handlePandocFailure (ParseFailure f) = pack f
    handlePandocFailure (ParsecError _ f) = pack $ show f

getReaderFunction :: InputType -> (ReaderOptions -> String -> Either PandocError Pandoc)
getReaderFunction InputMarkdown = readMarkdown
getReaderFunction InputLatex = readLaTeX
getReaderFunction InputMediaWiki = readMediaWiki
getReaderFunction InputCommonMark = readCommonMark
