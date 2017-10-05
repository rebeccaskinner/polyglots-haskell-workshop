{-# LANGUAGE OverloadedStrings #-}
module Converter where
import Data.Text.Lazy
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text.Lazy.Encoding
import Text.Pandoc
import Text.Pandoc.Readers.CommonMark
import Text.Pandoc.Readers.MediaWiki
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.LaTeX
import Text.Blaze.Renderer.Text

data DocumentType = InputMarkdown
               | InputMediaWiki
               | InputLaTeX
               | InputCommonMark deriving (Eq)

instance Show DocumentType where
  show InputMarkdown = "markdown"
  show InputMediaWiki = "MediaWiki"
  show InputCommonMark = "CommonMark"
  show InputLaTeX = "LaTeX"

class ShowText a where
  showText :: a -> Text

instance ShowText DocumentType where
  showText = pack . show

documentTypes :: [Text]
documentTypes =
  [ showText InputMarkdown
  , showText InputMediaWiki
  , showText InputCommonMark
  , showText InputLaTeX
  ]

convertToHTML :: DocumentType -> B.ByteString -> Either Text Text
convertToHTML iType input =
  let input' = B.unpack input
      readerF = getReaderFunction iType
  in case readerF def input' of
    Left err -> Left $ handlePandocFailure err
    Right converted -> Right $ mkHTML converted
  where
    handlePandocFailure (ParseFailure f) = pack f
    handlePandocFailure (ParsecError _ f) = pack $ "parsec error" ++ show f

mkHTML :: Pandoc -> Text
mkHTML p =
  let markup = writeHtml def p in
    renderMarkup markup

getReaderFunction :: DocumentType -> (ReaderOptions -> String -> Either PandocError Pandoc)
getReaderFunction InputMarkdown = readMarkdown
getReaderFunction InputMediaWiki = readMediaWiki
getReaderFunction InputCommonMark = readCommonMark
getReaderFunction InputLaTeX = readLaTeX
