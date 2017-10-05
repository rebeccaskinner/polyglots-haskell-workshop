{-# LANGUAGE OverloadedStrings #-}
module Viewer (runApp, app) where

import Data.Aeson (Value(..), object, (.=))
import Network.Wai (Application)
import Network
import qualified Web.Scotty as S
import Data.Text.Lazy as Text
import Converter
import Network.HTTP.Types.Status
import Control.Monad (unless)

app :: IO Application
app = S.scottyApp app'

runApp :: Int -> IO ()
runApp p = S.scotty (fromIntegral p) app'

app' :: S.ScottyM ()
app' = do

  S.get "/" $ S.file "frontend/index2.html"

  S.get "/supportedformats" $ S.json documentTypes
  S.get "/output-formats" $ S.json (["html"] :: [Text])


  S.post "/:render" $ do
    outputFormat <- S.param "render" :: S.ActionM Text
    unless (outputFormat == "html") $ do
      S.status status500
      S.text "invalid render type"
      S.finish
    r <- extractParam "format"
    body <- S.body
    case convertToHTML r body of
      Left errMsg -> do
        S.status status500
        S.text errMsg
        S.finish
      Right result -> S.text result


extractParam :: Text -> S.ActionM DocumentType
extractParam paramName = do
  rawText <- S.param paramName
  case S.parseParam rawText of
    Left errMsg -> do
      S.status status404
      S.text errMsg
      S.finish
    Right parsed -> return parsed

-- Make Converter.InputType an instance of Parsable so that we can
-- read it in from the query param directly
instance S.Parsable DocumentType where
  parseParam msg =
    case toLower msg of
      "markdown"   -> Right InputMarkdown
      "mediawiki"  -> Right InputMediaWiki
      "commonmark" -> Right InputCommonMark
      "latex"      -> Right InputLaTeX
      _            -> Left "Uknown input format"
