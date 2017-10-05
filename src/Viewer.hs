{-# LANGUAGE OverloadedStrings #-}
module Viewer (runApp, app) where

import Data.Aeson (Value(..), object, (.=))
import Network.Wai (Application)
import Network
import qualified Web.Scotty as S
import Data.Text.Lazy as Text
import Converter
import Network.HTTP.Types.Status

app :: IO Application
app = S.scottyApp app'

runApp :: Int -> IO ()
runApp p = S.scotty (fromIntegral p) app'

app' :: S.ScottyM ()
app' = do
  S.post "/render" $ do
    r <- extractParam "format"
    body <- S.body
    case convertToHTML r body of
      Left errMsg -> do
        S.status status500
        S.text errMsg
        S.finish
      Right result -> S.text result


extractParam :: Text -> S.ActionM InputType
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
instance S.Parsable InputType where
  parseParam msg =
    case toLower msg of
      "markdown"   -> Right InputMarkdown
      "mediawiki"  -> Right InputMediaWiki
      "commonmark" -> Right InputCommonMark
      _            -> Left "Uknown input format"
