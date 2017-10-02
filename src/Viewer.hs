{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Viewer (runApp, app) where

import Data.Aeson (Value(..), object, (.=))
import Network.Wai (Application)
import Network
import qualified Web.Scotty as S
import Data.Text.Lazy
import Converter

app' :: S.ScottyM ()
app' = do
  S.get "/" $ do
    S.text "hello"

  S.get "/render" $ do
    r <- (S.param "foo") :: S.ActionM Text
    S.text r

app :: IO Application
app = S.scottyApp app'

runApp :: PortNumber -> IO ()
runApp p = S.scotty (fromIntegral p) app'

-- Make Converter.InputType an instance of Parsable so that we can
-- read it in from the query param directly

instance S.Parsable InputType where
  parseParam msg =
    case (toLower msg) of
      "markdown"   -> Right Input_Markdown
      "latex"      -> Right Input_Latex
      "mediawiki"  -> Right Input_MediaWiki
      "commonmark" -> Right Input_CommonMark
      otherwise    -> Left "Uknown input format"
