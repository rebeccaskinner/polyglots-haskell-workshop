{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Viewer (runApp)
import Data.Data
import Network
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe)
import System.IO.Error (catchIOError)

main :: IO ()
main = do
  port <- fromMaybe 8080 <$> getPortParam
  runApp port

getPortParam :: IO (Maybe Int)
getPortParam = do
  hd <- listToMaybe <$> getArgs
  case hd of
    Nothing -> return Nothing
    Just hd' -> readMaybe hd'

readMaybe :: Read a => String -> IO (Maybe a)
readMaybe s = catchIOError (Just <$> readIO s) (\_ -> return Nothing)
