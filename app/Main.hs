{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Viewer (runApp)
import Data.Data
import Network
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe)
import System.IO.Error (catchIOError)

main :: IO ()
main = runApp 8080

-- getPortParam tries to get a parameter from the command line
getPortParam :: IO (Maybe Int)
getPortParam = do
  hd <- listToMaybe <$> getArgs
  return $ case hd of
    Nothing  -> Nothing
    Just hd' -> Just 8080

-- readMaybe tries to parse a string into a readable type
readMaybe :: Read a => String -> IO (Maybe a)
readMaybe s = catchIOError (Just <$> readIO s) (\_ -> return Nothing)
