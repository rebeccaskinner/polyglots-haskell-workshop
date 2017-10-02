{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Viewer (runApp)
import Data.Data
import Network

data Config = Config { listenPort :: Int
                     } deriving (Show, Data, Typeable)

main :: IO ()
main = runApp 8080
