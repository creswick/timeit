{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Time.Clock
import Data.List
import Data.Configurator

import System.Directory
import System.Environment
import System.FilePath
import System.Process

main :: IO ()
main = do
  args <- parseArgs
  conf <- loadConfiguration
  start <- getCurrentTime
  (_,_,_,hdl) <- createProcess $ shell $ command args
  _exit <- waitForProcess hdl
  end <- getCurrentTime

  saveTiming conf args $ diffUTCTime end start

saveTiming :: Configuration -> Arguments -> NominalDiffTime -> IO ()
saveTiming conf args theTime =
  appendFile (storage conf)
    (show (round (theTime * 1000)) ++ ", " ++ command args ++ "\n")

parseArgs :: IO Arguments
parseArgs = do
  args <- getArgs
  return Args { command = intercalate " " args
              , verbose = False
              }

-- | Command line arguments.
data Arguments = Args
  { command :: String
  , verbose :: Bool
  } deriving (Read, Show, Ord, Eq)

-- | Command line arguments.
data Configuration = Configuration
  { storage :: FilePath
  } deriving (Read, Show, Ord, Eq)

loadConfiguration :: IO Configuration
loadConfiguration = do
  homeDir <- getHomeDirectory
  config <- load [ Optional $ homeDir </> ".timings.cfg" ]
  let defaultStorage = homeDir </> ".timings.csv"
  store <- lookupDefault defaultStorage config "storage"
  return Configuration
         { storage = store
         }
