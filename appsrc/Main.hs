{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Time.Clock
import Data.List
import Data.Configurator


import Prelude hiding ((<>))
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.PrettyPrint

import Types
import Storage
import Render
import Histogram

main :: IO ()
main = do
  args <- parseArgs
  conf <- loadConfiguration
  start <- getCurrentTime
  (_,_,_,hdl) <- createProcess $ shell $ command args
  exitCode <- waitForProcess hdl
  end <- getCurrentTime
  let duration = diffUTCTime end start
  saveTiming conf (RunInfo (command args) duration)
  runInfo <- loadTiming conf
  reportStats (duration, timesForCommand (command args) runInfo)
  exitWith exitCode

reportStats :: (NominalDiffTime, [NominalDiffTime]) -> IO ()
reportStats (thisRun, times) = do
  let stats = statsForCommand times
  putStrLn ""
  putStrLn (render (blockBeside (renderStats (thisRun, stats)) (renderHistogram (mkHistogram (map realToFrac times)))))


parseArgs :: IO Arguments
parseArgs = do
  args <- getArgs
  return Args { command = intercalate " " args
              , verbose = False
              }

loadConfiguration :: IO Configuration
loadConfiguration = do
  homeDir <- getHomeDirectory
  config <- load [ Optional $ homeDir </> ".timings.cfg" ]
  let defaultStorage = homeDir </> ".timings.csv"
  store <- lookupDefault defaultStorage config "storage"
  return Configuration
         { storage = store
         }
