module Main where

import Data.Time.Clock
import Data.List
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- parseArgs
  start <- getCurrentTime
  (_,_,_,hdl) <- createProcess $ shell $ command args
  _exit <- waitForProcess hdl
  end <- getCurrentTime

  saveTiming args $ diffUTCTime end start

saveTiming :: Arguments -> NominalDiffTime -> IO ()
saveTiming args theTime =
  appendFile (storage args)
    (show (round (theTime * 1000)) ++ ", " ++ command args ++ "\n")

parseArgs :: IO Arguments
parseArgs = do
  args <- getArgs
  return Args { command = intercalate " " args
              , storage = "/home/rogan/.timings.csv"
              }

-- | Command line arguments.
data Arguments = Args
  { command :: String
  , storage :: FilePath
  } deriving (Read, Show, Ord, Eq)
