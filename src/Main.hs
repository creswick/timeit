module Main where

import Data.Time.Clock
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- parseArgs
  start <- getCurrentTime
  createProcess $ shell $ command args
  end <- getCurrentTime

  saveTiming args $ diffUTCTime end start

saveTiming :: Arguments -> Int -> IO ()
saveTiming args theTime =
  writeFile (storage args) (show theTime ++ ", " ++ command args ++ "\n")

parseArgs :: IO Arguments
parseArgs = do
  args <- getArgs
  Args { command = args
       , storage = "/tmp/timings"
       }

-- | Command line arguments.
data Arguments = Args
  { command :: [String]
  , storage :: FilePath
  } deriving (Read, Show, Ord, Eq)
