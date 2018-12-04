module Types where

import Data.Time.Clock

-- | Command line arguments.
data Arguments = Args
  { command :: String
  , verbose :: Bool
  } deriving (Read, Show, Ord, Eq)

-- | Command line arguments.
data Configuration = Configuration
  { storage :: FilePath
  } deriving (Read, Show, Ord, Eq)

-- | One record of a run.
data RunInfo = RunInfo
  { riCommand :: String
  , riTime :: NominalDiffTime
  } deriving (Show, Ord, Eq)

data Stats = Stats
  { stCount :: Int
  , stMax :: NominalDiffTime
  , stMin :: NominalDiffTime
  , stMean :: Double
  , stStdDev :: Double
  } deriving (Show, Ord, Eq)
