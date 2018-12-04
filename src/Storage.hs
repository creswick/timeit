{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Storage where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Maybe
import GHC.Stack (HasCallStack)
import Data.Time.Clock
import Data.String.Utils (strip)

import Statistics.Sample

import Types

saveTiming :: Configuration -> RunInfo -> IO ()
saveTiming conf info =
  let scaleFactor = 1000
  in appendFile (storage conf)
    (show (round ((riTime info) * scaleFactor)::Int) ++ ", " ++ riCommand info ++ "\n")


-- | Load all runs; this may become expensive.
loadTiming :: Configuration -> IO [RunInfo]
loadTiming conf = do
  csvData <- BL.readFile (storage conf)
  case decode NoHeader csvData of
    Left err -> error (unlines [ "Could not decode storage."
                               , "  storage: "++storage conf
                               , "  error: "++err
                               ])
    Right v -> return $ V.toList ((\(time::NominalDiffTime, cmd :: String) -> RunInfo (strip cmd) time) <$> v)


instance FromField NominalDiffTime where
  parseField s = do
    count <- parseField s :: Parser Int
    pure (fromRational $ (toRational count) / 1000)

timesForCommand :: String -> [RunInfo] -> [NominalDiffTime]
timesForCommand cmd xs = mapMaybe maybeTime xs
  where
    maybeTime :: RunInfo -> Maybe NominalDiffTime
    maybeTime (RunInfo c t) | (strip c) == (strip cmd) = Just t
                            | otherwise = Nothing

statsForCommand :: HasCallStack => [NominalDiffTime] -> Stats
statsForCommand times =
  let realTimes :: [Double]
      realTimes = realToFrac <$> times
  in Stats
  { stCount = length times
  , stMax = maximum times
  , stMin = minimum times
  , stMean = mean $ V.fromList realTimes
  , stStdDev = stdDev $ V.fromList realTimes
  }
