module Render where

import Data.List
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import Text.PrettyPrint
import Text.Printf

import Types
import Histogram

renderStats :: (NominalDiffTime, Stats) -> [String]
renderStats (thisRun, stats) =
   [ "  This: " ++ (diffTime thisRun)
   , "  mean: " ++ (doubleP2 (stMean stats))
   , "stdDev: " ++ (doubleP2 (stStdDev stats))
   , "   min: " ++ (diffTime (stMin stats))
   , "   max: " ++ (diffTime (stMax stats))
   , " count: " ++ (show (stCount stats))
   ]

renderHistogram :: Histogram -> [String]
renderHistogram hist = toStrings hist Nothing

diffTime :: NominalDiffTime -> String
diffTime t = formatTime defaultTimeLocale "%H:%M:%S" $ posixSecondsToUTCTime t

doubleP2 :: Double -> String
doubleP2 d = printf "%.2f" d

-- | Make a list of strings all have the same length (equal to the longest)
rightPad :: [String] -> [String]
rightPad xs =
  let fullLen = maximum $ map length xs
  in map (pad fullLen) xs

-- | Right-pad one string to the specified total length.
pad :: Int -> String -> String
pad l s | length s >= l = s
        | otherwise     = s ++ replicate (l - length s) ' '


-- | Place two lists of strings side-by-side.
blockBeside :: [String] -> [String] -> Doc
blockBeside left right =
  let leftBlock = map (\t-> text (t ++ " |")) $ rightPad left

      rightBlock :: [Doc]
      rightBlock = map text $ right

      allText :: [[Doc]]
      allText = [leftBlock, rightBlock]

      flipped = transpose allText

      zipped :: [Doc]
      zipped = zipWith (<+>) leftBlock rightBlock
  in vcat zipped
