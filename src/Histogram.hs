module Histogram where

import Data.Map (Map)
import qualified Data.Map as Map

data Histogram = Histogram
  { hRows :: Int
  , hCols :: Int
  , hNums :: Map Int Int
  } deriving (Read, Show, Ord, Eq)

mkHistogram :: [Double] -> Histogram
mkHistogram xs =
  let cols = 80
      rows = 5
      bucketed = toBuckets cols xs
      histData = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty bucketed

      vMin :: Double
      vMin = 0
      vMax = maximum (Map.elems histData)
      vScale = (vMax - vMin) / (fromIntegral (rows - 1))

      scaled = Map.map (\x -> round (x / vScale)) histData
  in Histogram rows cols scaled

toBuckets :: Int -> [Double] -> [Int]
toBuckets     _ [] = []
toBuckets count xs =
  let minVal = 0
      maxVal = maximum xs
      bucketSize = (maxVal - minVal) / (fromIntegral (count - 1 ))

      scale :: Double -> Int
      scale v = round (v / bucketSize)
  in map scale xs

toStrings :: Histogram -> Maybe Int -> [String]
toStrings (Histogram rows cols dat) mPoint =
  map (renderRow cols)
  [ Map.keys $ Map.filter (>= r) dat
  | r <- reverse [1..rows]
  ]

posToGaps :: [Int] -> [Int]
posToGaps xs = snd $ foldl (\(pos, acc) x -> (x + 1, acc ++ [gap x pos])) (0, []) xs
  where
    gap x pos = max 0 (x - pos)

renderRow :: Int -> [Int] -> String
renderRow end xs = let str = foldr (\x acc -> replicate x ' ' ++ "#" ++ acc) "" (posToGaps xs)
                   in if length str < end
                      then str ++ replicate (end - length str) ' '
                      else str


