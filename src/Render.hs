module Render where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import Text.PrettyPrint

import Types
import Histogram

renderStats :: (NominalDiffTime, Stats) -> Doc
renderStats (thisRun, stats) =
  vcat [ text "This took: " <+> (diffTime thisRun)
       , text "     mean: " <+> (double (stMean stats))
       , text "   stdDev: " <+> (double (stStdDev stats))
       , text "      min: " <+> (diffTime (stMin stats))
       , text "      max: " <+> (diffTime (stMax stats))
       , text "    count: " <+> (int (stCount stats))
       ]

renderHistogram :: Histogram -> Doc
renderHistogram hist = vcat (map text (toStrings hist Nothing))

diffTime :: NominalDiffTime -> Doc
diffTime t = text (formatTime defaultTimeLocale "%H:%M:%S:%Q" $ posixSecondsToUTCTime t)
