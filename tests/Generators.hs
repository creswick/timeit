{-# OPTIONS_GHC -Wno-orphans #-}
module Generators where

import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Modifiers ()

import Data.Time.Clock

import Types
import Storage
import Histogram

instance Arbitrary RunInfo where
  arbitrary = RunInfo <$> arbitrary <*> arbitrary

instance Arbitrary Stats where
  arbitrary = do
    times <- arbitrary :: Gen [NonNegative NominalDiffTime]
    pure $ statsForCommand (map getNonNegative times)

instance Arbitrary Histogram where
  arbitrary = do
    times <- arbitrary :: Gen [NonNegative Double]
    pure $ mkHistogram (map getNonNegative times)
