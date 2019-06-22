{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unit.LAB (tests) where

import           Data.Color
import           Data.Color.Spaces.LAB
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit as HUnit
import           Test.HUnit                     ((@?=))
import           Helpers.Utils
import           Helpers.LABColors
import           Helpers.XYZColors

-- We only test in one direction here. These verify the conversion
-- formulae. QuickCheck will verify that conversion is symmetric.

lab50 :: LABSpace
lab50 = LABSpace illD50

testToXYZFromLAB50 :: (LChromaData, XYZData) -> Test
testToXYZFromLAB50 (c, expected) = testCase label $ (roundColor (toXYZ lab50 c)) @?= expected
  where label = "Convert " ++ (show c) ++ " to XYZ from LAB50"

toXYZFromLAB50Tests :: [Test]
toXYZFromLAB50Tests = map testToXYZFromLAB50 (zip labColors xyzColors)

tests :: [Test]
tests = toXYZFromLAB50Tests
