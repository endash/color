{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unit.LUV (tests) where

import           Data.Color
import           Data.Color.Spaces.LUV
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit as HUnit
import           Test.HUnit                     ((@?=))
import           Helpers.Utils
import           Helpers.LUVColors
import           Helpers.XYZColors

-- We only test in one direction here. These verify the conversion
-- formulae. QuickCheck will verify that conversion is symmetric.

luv50 :: LUVSpace
luv50 = LUVSpace illD50

testToXYZFromLUV50 :: (LChromaData, XYZData) -> Test
testToXYZFromLUV50 (c, expected) = testCase label $ (roundColor (toXYZ luv50 c)) @?= expected
  where label = "Convert " ++ (show c) ++ " to XYZ from LAB50"

toXYZFromLUV50Tests :: [Test]
toXYZFromLUV50Tests = map testToXYZFromLUV50 (zip luvColors xyzColors)

tests :: [Test]
tests = toXYZFromLUV50Tests
