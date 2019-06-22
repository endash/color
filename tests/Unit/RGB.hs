{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unit.RGB (tests) where

import           Data.Color
import           Data.Color.Spaces.RGB
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit as HUnit
import           Test.HUnit                     ((@?=))
import           Helpers.Utils
import           Helpers.RGBColors
import           Helpers.XYZColors

-- We only test in one direction here. These verify the conversion
-- formulae. QuickCheck will verify that conversion is symmetric.

testToXYZFromSRGB :: (RGBData, XYZData) -> Test
testToXYZFromSRGB (c, expected) = testCase label $ (roundColor (toXYZ srgb c)) @?= expected
  where label = "Convert " ++ (show (roundColor c)) ++ " to XYZ from sRGB"

toXYZFromSRGBTests :: [Test]
toXYZFromSRGBTests = map testToXYZFromSRGB (zip rgbColors xyzColors)

tests :: [Test]
tests = toXYZFromSRGBTests
