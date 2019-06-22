{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Properties.RGB (tests) where

import           Control.Monad                        (liftM)
import           Data.Color
import           Data.Color.Spaces.RGB
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 as QuickCheck
import           Test.QuickCheck
import           Helpers.Utils

randomRGB :: Gen (Double, Double, Double)
randomRGB = do
  r <- choose (0, 1)
  g <- choose (0, 1)
  b <- choose (0, 1)
  return (r, g, b)

instance Arbitrary RGBData where
  arbitrary = liftM RGB randomRGB

testRGBXYZSymmetry :: RGBData -> Bool
testRGBXYZSymmetry c = (roundColor (fromXYZ srgb (toXYZ srgb c))) == (roundColor c)

tests :: [Test]
tests = [QuickCheck.testProperty "RGB <-> XYZ" testRGBXYZSymmetry]
