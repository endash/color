{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Properties.LChroma (tests) where

import           Control.Monad                        (liftM)
import           Data.Color
import           Data.Color.Models.LChroma
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 as QuickCheck
import           Test.QuickCheck
import           Helpers.Utils

randomLChroma :: Gen (Double, Double, Double)
randomLChroma = do
  l <- choose (0, 1)
  a <- choose (-1, 1)
  b <- choose (-1, 1)
  return (l, a, b)

instance Arbitrary LChromaData where
  arbitrary = liftM LChroma randomLChroma

testLChromaLChromaHueSymmetry :: LChromaData -> Bool
testLChromaLChromaHueSymmetry c = (roundColor (fromCylindrical (toCylindrical c))) == (roundColor c)

testLChromaHueHueRange :: LChromaData -> Bool
testLChromaHueHueRange c = h >= 0 && h < (2 * pi)
  where (LChromaHue (h, _, _)) = toCylindrical c

tests :: [Test]
tests = [
    QuickCheck.testProperty "LChroma <-> LChromaHue" testLChromaLChromaHueSymmetry,
    QuickCheck.testProperty "LChromaHue hue ∈ [0, 2 * pi]" testLChromaHueHueRange
  ]
