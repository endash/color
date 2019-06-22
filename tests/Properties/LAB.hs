{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Properties.LAB (tests) where

import           Control.Monad                        (liftM)
import           Data.Color
import           Data.Color.Spaces.LAB
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 as QuickCheck
import           Test.QuickCheck
import           Helpers.Utils

randomLAB :: Gen (Double, Double, Double)
randomLAB = do
  l <- choose (0, 1)
  a <- choose (-1, 1)
  b <- choose (-1, 1)
  return (l, a, b)

instance Arbitrary LChromaData where
  arbitrary = liftM LChroma randomLAB

labSpace :: LABSpace
labSpace = LABSpace illD50

testLABXYZSymmetry :: LChromaData -> Bool
testLABXYZSymmetry c = (roundColor (fromXYZ labSpace (toXYZ labSpace c))) == (roundColor c)

tests :: [Test]
tests = [QuickCheck.testProperty "LAB <-> XYZ" testLABXYZSymmetry]
