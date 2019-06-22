{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Properties.LUV (tests) where

import           Control.Monad                        (liftM)
import           Data.Color
import           Data.Color.Spaces.LUV
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 as QuickCheck
import           Test.QuickCheck
import           Helpers.Utils

randomLUV :: Gen (Double, Double, Double)
randomLUV = do
  l <- choose (0, 1)
  u <- choose ((-1), 1)
  v <- choose ((-1), 1)
  return (l, u, v)

instance Arbitrary LChromaData where
  arbitrary = liftM LChroma randomLUV

luvSpace :: LUVSpace
luvSpace = LUVSpace illD50

testLUVXYZSymmetry :: LChromaData -> Bool
testLUVXYZSymmetry c = (roundColor (fromXYZ luvSpace (toXYZ luvSpace c))) == (roundColor c)

tests :: [Test]
tests = [QuickCheck.testProperty "LUV <-> XYZ" testLUVXYZSymmetry]
