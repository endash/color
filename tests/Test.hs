{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import qualified Unit.RGB           as RGBUnit
import qualified Unit.LAB           as LABUnit
import qualified Unit.LUV           as LUVUnit
import qualified Properties.RGB     as RGBProps
import qualified Properties.LAB     as LABProps
import qualified Properties.LUV     as LUVProps
import qualified Properties.LChroma as LChromaProps
import           Test.Framework (Test, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testGroup "RGB Conversion" RGBUnit.tests,
    testGroup "LAB Conversion" LABUnit.tests,
    testGroup "LUV Conversion" LUVUnit.tests,
    testGroup "RGB Properties" RGBProps.tests,
    testGroup "LAB Properties" LABProps.tests,
    testGroup "LUV Properties" LUVProps.tests,
    testGroup "LChroma Properties" LChromaProps.tests
  ]
