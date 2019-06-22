module Data.Color.StandardIlluminants where

import Data.Color.Types

illD65 :: XYZData
illD65 = XYZ (0.9505, 1.0000, 1.0889)

illD55 :: XYZData
illD55 = XYZ (0.9568, 1.0000, 0.9214)

illD50 :: XYZData
illD50 = XYZ (0.9642, 1.0000, 0.8251)

illE :: XYZData
illE = XYZ (1.0000, 1.0000, 1.0000)

illC :: XYZData
illC = XYZ (0.9807, 1.0000, 1.1822)

illA :: XYZData
illA = XYZ (1.0985, 1.0000, 0.3558)
