{-# LANGUAGE TypeFamilies            #-}

module Data.Color.Spaces.LUV (
  module Data.Color.Models.LChroma,
  LUVSpace(..)
) where

import Data.Color.Types
import Data.Color.Models.LChroma

{- | A L*u*v* color space, made absolute with a white point.

Note that @CIELUV@ is properly a "color appearance model" and attempts
to account for chromatic adaptation in its definition. This is where
the white point comes into play: values are adjusted by normalizing the
luminosity against the chosen white point, and then recentering the
space at the chromacity of the white point.
-}
data LUVSpace = LUVSpace XYZData
  deriving (Eq, Ord, Show)

type instance ColorType LUVSpace = LChromaData

-- | These conversions are based on formulae from wikipedia:
-- https://en.wikipedia.org/wiki/CIELUV, modified to account
-- for the [0, 1] range.
instance ColorSpace LUVSpace where
  fromXYZ _             (XYZ (0, 0, 0)) = LChroma (0, 0, 0)
  fromXYZ (LUVSpace wp) c               = LChroma (l, u, v)
    where
      l                      = forwardL wp c
      u                      = 13 * l * ((uPrime c) - (uPrime wp))
      v                      = 13 * l * ((vPrime c) - (vPrime wp))

  toXYZ _             (LChroma (0, 0, 0)) = XYZ (0, 0, 0)
  toXYZ (LUVSpace wp) c                   = XYZ (x, y, z)
    where
      (LChroma (l, u, v))  = c
      y                    = reverseL wp c
      u'                   = (u / (13 * l)) + (uPrime wp)
      v'                   = (v / (13 * l)) + (vPrime wp)
      x                    = y * ((9 * u') / (4 * v'))
      z                    = y * ((12 - (3 * u') - (20 * v')) / (4 * v'))

uPrime :: XYZData -> Double
uPrime (XYZ (x, y, z)) = (4 * x) / (x + (15 * y) + (3 * z))

vPrime :: XYZData -> Double
vPrime (XYZ (x, y, z)) = (9 * y) / (x + (15 * y) + (3 * z))

forwardL :: XYZData -> XYZData -> Double
forwardL (XYZ (_, wpY, _)) (XYZ (_, y, _)) | (y / wpY) > ((6 / 29) ** 3) = 1.16 * ((y / wpY) ** (1/3)) - 0.16
                                           | otherwise                   = ((29 / 3) ** 3) * (y / wpY) / 100

reverseL :: XYZData -> LChromaData -> Double
reverseL (XYZ (_, wpY, _)) (LChroma (l, _, _)) | l < 0.08  = wpY * l * ((3 / 29) ** 3) * 100
                                               | otherwise = wpY * (((l + 0.16) / 1.16) ** 3)
