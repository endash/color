{-# LANGUAGE TypeFamilies            #-}

module Data.Color.Spaces.LAB (
  module Data.Color.Models.LChroma,
  LABSpace(..)
) where

import Data.Color.Types
import Data.Color.Models.LChroma

{- | A L*a*b* color space, made absolute with a white point

Note that @CIELAB@ is properly a "color appearance model" and attempts
to account for chromatic adaptation in its definition. This is where
the white point comes into play: values are normalized by dividing by
the matching white point value. Recall that the @CIEXYZ@ space is
defined such that the three tristimuli contribute equally, in a sense
resulting in a natural white point equal to the equal energy point
(standard illuminant E.) What this does, then, in essence is rescale
the space so that the three stimuli *don't* contribute equally, instead
contributing in proportion to the new white point's values.

This is a variant of the von Kries CAT performed in XYZ space. More
sophisticated CATs, at a minimum, transform into LMS space first. If
you want to use the L*a*b* space but perform a more accurate CAT, you
can use (XYZ (1, 1, 1)) as the white point for @LABSpace@.
-}
data LABSpace = LABSpace XYZData
  deriving (Eq, Ord, Show)

type instance ColorType LABSpace = LChromaData

-- | These conversions are based on formulae from wikipedia:
-- https://en.wikipedia.org/wiki/CIELAB_color_space, modified to
-- account for the [0, 1] range.
instance ColorSpace LABSpace where
  fromXYZ _             (XYZ (0, 0, 0)) = LChroma (0, 0, 0)
  fromXYZ (LABSpace wp) c               = LChroma (l, a, b)
    where
      (XYZ (xWP, yWP, zWP))  = wp
      (XYZ (x, y, z))        = c
      y'                     = (forwardTransform (y / yWP))
      l                      = 1.16 * y' - 0.16
      a                      = 5 * ((forwardTransform (x / xWP)) - y')
      b                      = 2 * (y' - (forwardTransform (z / zWP)))

  toXYZ _             (LChroma (0, 0, 0)) = XYZ (0, 0, 0)
  toXYZ (LABSpace wp) c                   = XYZ (x, y, z)
    where
      (XYZ (xWP, yWP, zWP))  = wp
      (LChroma (l, a, b))    = c
      l'                     = ((l + 0.16) / 1.16)
      x                      = xWP * (reverseTransform (l' + (a / 5)))
      y                      = yWP * (reverseTransform l')
      z                      = zWP * (reverseTransform (l' - (b / 2)))

forwardTransform :: Double -> Double
forwardTransform t | t > ((6 / 29) ** 3) = t ** (1 / 3)
                   | otherwise           = (t / (3 * ((6 / 29) ** 2))) + (4 / 29)

reverseTransform :: Double -> Double
reverseTransform t | t > (6 / 29) = t ** 3
                   | otherwise    = 3 * ((6 / 29) ** 2) * (t - (4 / 29))
