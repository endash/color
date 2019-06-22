{-# LANGUAGE TypeFamilies            #-}

module Data.Color.Spaces.RGB (
  module Data.Color.Models.RGB,
  RGBSpace(..),
  srgb
) where

import Data.Color.Types
import Data.Color.Types.ParametricCurve
import Data.Color.Models.RGB
import Data.Matrix

{- | An RGB color space defined by red, green, and blue primaries,
each with a parametric curve (as defined by the ICC spec.)

N.B.: Some spaces (e.g., sRGB) are endowed by their standard with a
"reference white point." It can be easy to get caught up in formulae
and whatnot and forget that in these spaces the white point is
simply the three primaries added together. Changing the white point
changes the primaries, and vice versa. This is why the sRGB values
in the ICC profile are different from the reference. The ICC
standard specifies a different whitepoint, so the primaries have
been adjusted so that "white" is a slightly different color.
-}
data RGBSpace = RGBSpace (XYZData, ParametricCurve) (XYZData, ParametricCurve) (XYZData, ParametricCurve)

type instance ColorType RGBSpace = RGBData

instance ColorSpace RGBSpace

-- | An RGB color space is a linear transformation from @CIE XYZ@
-- so we can construct a transformation matrix from the RGB primaries
instance MatrixConvertible RGBSpace where
  forwardMatrix s = let (Right i) = inverse (reverseMatrix s)
                    in  i

  reverseMatrix (RGBSpace (r, _) (g, _) (b, _)) = transpose $ fromLists (fmap components [r, g, b])

  fromLinear (RGBSpace (_, rCurve) (_, gCurve) (_, bCurve)) (RGB (r, g, b)) = RGB ((forwardGamma rCurve r), (forwardGamma gCurve g), (forwardGamma bCurve b))

  toLinear (RGBSpace (_, rCurve) (_, gCurve) (_, bCurve)) (RGB (r, g, b)) = RGB ((reverseGamma rCurve r), (reverseGamma gCurve g), (reverseGamma bCurve b))

-- | The sRGB space. Note that the primaries defined here are not the
-- same as what you'll see if you pull up the sRGB ICC profile. Those
-- values have been transformed into "profile connection space," which
-- requires a white point adjustment from D65 to D50.
srgb :: RGBSpace
srgb = RGBSpace (rPrimary, curve) (gPrimary, curve) (bPrimary, curve)
  where
    rPrimary = XYZ (0.4124, 0.2126, 0.0193)
    gPrimary = XYZ (0.3576, 0.7152, 0.1192)
    bPrimary = XYZ (0.1805, 0.0722, 0.9504)
    curve    = Type3 2.4 (1 / 1.055) (0.055 / 1.055) (1 / 12.92) 0.04045
