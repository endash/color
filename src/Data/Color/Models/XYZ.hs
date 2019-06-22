{-# LANGUAGE TypeFamilies            #-}

module Data.Color.Models.XYZ where

import Data.Color.Types.Color

{- | A point in the @CIE XYZ@ color space.

@CIE XYZ@ can be considered the ur-color-space, through which all
other derived spaces can be converted. As such, it is imported at
the top-level, and used elsewhere, as a primitive. The @XYZ@
components are referred to as "tristimulus values."

This is essentially an abstract space derived as a linear
transformation of the earlier, emperically-constructed @CIE RGB@
color space. Many other color spaces can be defined, in part, as
linear transformations of @CIE XYZ@. This explains the use of
3x3 matrices in the conversions. Others are non-linear but still
mathematically related to @CIE XYZ@ via specified formulae.

@CIE XYZ@ has a number of advantages over @CIE RGB@, at least
from the perspective of an engineer in 1931:

0) Not an advantage by design, but for our purposes the relief at
  sidestepping the naming confusion between @CIE RGB@ and other
  RGB spaces is not insignificant.

1) The coordinates are always positive. The derived chromaticity
  @x, y@ coordinates are always in the range [0, 1].

  Essentially what the CIERGB-to-CIEXYZ matrix does is rotate,
  scale, and skew the coordinates from the @r-g@ chromaticity
  plane so that the entire gamut fits within the triangle defined
  by [0, 0], [0, 1], and [1, 0]. This explains the particular shape
  of the matrix, in which the last row is pretty close to [0, 0, 1],
  approximating an affine transformation. This pecularity—a 3D space
  being transformed by a 2D affine matrix—is due to @B@ being held
  mostly constant, which reduces the 3D linear transformation to a 2D
  affine transformation in the @R-G@ plane. (Note that the fact that
  the last row is *not* exactly [0, 0, 1] means it is not a perfect
  affine transformation. In the 2D plane, it is a perspective
  transformation. As the name implies, a perspective transformation
  can distort relative distances that an affine transformation would
  hold constant. Straight lines, however, remain straight.)

  This requires the tristimulus values to also be in all cases
  positive, although not necessarily so limited in range. In
  particular, because the maximum luminosity is fixed by definition
  to 1.0, very bright colors (such as the standard illuminants) can
  have (at most) one other value greater than 1.0.

2) The transformation was chosen such that @Y@ = @Luminosity@.
  When viewed as a chromaticity diagram in the original @r-g@
  plane, this manifests as a new red primary that is significantly
  to the right of the @RGB@ red primary and below the axis. The
  negative slope this gives the line between the new red and blue
  primaries becomes the new @x@ axis, which is what gives the line
  of purples positive slope in the new @x-y@ plane.

3) The constant energy white point (standard illuminant @E@) was
  required to have equal @XYZ@ component values, which had the
  practical result of normalizing the contributions of each of the
  pimaries.
-}

newtype XYZData = XYZ (Double, Double, Double)
  deriving (Eq, Ord, Show)

instance Color XYZData where
  components (XYZ (x, y, z)) = [x, y, z]
  fromComponents (x:y:z:_) = Just (XYZ (x, y, z))
  fromComponents _         = Nothing

