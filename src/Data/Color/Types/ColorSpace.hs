{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE DefaultSignatures       #-}

module Data.Color.Types.ColorSpace where

import Data.Color.Types.Color
import Data.Color.Models.XYZ
import Data.Matrix
import Data.Maybe

{- | The @ColorSpace@ is what defines the transformations from
itself to @CIEXYZ@ and back again. In a sense, that's all a
color space actually is: an unambiguous coordinate transform.
-}
class Color (ColorType s) => ColorSpace s where
  -- | Convert color from @CIE XYZ@
  fromXYZ :: s -> XYZData -> ColorType s

  -- | Convert color to @CIE XYZ@
  toXYZ :: s -> ColorType s -> XYZData

  default toXYZ :: (MatrixConvertible s) => s -> ColorType s -> XYZData
  toXYZ s c = (XYZ (x, y, z))
    where (x:y:z:_) = toList $ (reverseMatrix s) * (fromList 3 1 (components (toLinear s c)))

  default fromXYZ :: (MatrixConvertible s) => s -> XYZData -> ColorType s
  fromXYZ s c = fromLinear s $ fromJust $ fromComponents $ toList $ (forwardMatrix s) * (fromList 3 1 (components c))

-- | Convert a color from a colorspace to another colorspace
convert :: (ColorSpace s1, ColorSpace s2) => s1 -> s2 -> (ColorType s1) -> (ColorType s2)
convert s1 s2 c = fromXYZ s2 (toXYZ s1 c)

{- | @ColorSpace@s which are linear transformations of @CIEXYZ@ can be
constructed in part as a coordinate transformation matrix which, when
multipled by a color, yields the representation of that color in the
opposite space.

Aside from the overall transformation, a color space can define its
peculiar response curves, which make better use of the coordinate space
by non-linearly mapping values so that, for instance, more of the range
can be devoted to darker colors. This is called "gamma". Historically
this has been coincident with correcting for the physical
characteristics of CRT phosphor responses, but they are unrelated in
their causes and purposes.

In modern usage, the purpose of gamma correction is simply a
matter of low precision of the final representation type combined with
the non-linearity of human vision. When encoding a color, it could
be the case (and, roughly speaking, it is the case) that an observer
perceives the same relative difference between the top 75% of values
as they they do between the bottom 25%. When dealing with floating
point values, this doesn't matter—encoding inefficiencies are
already baked into the type, and there are plenty of bits to
distinguish dark shades. With 8-bit color, however, we're limited to
256 discrete values. When quantized like that, detail is lost in
ranges that are compressed relative to the sensitivity of human vision.

What the gamma curves do is spread darker colors over more of the
range and squeeze brighter colors into less of the range, so that
the darkest ~25% of values get ~50% of the range.

What is important to understand is that this is not about changing
the appearance of the colors, it is simply a matter of encoding
efficiency, given the low precision of the destination type.
Ordinarily, this would entail an entirely separate concern, but
due to a combination of historical and technical reasons it is
assumed that all colors will, sooner or later, be stored as integers
with only a handful of bits per channel, and standardizing the manner
of most efficiently using those bits is a key concern of color
management systems.
-}
class (ColorSpace s) => MatrixConvertible s where
  forwardMatrix :: s -> Matrix Double
  reverseMatrix :: s -> Matrix Double

  -- | Transform gamma-corrected color to linear color
  toLinear :: s -> ColorType s -> ColorType s
  toLinear _ c = c

  -- | Transform linear color to gamma-corrected color
  fromLinear :: s -> ColorType s -> ColorType s
  fromLinear _ c = c

