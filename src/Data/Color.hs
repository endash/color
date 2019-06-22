module Data.Color (
  module Data.Color.Types,
  module Data.Color.StandardIlluminants
) where

import Data.Color.Types
import Data.Color.StandardIlluminants


{- $color_management

Color accuracy and conversion is complicated. There is no 100% perfect
or appropriate model or formulae for converting or adjusting colors
that can be applied in all cases.

* Working with RGB colors

The single most important thing to grok is that those RGB values you
have are, by themselves, meaningless. They're an abstraction. RGB might
as well be named Ribble, Gnarf, and Blorp for all they actually tell
you about what color will be produced. Only by fixing the meanings of
Red, Green, and Blue in physical terms (e.g., their spectral power
distributions or cone cell stimulus responses) can you actually assign
them anywhere near an absolute color. This can be trivially shown: the
same RGB values might be sky blue on one display, purplish on another
and a dark cyan on a third. The generic RGB space is device-dependent,
as the colors produced depend on the properties of the specific device
in question.

The next most important thing to grok is that this doesn't mean that
it is meaningless to want to do things with a color, just because it
doesn't exist in a perfect system of 100% accurate color management.
First, absent a good reason not to we can usually assume that an RGB
space is actually sRGB. Colors in sRGB space might not look exactly
identical to a generic RGB color on your display but they should be
roughly as close as they'll be on any other display, on average.

The cumulative result is that even if the original and final colors
continue to look different from display to display, the mathematical
relationship between them, performed in the other color space, should
continue to hold. By analogy, depending on where we're sitting in a
ballpark we might disagree as to exactly where a given ballplayer is
standing, in relation to a base. After he advances, we might continue
to disagree as to exactly where he is standing in relation to his new
base, but we'll absolutely agree that he has advanced to a different
base, in a particular direction.

Of course, we don't have to use sRGB to fix our colors. We could use
precise values derived empirically from our own display, in which case
the input and output colors will appear to match "generic" RGB colors.
The absolutely essential thing is that Red, Green, and Blue have their
values nailed down. It is utterly impossible to talk about converting
from RGB to another color space in any other sense. If there's a need
to simplify, then the simplest thing to do is just to assume sRGB.

* What about HSL or HSV?

These are what are technically called *bullshit* color spaces. Their
chief advantages are that they're computationally simple, and they
admit a Hue dimension which is more-or-less reasonably perceptual (but
still not uniform: colors with a given hue value will generally be
perceived as the same hue, but the primaries themselves would not be
equally spaced in a perceptually uniform space.) This is a trivial
symmetry of the underlying RGB cube, and its apparent success is a mere
illusion owing more to the extent to which RGB offends our intuitions
about how colors work than anything else.

Despite representations to the contrary, these are not perceptual color
spaces. Even for simple use cases like tinting or shading colors they
don't offer any advantage over linear mixing in RGB space. Their one
potential advantage over plain old RGB is colorful gradients: because
it's a cylindrical coordinate system, a "straight line" between two
colors curves around the center instead of through it, potentially
selecting more visually appealing intermediate colors by avoiding the
achromatic zone. Note, however, that for serious use cases, like data
visualization, cylindrical versions of @CIE L*u*v*@ are preferred as
it's actually possible to hold lightness and saturation constant in
that space (whereas they are both non-uniform and confounded in HSL/V)
and differences in hue are perceptually uniform, as well.

* What about ICC Profiles, then, don't those help?

ICC profiles are not directly modeled herein but reading the spec is
informative, and some types correspond closely to types used in ICC
profiles, like @ParametricCurve@. They're discussed here because of the
important role they play in color management in general, and their
outsized contribution to our understanding, rightly or wrongly, as to
how color management and color conversion work.

When it comes to displays, ICC profiles can be best understood as
definining a device-specific color space as a transformation of a
common absolute color space, called the Profile Connection Space. The
PCS is further defined as @CIE XYZ@, with chromatic adaptation applied
so as to shift the white point chromaticity to the D50 standard
illuminant. The specified default CAT is the linear Bradford model.

(Note: PCS is part of the ICC spec. There isn't anything inherently
advantangeous about shifting the white point of the XYZ space. If you
aren't actually dealing with converting colors between ICC profiles,
specifically, then there's generally no need to apply a CAT manually.)

Let's take the sRGB profile as an illustrative example. It's an RGB
space, of course, so its color model is additive: "white" is just what
you get when you add the full intensities of each primary. The three
primaries as defined by the sRGB standard are:

  red   = XYZ (0.4124, 0.2126, 0.0193)
  green = XYZ (0.3576, 0.7152, 0.1192)
  blue  = XYZ (0.1805, 0.0722, 0.9504)

Add them up and you get:

  white = XYZ (0.9505, 1.0000, 1.0889)

This is standard illuminant D65, and this is what's meant when it is
said that sRGB has a "reference white point" of D65—it's just all the
primaries summed together. Adopting a new white point means shifting
the primaries so that they sum to a different "white". In the case of
ICC profiles, that's standard illuminant D50:

  d50   = XYZ (0.9642, 1.0000, 0.8251)

The spec says that we should apply the linear Bradford chromatic
adaptation transformation (CAT) to each of the primaries to determine
their PCSXYZ values as saved in the profile.

"Linear" in this case means that it effects a linear transformation of
the coordinate space via a 3x3 linear transformation matrix. In a
linear transformation, you can rotate, scale, and skew the space, but
you can't translate it or warp the perspective (parallel lines remain
parallel.) Note that transformation matrices used in graphics are
usually of 1 higher dimension than the graphics themselves, because
these allow additional transformations to be mediated by the extra
dimension. In this case, we have 3x3 matrices in 3 dimensional space,
so they're more constrained.

What the Bradford model entails is the following:

1) A linear transformation into LMS cone response space. This is simply
  a matrix defined exactly by @CIECAM97s@ and also by the ICC spec. We
  need transformed values for the color itself, the origin white point,
  and the destination white point.

2) The LMS values for the color are each rescaled by the corresponding
  values for the new white point divided by the value for the old WP.

3) The LMS values are transformed back into XYZ with the inverse of the
  matrix from step 1.

We'll go through this for just one of the primaries, in pseudo-code:

  red    = bradford * XYZ (0.4124, 0.2126, 0.0193) = LMS (0.4227, 0.0556, 0.0214)
  d65    = bradford * XYZ (0.9505, 1.0000, 1.0889) = LMS (0.9414, 1.0404, 1.0896)
  d50    = bradford * XYZ (0.9642, 1.0000, 0.8251) = LMS (0.9963, 1.0204, 0.8185)

  l'     = (0.9963 / 0.9414) * 0.4227 = 0.4474
  m'     = (1.0204 / 1.0404) * 0.0556 = 0.0545
  s'     = (0.8185 / 1.0896) * 0.0214 = 0.0161

  red'   = LMS (0.4474, 0.0545, 0.0161)

  newRed = (inverse bradford) * red'
         = XYZ (0.4361, 0.2225, 0.0140)

Indeed, if we pull up the sRGB profile (with Mac OS's ColorSync Utility
for instance) we can see that the encoded PCSXYZ values are (as shown):

  rXYZ   = XYZ (0.436, 0.222, 0.014)

As all the transformations involved are linear (even the scaling can be
made into a 3x3 matrix) they can be multiplied together to obtain a
single matrix that will chromatically shift any color from D65 to D50.
If ever it should be needed, version 4 of the standard specifies that
the CAT matrix be stored in the @chad@ tag, which can be inverted and
used to obtain the original primaries. In principle, the primaries
could be left unmodified and the CAT could be applied to each
transformed color, instead. However, because all the parts of the
conversion other than gamma correction are linear transformations
there's no benefit, and the original values can be recovered in any
case—storing the adapted primaries is basically just skipping ahead
to the matrix that would be created and cached if the primaries were
stored unmodified.

All that said, from the point of view of the display itself nothing
actually changes. That is to say that if our display were a perfect
sRGB reference monitor, RGB (1, 1, 1) in the sRGB space would still be
full intensity in each of the primaries, resulting in a D65 white. The
profile is just a mapping from a mathematical abstraction to the
actual, physical color-producing subpixels in our display, which still
light up at their normal intensities.

** Wait... wait wait wait wait wait.

Hmm?

** Doesn't that mean that, if we had a second monitor that had primaries
that were the same as the adapted sRGB primaries, with a D50 white
point, then both of our monitors would have the same ICC profile? Just
our second one wouldn't need a chromatic adaptation tag, which isn't
even used, anyway?

Yes, that's correct.

** And if we displayed RGB (1, 1, 1) in the sRGB space on our second
monitor, it would be a D50 white?

Yes, that's correct.

** And if we put the two monitors next to each other and displayed the
same shade of blue in the sRGB space on each monitor, they would *not*
appear to be the same shade of blue? Even perfectly calibrated?

Yes, that's correct.

** But... but... isn't that supposed to be the whole damn point?! AAHHH!

And now we see the violence inherent in the system. The key to
enlightenment is this: "chromatic adaptation" *is not* what we were
doing when we multiplied matrices together and shifted the white point.
The ICC specification uses terms like "chromatically adapted values" to
refer to the results of applying the CAT, but that is a shorthand.
Chromatic adaptation is what the *visual cortex* does when it preserves
the appearance of colors ("color constancy") despite differences in the
chromacity of the illumination.

The second sentence of this documentation is: "There is no 100% perfect
or appropriate model or formulae for converting or adjusting colors
that can be applied in all cases." The role of the color management
profile in the real world *is not* to ensure that different displays
output the same spectral power distribution for a given color. A single
color displayed full-screen and compared side-to-side is a totally
artificial and naïve scenario. In almost any realistic use-case, where
color accuracy is of concern, there will be color cues that the visual
cortex will pick up on and use to chromatically adapt to the display's
white point. In fact, the spec explicitly says that the "the viewer is
assumed to completely adapt to the white point" for the purposes of
display profiles.

Basically, our brains do *a lot* of the heavy lifting, here, but that
means we're relying on our brain not getting too tripped up by, for
instance, two displays side-by-side with the same gamut in PCSXYZ but
different white points. The fact that the ICC spec mandates the CAT
adjustment is a kind of bookkeeping, valuable when moving colors along
a chain of color management profiles but not so much for the general
concept of converting colors between spaces.

Actually, the white point of the ambient light is also a potential
hangup for our brains. It's possible to meter the ambient light and
adjust the white point of a display so that the visual cortex is
perfectly chromatically adapted to both the display and environment:
this is what Apple's TrueTone feature does, for example. That is,
however, outside of the scope of the ICC standard. FWIW, this
necessarily works by restricting the range of two of the primaries, so
the effective gamut of the display is reduced accordingly.

* Alright, alright, just give it to me short and sweet already

If you're working with colors for general consumer-quality displays,
specify input and output colors in the sRGB color space, and convert
to @CIE L*u*v*@ using a D65 white point for perceptual adjustments. You
do not need to apply a chromatic adaptation transformation to the
@CIE XYZ@ values in either direction.

* That's it?

That's it.
-}

{- $color_spaces
We differentiate between a color space and a color model. For our
purposes, a model is a way of describing colors. We mostly care
about dimensionality, with topology a distant concern. E.g., RGB is
the unit cube with three completely independent dimensions. CMYK is
similarly the unit tesseract with four dimensions. On the other
hand, YUV is a three dimensional model where two of the dimensions
are dependent on the third.

A color space (again: for our purposes) is a combination of a color
model and a mapping to and from the abstract XYZ space. There isn't
actually anything special about @XYZ@, in absolute terms. Color
spaces needn't be defined mathematically at all, e.g: Pantone. In
order to be computationally tractable, however, we're concerned with
spaces that *are* defined mathematically, and which can be constructed
in relation to @CIE XYZ@, as it is most convenient for our purposes.

(N.B.: We *don't* differentiate explicitly between a color space and a
"color appearance model," which is a color space constructed with
corrections for the non-linear nature of human color perception.)

Any actual given color can be constructed in a model in a way that
has meaning regardless of the actual color space, but the specific
meaning will change from space to space. E.g., RGB (0.5, 0.5, 0.5)
is a valid color in all RGB spaces, but exactly what color it is
depends on the gamma curves and primaries of a given space.

We use these types to describe the shape of the data, not the color
space they inhabit. For instance, @CIE L*a*b*@ and the closely
related @CIE L*u*v*@ each have two dimensions of chromaticity and
one for luminosity, so values in both those spaces are reflected as
@LChroma@.

From a types perspective, what we're doing is reifying the color space
instead of baking it into the type of the color. That makes everything
more flexible and allows for manipulation and introspection of the
color spaces as data instead of an inscrutable & abstract category.

The flipside is that colors can't be constrained by the type system
in terms of the space they inhabit. This is reasonable in much the
same way that the type system can't be used to constrain lengths of
lists or primality of integers.
-}
