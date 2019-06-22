# Color

`color` is a Haskell library for manipulating colors and color spaces. They are typed separately, with type families and constraints used to associate a given color space type with a color model type.

Currently, `color` supports converting between CIEXYZ, CIELAB, CIELUV, CIELCH, CIELCHuv/HCL, and sRGB. The base scale is 1.0, instead of 255 or 100, as applicable.

## Color spaces

Individual color space types are instances of the `ColorSpace` class, which defines a conversion function to and from the CIEXYZ space. An additional class, `MatrixConvertible`, is a convenience for color spaces that can be defined as a matrix transformation of CIEXYZ, such as 3-component RGB spaces. Other spaces with their peculiar conversion formulae implement `fromXYZ` and `toXYZ` themselves.

### CIE L* a* b*

CIELAB is one of two CIE color spaces introduced in 1976, intended to be more perceptually uniform than CIEXYZ, i.e., the Euclidian distance between two color coordinates corresponds closely to the perceived visual difference. A CIELAB space is created with a specific white point, which is used by a crude chromatic adaptation transformation (CAT) applied as part of the conversion formulae.

The color model used is `LChromaData`, which is shared with CIELUV. `LChromaData` is a three dimensional value, with the first value being luminance, [0, 1], and the other two values being the two antagonistic chroma dimensions (red-green and blue-yellow), ranging from (very roughly) [-1.5, 1.5] for colors within the gamut of human vision.

Independent of the color space, `LChromaData` can be converted into `LChromaHueData` with `toCylindrical`.

```
import Data.Color.Spaces.LAB

lab50 = LABSpace illD50

fromXYZ lab50 illD50
-- LChroma (1.0, 0.0, 0.0)
```

### CIE L* u* v*

The other CIE color space introduced in 1976, CIELUV is also intended to be perceptually uniform. It has the advantage over CIELAB of a saturation correlate: holding chroma and luminance constant will always hold saturation constant as well, and they otherwise have a linear relationship. This property sees use in information visualization, where biases owing to varying saturation can be eliminated or controlled by holding saturation constant or manipulating it in a uniform manner.

CIELUV also uses the `LChromaData` color model, and as such it also has a cylindrical representation.

```
import Data.Color.Spaces.LUV

luv50 = LUVSpace illD50

fromXYZ luv50 illD50
-- LChroma (1.0, 0.0, 0.0)
```

### sRGB

The sRGB space is a combination of three primaries and a gamma correction curve. The primaries (the ones defined in the actual spec, not the transformed values you will find in the sRGB ICC profile) become the columns in a 3x3 matrix, and multiplying by an CIEXYZ value will move that color into the sRGB space. The resulting color has linear component values, but the final representation is non-linear: the gamma correction curve allows the limited precision of the 8-bit encoding to be used more efficiently, by allocating more of the range to darker colors. The whole process moves in reverse by inverting the gamma curve and the matrix.

```
import Data.Color.Spaces.RGB

fromXYZ srgb d65
-- RGB (1.0, 1.0, 1.0)
```

### Why no HSL/HSV?

HSL and HSV are cylindrical transformations of RGB, but apart from being reasonably perceptual for any given hue (all colors with that hue value will more-or-less be perceived to be the same actual hue) they are borderline useless. None of the dimensions are perceptually uniform, and calling the second dimension "saturation" is just utter nonsense.

Instead, convert sRGB colors to CIELUV with a D65 white point. In its cylindrical transformation, CIELUV makes manipulating hue, chroma, saturation, and brightness both trivially easy and perceptually uniform. In CIELCHuv, saturation is just `L* / C*`, which means that paths of constant saturation through the space are straight lines, and saturation changes linearly in proportion to the changing chroma or luminance. (Of course, CIELCHuv is a trivial coordinate transformation, so the actual shape of the space is unchanged, including lines of constant saturation—the correlation isn't specific to the cylindrical coordinates.)

The flipside is that the visible or display gamut in CIELUV/CIELCHuv space is not actually a cylinder, so care must be taken to ensure that the transformed colors are in-gamut.

## Example

```
import Data.Color
import Data.Color.Spaces.RGB
import Data.Color.Spaces.LUV

-- reducing luminance and chroma by equal measure keeps saturation constant
tint :: LChromaData -> LChromaData
tint (LChroma (l, u, v)) = LChroma (l * 0.8, u * 0.8, v * 0.8)

let luv = LUVSpace illD65
let maroon = convert srgb luv (RGB (0.75, 0.2, 0.2))
-- LChroma (0.4382, 1.0633, 0.2293)

convert luv srgb $ tint maroon
-- RGB (0.6053, 0.1542, 0.1542)

-- Compare to the same transformation in sRGB space, the components aren't
-- uniformly changed. The red component is slightly higher and the blue
-- and green components are slightly lower than if we just manipulated the
-- RGB values directly:
--
-- RGB (0.6, 0.16, 0.16)
```

## Acknowledgements

This project began by hacking on the `prizm` library. For a number of reasons, I decided it would be advantageous to have a more general and simplified library for moving colors between spaces, and without the higher-level concerns like dealing with encoding formats or mixing colors.

## Note

This is my first Haskell library. If anything jumps out at you as particularly sub-optimal, either in terms of style, performance, or design, I'd appreciate it if you dropped me a note.
