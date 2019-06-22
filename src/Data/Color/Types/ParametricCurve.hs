module Data.Color.Types.ParametricCurve where

{- | These curves are defined by the ICCv4 spec. Types 1 and 2 are not
bijections (they clip values below a certain point, so the reverse
value cannot be recovered below that floor) so we only implement
curves 3 and 4, as well as the trivial one-parameter gamma curve.
Note that the curves are defined in terms of the reverse transform:
going from adjusted values to linear values. The inversions were
derived manually by solving for x. This is something of an annoyance
and accounts for why the curve for the sRGB space is defined with so
many reciprocal values.

Most profiles seem to use Type 3, and the spec doesn't note an
example for Type 4 as it does the others. Presumably it sees some
use, however, or it wouldn't be in the spec. (Edit: According to a
somewhat buggy info sheet on sRGB from the ICC, Type 4 curves can be
used to combine the gamma correction and black normalization into a
single formula, should that be desirable.)
-}
data ParametricCurve = Gamma Double
                     | Type3 Double Double Double Double Double
                     | Type4 Double Double Double Double Double Double Double

reverseGamma :: ParametricCurve -> Double -> Double
reverseGamma (Gamma g) x = x ** g

reverseGamma (Type3 g a b c d) x | x < d     = x * c
                                 | otherwise = (a * x + b) ** g

reverseGamma (Type4 g a b c d e f) x | x < d     = (1 / c) * x + f
                                     | otherwise = (a * x + b) ** g + e

forwardGamma :: ParametricCurve -> Double -> Double
forwardGamma (Gamma g) y = y ** (1 / g)

forwardGamma (Type3 g a b c d) y | revLin < d = revLin
                                 | otherwise  = ((y ** (1 / g)) - b) / a
  where
    revLin = y / c

forwardGamma (Type4 g a b c d e f) y | revLin < d = revLin
                                     | otherwise  = (((y - e) ** (1 / g)) - b) / a
  where
    revLin = ((y - f) / c)