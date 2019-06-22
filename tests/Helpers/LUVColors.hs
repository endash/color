-- | The 16 web colors, transformed values generated with this library
-- and spot-checked for accuracy. Hardcoded here to help guard against
-- regressions. Original space was sRGB.
module Helpers.LUVColors (luvColors) where

import Data.Color.Spaces.LUV

white :: LChromaData
white = LChroma (0.9999999999999999, -0.1471091170920678, -0.2567785310157602)

silver :: LChromaData
silver = LChroma (0.77703467291007, -0.1143654580204235, -0.19957900764636366)

gray :: LChromaData
gray = LChroma (0.5358925760131109, -7.892382725723196e-2, -0.13761083562013998)

black :: LChromaData
black = LChroma (0, 0, 0)

red :: LChromaData
red = LChroma (0.5323288178584245, 1.6721938204689297, 0.24085491489742245)

green :: LChromaData
green = LChroma (0.8773703347354421, -0.9599097358469014, 0.8487904938979227)

blue :: LChromaData
blue = LChroma (0.3230258666724948, -0.1414808542318532, -1.3864572731860587)

maroon :: LChromaData
maroon = LChroma (0.25533215079071325, 0.8013371023334946, 0.11538799242638562)

forest :: LChromaData
forest = LChroma (0.46231044149008416, -0.5057722994303225, 0.4473877927728341)

navy :: LChromaData
navy = LChroma (0.12984525077104267, -5.672084212698577e-2, -0.557173356253669)

yellow :: LChromaData
yellow = LChroma (0.9713824698129728, -6.591724785710601e-2, 0.8185349079780062)

aqua :: LChromaData
aqua = LChroma (0.9111652110946341, -0.8387781747213158, -0.3859641411672332)

magenta :: LChromaData
magenta = LChroma (0.6031993366407599, 0.7520752006314124, -1.2418588363792595)

olive :: LChromaData
olive = LChroma (0.5187111318711751, -3.545815592439463e-2, 0.4371306365465094)

teal :: LChromaData
teal = LChroma (0.48260035778351784, -0.4440664713596554, -0.2043778552115406)

purple :: LChromaData
purple = LChroma (0.2978779128321507, 0.3713363054327636, -0.6130195629576239)

luvColors :: [LChromaData]
luvColors = [
    white, silver, gray, black, red, green, blue, maroon,
    forest, navy, yellow, aqua, magenta, olive, teal, purple
  ]
