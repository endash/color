-- | The 16 web colors, transformed values generated with this library
-- and spot-checked for accuracy. Hardcoded here to help guard against
-- regressions. Original space was sRGB.
module Helpers.XYZColors (xyzColors) where

import Data.Color.Models.XYZ

white :: XYZData
white = XYZ (0.9505, 1.0000, 1.0889)

silver :: XYZData
silver = XYZ (0.501, 0.5271, 0.574)

gray :: XYZData
gray = XYZ (0.2052, 0.2159, 0.2351)

black :: XYZData
black = XYZ (0, 0, 0)

red :: XYZData
red = XYZ (0.4124, 0.2126, 0.0193)

green :: XYZData
green = XYZ (0.3576, 0.7152, 0.1192)

blue :: XYZData
blue = XYZ (0.1805, 0.0722, 0.9504)

maroon :: XYZData
maroon = XYZ (0.089, 0.0459, 0.0042)

forest :: XYZData
forest = XYZ (0.0772, 0.1544, 0.0257)

navy :: XYZData
navy = XYZ (0.039, 0.0156, 0.2052)

yellow :: XYZData
yellow = XYZ (0.7700, 0.9278, 0.1385)

aqua :: XYZData
aqua = XYZ (0.5381, 0.7874, 1.0696)

magenta :: XYZData
magenta = XYZ (0.5929, 0.2848, 0.9697)

olive :: XYZData
olive = XYZ (0.1662, 0.2003, 0.0299)

teal :: XYZData
teal = XYZ (0.1162, 0.17, 0.2309)

purple :: XYZData
purple = XYZ (0.128, 0.0615, 0.2093)

xyzColors :: [XYZData]
xyzColors = [
    white, silver, gray, black, red, green, blue, maroon,
    forest, navy, yellow, aqua, magenta, olive, teal, purple
  ]
