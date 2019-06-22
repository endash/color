-- | The 16 web colors, transformed values generated with this library
-- and spot-checked for accuracy. Hardcoded here to help guard against
-- regressions. Original space was sRGB.
module Helpers.LABColors (labColors) where

import Data.Color.Spaces.LAB

white :: LChromaData
white = LChroma (0.9999999999999999, -2.379417022296937e-2, -0.19376683141066975)

silver :: LChromaData
silver = LChroma (0.77703467291007, -1.9243523759271963e-2, -0.1565646195629915)

gray :: LChromaData
gray = LChroma (0.5358925760131109, -1.4337095129333677e-2, -0.11625426496419089)

black :: LChromaData
black = LChroma (0, 0, 0)

red :: LChromaData
red = LChroma (0.5323288178584245, 0.7830399932711313, 0.621691175202334)

green :: LChromaData
green = LChroma (0.8773703347354421, -0.8790615404737073, 0.7391401630974592)

blue :: LChromaData
blue = LChroma (0.3230258666724948, 0.7782597565419364, -1.2637051929779948)

maroon :: LChromaData
maroon = LChroma (0.25533215079071325, 0.46944145374804014, 0.36095126018784157)

forest :: LChromaData
forest = LChroma (0.46231044149008416, -0.5273422200651792, 0.44367640936650043)

navy :: LChromaData
navy = LChroma (0.12984525077104267, 0.4670037566597948, -0.7579953573071359)

yellow :: LChromaData
yellow = LChroma (0.9713824698129728, -0.2377892401532583, 0.8473998071326938)

aqua :: LChromaData
aqua = LChroma (0.9111652110946341, -0.5005234376921175, -0.33389192223419784)

magenta :: LChromaData
magenta = LChroma (0.6031993366407599, 0.9621667888416174, -0.7947439250029278)

olive :: LChromaData
olive = LChroma (0.5187111318711751, -0.14283296383423794, 0.5083538650304189)

teal :: LChromaData
teal = LChroma (0.48260035778351784, -0.30009802442702516, -0.20025318551945115)

purple :: LChromaData
purple = LChroma (0.2978779128321507, 0.5770387941897787, -0.4766053766296927)

labColors :: [LChromaData]
labColors = [
    white, silver, gray, black, red, green, blue, maroon,
    forest, navy, yellow, aqua, magenta, olive, teal, purple
  ]
