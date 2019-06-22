{-# LANGUAGE TypeFamilies            #-}

module Data.Color.Models.LChroma where

import Data.Color.Types

-- | A value combining luminosity and two dimensions of chromacity.
-- Component ranges are [0, 1] for @Y@ and (-?, ?) for the others.
newtype LChromaData = LChroma (Double, Double, Double)
  deriving (Eq, Ord, Show)

instance Color LChromaData where
  components (LChroma (l, u, v)) = [l, u, v]
  fromComponents (l:u:v:_) = Just (LChroma (l, u, v))
  fromComponents _         = Nothing

-- | A cylindrical reprojection of LChroma values. Component ranges are
-- [0, 1] for @Y@, [0, 2π) for @Hue@, and [0, ?) for @Chroma@
newtype LChromaHueData = LChromaHue (Double, Double, Double)
  deriving (Eq, Ord, Show)

instance Color LChromaHueData where
  components (LChromaHue (l, c, h)) = [l, c, h]
  fromComponents (l:c:h:_) = Just (LChromaHue (l, c, h))
  fromComponents _         = Nothing

toCylindrical :: LChromaData -> LChromaHueData
toCylindrical (LChroma (l, u, v)) = LChromaHue (l, c, h)
  where
    c = sqrt (u * u + v * v)
    t = atan2 v u
    h = if t >= 0 then t else t + pi + pi

fromCylindrical :: LChromaHueData -> LChromaData
fromCylindrical (LChromaHue (l, c, h)) = LChroma (l, u, v)
  where
    u = c * (cos h)
    v = c * (sin h)
