module Helpers.Utils where

import Data.Color.Types
import Data.Maybe

roundN :: Int -> Double -> Double
roundN n d = (fromInteger $ round $ d * (10^n)) / (10.0^^n)

roundColor :: (Color c) => c -> c
roundColor c = fromJust $ fromComponents (map (roundN 4) (components c))

