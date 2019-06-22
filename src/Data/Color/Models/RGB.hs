{-# LANGUAGE TypeFamilies            #-}

module Data.Color.Models.RGB where

import Data.Color.Types

-- | An RGB triplet. Component ranges are [0, 1].
newtype RGBData = RGB (Double, Double, Double)
  deriving (Eq, Ord, Show)

instance Color RGBData where
  components (RGB (r, g, b)) = [r, g, b]
  fromComponents (r:g:b:_) = Just (RGB (r, g, b))
  fromComponents _         = Nothing

