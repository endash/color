{-# LANGUAGE TypeFamilies            #-}

module Data.Color.Types.Color where

-- | Basic operations for dealing with color types at a low level, in
-- terms of their raw component values.
class Color c where
  -- | Get the components as a list
  components :: c -> [Double]
  fromComponents :: [Double] -> Maybe c

-- | An instance of @ColorSpace@ is associated with a particular
-- instance of @Color@
type family ColorType c
