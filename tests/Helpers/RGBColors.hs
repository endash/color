module Helpers.RGBColors (rgbColors) where

import Data.Word
import Data.Bits
import Data.Color.Spaces.RGB

fromHex :: Word32 -> RGBData
fromHex hex = RGB (r / 255, g / 255, b / 255)
  where
    r = fromIntegral $ shiftR (hex .&. 0xFF0000) 16
    g = fromIntegral $ shiftR (hex .&. 0xFF00) 8
    b = fromIntegral $ hex .&. 0xFF

white :: RGBData
white = fromHex 0xffffff

silver :: RGBData
silver = fromHex 0xc0c0c0

gray :: RGBData
gray = fromHex 0x808080

black :: RGBData
black = fromHex 0x000000

red :: RGBData
red = fromHex 0xff0000

green :: RGBData
green = fromHex 0x00ff00

blue :: RGBData
blue = fromHex 0x0000ff

maroon :: RGBData
maroon = fromHex 0x800000

forest :: RGBData
forest = fromHex 0x008000

navy :: RGBData
navy = fromHex 0x000080

yellow :: RGBData
yellow = fromHex 0xffff00

aqua :: RGBData
aqua = fromHex 0x00ffff

magenta :: RGBData
magenta = fromHex 0xff00ff

olive :: RGBData
olive = fromHex 0x808000

teal :: RGBData
teal = fromHex 0x008080

purple :: RGBData
purple = fromHex 0x800080

rgbColors :: [RGBData]
rgbColors = [
    white, silver, gray, black, red, green, blue, maroon,
    forest, navy, yellow, aqua, magenta, olive, teal, purple
  ]
