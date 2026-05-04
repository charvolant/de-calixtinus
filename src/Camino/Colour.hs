{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Colour
Description : Colour management for Camino description and rendering
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Palettes, standard colours and conversions for use when displaying camino information
-}
module Camino.Colour (
  -- * Palettes
    Palette(..)
  , PaletteColour
  -- * Colour Operations
  , lighten
  , darken
  -- * Conversion
  , fromColourString
  , toColourString
  , toCssColour
  , toExcelColour
  , toKmlColour
  -- * Standard colours
  , caminoBlue
  , caminoLightGrey
  , caminoYellow
  , informationBlue
  , mutedBlue
  , recreationGreen
  , routeFerry
  , routeUnused
  , standardWhite
  , successGreen
  , warningRed
) where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Aeson
import Data.Colour (blend)
import Data.Colour.Names (antiquewhite, black, grey, mediumaquamarine)
import Data.Colour.SRGB
import Data.Default.Class
import Data.Text (Text, pack)
import Data.Util (tailOrEmpty)
import Data.Word (Word8)
import Numeric (showHex)

-- | What we use to encode colours
newtype PaletteColour = PaletteColour (Colour Double)
  deriving (Show, Eq, Generic)

-- Make PaletteColour NFData
-- Colour is already strict, so leave it be
instance NFData PaletteColour where
  rnf _ = ()

-- | A palette, graphical styles to use for displaying information.
data Palette = Palette {
    paletteColour :: PaletteColour -- ^ The basic colour of the element
  , paletteTextColour :: PaletteColour -- ^ The text colour of the element
} deriving (Show, Generic)

instance FromJSON Palette where
  parseJSON (Object v) = do
    colour' <- v .: "colour"
    textColour' <- v .:? "text-colour" .!= colour'
    return Palette {
        paletteColour = fromColourString colour'
      , paletteTextColour = fromColourString textColour'
    }
  parseJSON v = error ("Unable to parse palette object " ++ show v)

instance ToJSON Palette where
  toJSON (Palette colour' textColour') =
    object [
        "colour" .= toColourString colour'
      , "text-colour" .= (if colour' == textColour' then Nothing else Just $ toColourString textColour')
      ]
  toEncoding (Palette colour' textColour') =
    pairs $
        "colour" .= toColourString colour'
      <> "text-colour" .?= (if colour' == textColour' then Nothing else Just $ toColourString textColour')

instance NFData Palette

instance Default Palette where
  def = Palette {
      paletteColour = caminoYellow -- Camino yellow
    , paletteTextColour = caminoYellow -- Camino yellow
  }

-- | The traditional blue tile colour. Used as a primary darkish colour
caminoBlue :: PaletteColour
caminoBlue = PaletteColour $ sRGB24read "1964c0"

-- | The traditional yellow tile colour. Used as a primary lightish colour
caminoYellow :: PaletteColour
caminoYellow = PaletteColour $ sRGB24read "f9b34a"

-- | A light grey version of the background colour
caminoLightGrey :: PaletteColour
caminoLightGrey = PaletteColour $ sRGB24read "e4e0cb"

-- | A blue indicating information. Not the traditional information sign colour, since it's too close to camino blue
informationBlue :: PaletteColour
informationBlue = PaletteColour $ sRGB24read "1c9cf1"

-- | A green indicating rest and recreation
recreationGreen :: PaletteColour
recreationGreen = PaletteColour $ sRGB24read "00b820"

-- | A muted blue indicating deprectaed information
mutedBlue :: PaletteColour
mutedBlue = PaletteColour $ sRGB24read "a0b3ca"

-- | A bootstrap warning color
warningRed :: PaletteColour
warningRed = PaletteColour $ sRGB24read "dc3545"

-- | A bootstrap success color
successGreen :: PaletteColour
successGreen = PaletteColour $ sRGB24read "198754"

-- | A blue for ferry routes and the like
routeFerry :: PaletteColour
routeFerry = PaletteColour mediumaquamarine

-- | A grey for routes that are not used
routeUnused :: PaletteColour
routeUnused = PaletteColour grey

-- | A white to use for backgrounds and font contrast
standardWhite :: PaletteColour
standardWhite = PaletteColour antiquewhite

--
-- | Lighten a palette colour
--
-- >>> lighten successGreen
-- PaletteColour (Data.Colour.SRGB.Linear.rgb 0.2935968580989523 0.41882774875828477 0.2659216512899692)
lighten :: PaletteColour -> PaletteColour
lighten (PaletteColour c) = PaletteColour $ blend 0.3 antiquewhite c

--
-- | Darken a palette colour
--
-- >>> darken successGreen
-- PaletteColour (Data.Colour.SRGB.Linear.rgb 4.860608660118925e-3 0.12114056123277743 4.432779314288647e-2)
darken :: PaletteColour -> PaletteColour
darken (PaletteColour c) = PaletteColour $ blend 0.5 black c

-- | Read from an rrggbb hex colour
--
--  The string may have a '#' at the start
--
-- >>> fromColourString "dc3545"
-- PaletteColour (Data.Colour.SRGB.Linear.rgb 0.9473065367331999 0.45078578283822346 6.847816984440017e-2)
fromColourString :: String -> PaletteColour
fromColourString = PaletteColour . sRGB24read

-- | Render as a #rrggbb hex colour
--
-- >>> toColourString caminoYellow
-- "f9b34a"
toColourString :: PaletteColour -> String
toColourString (PaletteColour c) =  sRGB24show c
--
--
-- | Create a CSS-able colour
--
--   >>> toCssColour caminoBlue
--   "#1964c0"
toCssColour :: PaletteColour -- ^ The colour to display
 -> String -- ^ A #rrggbb colour triple
toCssColour = toColourString

-- | Create a colour that can be used in an Excel style
--
--   >>> toExcelColour standardWhite
--   "fffaebd7"
toExcelColour :: PaletteColour -- ^ The colour to display
  -> Text -- ^ A aarrggbb colour quad
toExcelColour pc = "ff" <> (pack $ tailOrEmpty $ toColourString pc)

-- | Convert am opaque colour into an ABGR triple suitable for KML
--
--   KML is hex aabbggrr - alpha, blue, green, red for some reason
--
--   >>> toKmlColour 1.0 caminoBlue
--   "ffc06419"
toKmlColour :: Double -> PaletteColour -> Text
toKmlColour alpha (PaletteColour colour) = pack $ (showHex2 a' . showHex2 b' . showHex2 g' . showHex2 r') ""
  where
    a' = floor (alpha * 255)
    RGB r' g' b' = toSRGB24 colour

showHex2 :: Word8 -> ShowS
showHex2 x
  | x <= 0xf = ("0"++) . showHex x
  | otherwise = showHex x

