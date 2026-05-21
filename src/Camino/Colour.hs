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
  , PaletteColour(..)
  -- ** Properties
  , luminance
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
  --
  -- | A collection of standard colours that make their way into the CSS files and can be used to include the colours in code.
  --
  -- ![colour chart](colours.svg)
  , asideBackground
  , caminoBackground
  , caminoBlue
  , caminoLightGrey
  , caminoYellow
  , casaRuralCyan
  , ferryGreen
  , homeStayPink
  , hostelGreen
  , informationBlue
  , mutedBlue
  , naismithBlue
  , recreationGreen
  , refugeRed
  , rejectedRed
  , successGreen
  , textBlack
  , textWhite
  , unusedGrey
  , warningOrange
  , warningRed
  , namedColours
) where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Aeson
import Data.Colour (blend)
import Data.Colour.Names (antiquewhite, black, grey, mediumaquamarine)
import qualified Data.Colour.CIE as CIE
import Data.Colour.SRGB
import Data.Default.Class
import Data.Text (Text, pack)
import Data.Util (tailOrEmpty)
import Data.Word (Word8)
import Numeric (showHex)

-- | What we use to encode colours
newtype PaletteColour = PaletteColour { unPaletteColour :: Colour Double }
  deriving (Show, Eq, Generic)

-- Make PaletteColour NFData
-- Colour is already strict, so leave it be
instance NFData PaletteColour where
  rnf _ = ()

-- Operations on underlying colour
cmap :: (Colour Double -> Colour Double) -> PaletteColour -> PaletteColour
cmap f (PaletteColour c) = PaletteColour (f c)

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

-- | A light camino background colour that evokes camino yellow but pale enough to act as a background.
--
--   This colour corrersponds to the custom bootstrap background and
--   is used as a reference for computing suitably contrasting text colours.
--
--   <https://www.colorhexa.com/faf8f0>
caminoBackground :: PaletteColour
caminoBackground = fromColourString "faf8f0"

-- | The traditional blue tile colour. Used as a primary darkish colour
--
--   <https://www.colorhexa.com/1964c0>
caminoBlue :: PaletteColour
caminoBlue = fromColourString "1964c0"

-- | The traditional yellow tile colour. Used as a primary lightish colour
--
--   <https://www.colorhexa.com/f9b34a>
caminoYellow :: PaletteColour
caminoYellow = fromColourString "f9b34a"

-- | A light grey version of the background colour
--
--   <https://www.colorhexa.com/e4e0cb>
caminoLightGrey :: PaletteColour
caminoLightGrey = fromColourString "e4e0cb"

-- | A blue indicating information. Not the traditional information sign colour, since it's too close to camino blue
--
--   <https://www.colorhexa.com/1c9cf1>
informationBlue :: PaletteColour
informationBlue = fromColourString "1c9cf1"

-- | A green indicating rest and recreation
--
--   <https://www.colorhexa.com/00b820>
recreationGreen :: PaletteColour
recreationGreen = fromColourString "00b820"

-- | A muted blue indicating deprectaed information
--
--   <https://www.colorhexa.com/a0b3ca>
mutedBlue :: PaletteColour
mutedBlue = fromColourString "a0b3ca"


-- | A warning color
--
--   <https://www.colorhexa.com/fd7e13>
warningOrange :: PaletteColour
warningOrange = fromColourString "fd7e13"

-- | A warning color
--
--   <https://www.colorhexa.com/dc3545>
warningRed :: PaletteColour
warningRed = fromColourString "dc3545"

-- | A bootstrap success color
--
--   <https://www.colorhexa.com/198754>
successGreen :: PaletteColour
successGreen = fromColourString "198754"

-- | Indicates that a preference has been rejected
--
--   <https://www.colorhexa.com/c5321b>
rejectedRed :: PaletteColour
rejectedRed = fromColourString "c5321b"

-- | Casa rural/Quinta colour
--
--   <https://www.colorhexa.com/19c0bf>
casaRuralCyan :: PaletteColour
casaRuralCyan = fromColourString "19c0bf"

-- | Hostel colour
--
--   <https://www.colorhexa.com/2ab472>
hostelGreen :: PaletteColour
hostelGreen = fromColourString "2ab472"

-- | Refuge colour
--
--   <https://www.colorhexa.com/ff201c>
refugeRed :: PaletteColour
refugeRed = fromColourString "ff201c"

-- | Home stay colour
--
--   <https://www.colorhexa.com/ff395c>
homeStayPink :: PaletteColour
homeStayPink = fromColourString "ff395c"

-- | Naismoth's rule colour, indicating Naismith's rule is being used to estimate time
--
--   <https://www.colorhexa.com/506890>
naismithBlue :: PaletteColour
naismithBlue = fromColourString "506890"

-- | Background colour for asides
--
--   <https://www.colorhexa.com/d0f8ff>
asideBackground :: PaletteColour
asideBackground = fromColourString "d0f8ff"

-- | A blue for ferry routes and the like
--
--   <https://www.colorhexa.com/66cdaa> (medium aquamarine)
ferryGreen :: PaletteColour
ferryGreen = PaletteColour mediumaquamarine

-- | A grey for routes that are not used
--
--   <https://www.colorhexa.com/80800> (grey)
unusedGrey :: PaletteColour
unusedGrey = PaletteColour grey

-- | A white to use for text on a dark background
--
--   <https://www.colorhexa.com/faf8f0>
textWhite :: PaletteColour
textWhite = fromColourString "faf8f0"

-- | A black to use for text on a light background
--
--   <https://www.colorhexa.com/212529>
textBlack :: PaletteColour
textBlack = fromColourString "212529"

--
-- | Lighten a palette colour
--
-- >>> toCssColour <$> [caminoYellow, lighten caminoYellow]
-- ["#f9b34a","#f9c689"]
lighten :: PaletteColour -> PaletteColour
lighten c = blend 0.3 antiquewhite `cmap` c

-- | Darken a palette colour
--
-- >>> toCssColour <$> [caminoYellow, darken caminoYellow]
-- ["#f9b34a","#b78334"]i
darken :: PaletteColour -> PaletteColour
darken c = blend 0.5 black `cmap` c

-- | Colour luminance
--
-- >>> luminance caminoYellow
-- 0.5287693392210411
luminance :: PaletteColour -- ^ The colour
  -> Double -- The luminance from 0.0 (black) to 1.0 (white)
luminance (PaletteColour c) = CIE.luminance c

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
-- "#f9b34a"
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

-- | Standard colour list of (English) names and colours
namedColours :: [(Text, PaletteColour)]
namedColours = [
    ("Aside Background", asideBackground)
  , ("Camino Background", caminoBackground)
  , ("Camino Blue", caminoBlue)
  , ("Camino Light Grey", caminoLightGrey)
  , ("Camino Yellow", caminoYellow)
  , ("Casa Rural Cyan", casaRuralCyan)
  , ("Ferry Green", ferryGreen)
  , ("Home Stay Pink", homeStayPink)
  , ("Hostel Green", hostelGreen)
  , ("Information Blue", informationBlue)
  , ("Muted Blue", mutedBlue)
  , ("Naismith Blue", naismithBlue)
  , ("Recreation Green", recreationGreen)
  , ("Refuge Red", refugeRed)
  , ("Rejected Red", rejectedRed)
  , ("Success Green", successGreen)
  , ("Text Black", textBlack)
  , ("Text White", textWhite)
  , ("Unused Grey", unusedGrey)
  , ("Warning Orange", warningOrange)
  , ("Warning Red", warningRed)
  ]