{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Data.Aeson.Formatting
Description : Formatting and pretty-printing for Aeson
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

-}

module Data.Aeson.Formatting (
    FieldOptions(..)
  , PrintOptions(..)

  , buildPrintOptions
  , encodePretty
  , fieldFixedFormat
  , fieldIntFormat
  , foInline
  , foName
  , foNumberFormat
  , foRemove
  , foWrap
  , inlineAlways
  , inlineWhenLiterals
  , poFields
  , poIndent
  , poOrder
  , removeNull
  , removeNullEmpty
  , removeNullFalse
  , removeNullZero
) where

import GHC.Generics (Generic)
import Control.Lens
import Data.Aeson
import Data.Aeson.Encoding (bool, null_, pair, scientific, text)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy as LB (ByteString)
import Data.Default.Class
import Data.List (sortBy)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Scientific (FPFormat(..), Scientific, formatScientific)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector as V (all, null, toList)

data FieldOptions = FieldOptions {
    _foName :: Key
  , _foRemove :: Value -> Bool -- ^ Conditions under which a field will not be output
  , _foInline :: Value -> Bool -- ^ Conditions under which a field will be output inline
  , _foWrap :: Maybe Int -- ^ If inlined data is to be wrapped, the number of elements on each line
  , _foNumberFormat :: Scientific -> B.Builder -- ^ Formatting for numbers
} deriving (Generic)

makeLenses ''FieldOptions

instance Default FieldOptions where
  def = FieldOptions "def" (const False) (const False) Nothing (B.stringUtf8 . show)

data PrintOptions = PrintOptions {
    _poFields :: M.Map Key FieldOptions
  , _poOrder :: M.Map Key Int
  , _poIndent :: Int
}

makeLenses ''PrintOptions

buildPrintOptions :: [FieldOptions] -> PrintOptions
buildPrintOptions options = PrintOptions {
    _poFields = M.fromList $ map (\fo -> (fo ^. foName, fo)) options
  , _poOrder = M.fromList $ map (\(fo, i) -> (fo ^. foName, i)) (zip options [1..])
  , _poIndent = 2
  }

-- | Remove a null value
removeNull Null = True
removeNull _ = False

-- | Remove an empty value
removeNullEmpty Null = True
removeNullEmpty (String v) = T.null v
removeNullEmpty (Array v) = V.null v
removeNullEmpty (Object v) = KM.null v
removeNullEmpty _ = False

-- | Remove a null or false value
removeNullFalse Null = True
removeNullFalse (Bool False) = True
removeNullFalse _ = False

-- | Remove a null or zero value
removeNullZero Null = True
removeNullZero (Number 0) = True
removeNullZero _ = False

-- | Always show inline
inlineAlways = const True

isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False
-- | Show inline if all sub-elements are leaf values
inlineWhenLiterals Null = True
inlineWhenLiterals (Bool _) = True
inlineWhenLiterals (Number _) = True
inlineWhenLiterals (String _) = True
inlineWhenLiterals (Array v) = V.all (not . isObject) v
inlineWhenLiterals (Object v) = KM.foldr (\v' -> \il -> il && not (isObject v')) True v

-- | Format a number field as a fixed number of decimal places
fieldFixedFormat :: Int -> Scientific -> B.Builder
fieldFixedFormat decimals n = B.stringUtf8 $ formatScientific Fixed (Just decimals) n

-- | Format a number field as an integer
fieldIntFormat = fieldFixedFormat 0

textToLiteral :: T.Text -> B.Builder
textToLiteral txt = "\"" <> encodeUtf8Builder (T.replace "\"" "\\\"" txt) <> "\""

makeIndentBuffer :: Int -> B.Builder
makeIndentBuffer n = B.stringUtf8 $ replicate n ' '

-- Keys with an explicit order are in specified order, then other keys sorted by string value
keyOrder :: PrintOptions -> Key -> Key -> Ordering
keyOrder po k1 k2 = case (mo1, mo2) of
    (Nothing, Nothing) -> k1 `compare` k2
    (Nothing, Just _) -> GT
    (Just _, Nothing) -> LT
    (Just o1, Just o2) -> o1 `compare` o2
  where
    order = po ^. poOrder
    mo1 = M.lookup k1 order
    mo2 = M.lookup k2 order

encodePretty' :: PrintOptions -> FieldOptions -> Int -> Value -> B.Builder
encodePretty' _po _fo _indent Null = "null"
encodePretty' _po _fo _indent(Bool b) = if b then "true" else "false"
encodePretty' _po fo _indent (Number n) = (fo ^. foNumberFormat) n
encodePretty' _po _fo _indent (String s) = textToLiteral s
encodePretty' po fo indent v@(Array av) = let
    inline = (fo ^. foInline) v
    fo' = fo & foInline .~ (\v' -> (fo ^. foInline) v' && not (isObject v'))
    indent' = indent + (po ^. poIndent)
    parts = map (encodePretty' po fo' indent') (V.toList av)
    chunks = maybe [parts] (\n -> chunksOf n parts) (fo ^. foWrap)
    indentBuffer = makeIndentBuffer indent
    indentBuffer' = makeIndentBuffer indent'
    inlineBuffers vs = fst $ foldl (\(bs, follow) -> \b -> (bs <> (if follow then ", " else mempty) <> b, True)) (mempty, False) vs
  in
    if null parts then
      "[ ]"
    else
      if inline then
        if length chunks == 1 then
          "[ " <> inlineBuffers parts <> " ]"
        else
          "["
          <> (fst $ foldl (\(bs, follow) -> \c -> (bs <> (if follow then "," else mempty) <> "\n" <> indentBuffer' <> inlineBuffers c, True)) (mempty, False) chunks)
          <> "\n" <> indentBuffer <> "]"
      else
        "["
        <> (fst $ foldl (\(bs, follow) -> \b -> (bs <> (if follow then "," else mempty) <> "\n" <> indentBuffer' <> b, True)) (mempty, False) parts)
        <> "\n" <> indentBuffer <> "]"
encodePretty' po fo indent v@(Object km) = let
    inline = (fo ^. foInline) v
    indent' = indent + (po ^. poIndent)
    indentBuffer = makeIndentBuffer indent
    indentBuffer' = makeIndentBuffer indent'
    ko = sortBy (keyOrder po) $ KM.keys km
    parts = map (\k -> let
          fo' = M.findWithDefault def k (po ^. poFields)
          mv = maybe Null id $ KM.lookup k km
        in
          if (fo' ^. foRemove) mv then
              (k, Nothing)
            else
              (k, Just (encodePretty' po fo' indent' mv))
        ) ko
  in
    if null parts then
      "{ }"
    else
      if inline then
        "{ "
        <> (fst $ foldl (\(bs, follow) -> \(k, mb) -> maybe (bs, follow) (\b -> (bs <> (if follow then ", " else mempty) <> (B.stringUtf8 $ show k) <> ": " <> b, True)) mb) (mempty, False) parts)
        <> " }"
      else
        "{"
        <> (fst $ foldl (\(bs, follow) -> \(k, mb) -> maybe (bs, follow) (\b -> (bs <> (if follow then "," else mempty) <> "\n" <> indentBuffer' <> (B.stringUtf8 $ show k) <> ": " <> b, True)) mb) (mempty, False) parts)
        <> "\n" <> indentBuffer <> "}"

encodePretty :: PrintOptions -> Value -> LB.ByteString
encodePretty po val = B.toLazyByteString $ encodePretty' po def 0 val