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
  , JPath(..)
  , PrintOptions(..)

  , buildPrintOptions
  , encodePretty
  , fieldFixedFormat
  , fieldGeneralFormat
  , fieldIntFormat
  , foChildren
  , foInline
  , foNumberFormat
  , foPath
  , foRemove
  , foWrap
  , inlineAlways
  , inlineNever
  , inlineWhenLiterals
  , matchJPath
  , parseJPath
  , poFields
  , poIndent
  , removeAlways
  , removeNever
  , removeNull
  , removeNullEmpty
  , removeNullFalse
  , removeNullZero
) where

import GHC.Generics (Generic)
import Control.Lens
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy as LB (ByteString, intercalate, intersperse)
import Data.Default.Class
import Data.List (find, intercalate, intersperse, sortBy)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Data.Scientific (FPFormat(..), Scientific, formatScientific)
import qualified Data.Set as S
import qualified Data.String as ST (IsString(..))
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Util (headWithError, headWithDefault)
import qualified Data.Vector as V (all, null, toList, length)
import Text.Parsec
import Text.Parsec.String (Parser)
import Debug.Trace
import Data.ByteString.Builder (lazyByteString)


data JPath =
    JPAny
  | JPObject
  | JPField Key
  | JPArray
  | JPArrayElement (S.Set Int)
  deriving (Eq)

instance Show JPath where
  show JPAny = "*"
  show JPObject = "{}"
  show (JPField key) = show key
  show JPArray = "[]"
  show (JPArrayElement elts) = "[" ++ Data.List.intercalate "," (map show $ S.toList elts) ++ "]"

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  spaces
  return x

jpathAnyParser :: Parser JPath
jpathAnyParser = do
  void $ lexeme $ char '*'
  return JPAny

jpathArrayParser :: Parser JPath
jpathArrayParser = do
  void $ lexeme $ char '['
  void $ lexeme $ char ']'
  return JPArray

jpathObjectParser :: Parser JPath
jpathObjectParser = do
  void $ lexeme $ char '{'
  void $ lexeme $ char '}'
  return JPObject

jpathArrayElementParser :: Parser JPath
jpathArrayElementParser = do
  void $ lexeme $ char '['
  indexes <- jpathIndexParser `sepBy1` (lexeme $ char ',')
  void $ lexeme $ char ']'
  return $ JPArrayElement $ if length indexes == 1 then headWithError indexes else S.unions indexes

jpathIndexParser :: Parser (S.Set Int)
jpathIndexParser = do
  low <- optionMaybe (lexeme $ many1 digit)
  range <- optionMaybe (lexeme $ string "..")
  up <- optionMaybe (lexeme $ many1 digit)
  case (read <$> low, range, read <$> up) of
    (Nothing, Nothing, Nothing) -> fail "Invalid index or index range"
    (Just l, Nothing, Nothing) -> return $ S.singleton l
    (Just l, Just _, Just u) -> return $ S.fromList [l .. u]
    (Nothing, Just _, Just u) -> return $ S.fromList [0 .. u]
    (_, _, _) -> fail "Invalid index specification"

jpathFieldParser :: Parser JPath
jpathFieldParser = do
  field <- lexeme $ many1 $ (Text.Parsec.noneOf "[]*, \t\n")
  return $ JPField (fromString field)

jpathParser :: Parser JPath
jpathParser = do
  spaces
  jpath <- try jpathAnyParser
    <|> try jpathObjectParser
    <|> try jpathArrayParser
    <|> try jpathArrayElementParser
    <|> jpathFieldParser
  return jpath

parseJPath :: String -> JPath
parseJPath s = either (\e -> error $ show e) id $ parse jpathParser "" s

instance ST.IsString JPath where
  fromString = parseJPath

-- | Match a value angainst a jpath specification
matchJPath :: JPath -> Value -> Maybe Key -> Maybe Int -> Bool
matchJPath JPAny _ _ _ = True
matchJPath JPObject (Object _) _ _ = True
matchJPath (JPField key) (Object km) (Just key') _ = key == key' && KM.member key km
matchJPath JPArray (Array _) _ _ = True
matchJPath (JPArrayElement indexes) (Array vs) _ (Just idx) = idx >= 0 && idx < V.length vs && S.member idx indexes
matchJPath _ _ _ _ = False


-- | Do not remove values
removeNever :: Value -> Bool
removeNever = const False

-- | Always remove values
removeAlways :: Value -> Bool
removeAlways = const True

-- | Remove a null value
removeNull :: Value -> Bool
removeNull Null = True
removeNull _ = False

-- | Remove an empty value
removeNullEmpty :: Value -> Bool
removeNullEmpty Null = True
removeNullEmpty (String v) = T.null v
removeNullEmpty (Array v) = V.null v
removeNullEmpty (Object v) = KM.null v
removeNullEmpty _ = False

-- | Remove a null or false value
removeNullFalse :: Value -> Bool

removeNullFalse Null = True
removeNullFalse (Bool False) = True
removeNullFalse _ = False

-- | Remove a null or zero value
removeNullZero :: Value -> Bool
removeNullZero Null = True
removeNullZero (Number 0) = True
removeNullZero _ = False


-- | Do not put inline
inlineNever :: Value -> Bool
inlineNever = const False

-- | Always show inline
inlineAlways :: Value -> Bool
inlineAlways = const True

isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False

isComplex :: Value -> Bool
isComplex (Object _) = True
isComplex (Array _) = True
isComplex _ = False

-- | Show inline if all sub-elements are leaf values
inlineWhenLiterals :: Value -> Bool
inlineWhenLiterals Null = True
inlineWhenLiterals (Bool _) = True
inlineWhenLiterals (Number _) = True
inlineWhenLiterals (String _) = True
inlineWhenLiterals (Array v) = V.all (not . isComplex) v
inlineWhenLiterals (Object v) = KM.foldr (\v' -> \il -> il && not (isComplex v')) True v

-- | Format a number field using general `show` formatting
fieldGeneralFormat :: Scientific -> B.Builder
fieldGeneralFormat n = B.stringUtf8 $ show n

-- | Format a number field as a fixed number of decimal places
fieldFixedFormat :: Int -> Scientific -> B.Builder
fieldFixedFormat decimals n = B.stringUtf8 $ formatScientific Fixed (Just decimals) n

-- | Format a number field as an integer
fieldIntFormat :: Scientific -> B.Builder
fieldIntFormat = fieldFixedFormat 0

data FieldOptions = FieldOptions {
    _foPath :: JPath
  , _foRemove :: Maybe (Value -> Bool) -- ^ Conditions under which a field will not be output
  , _foInline :: Maybe (Value -> Bool) -- ^ Conditions under which a field will be output inline
  , _foWrap :: Maybe Int -- ^ If inlined data is to be wrapped, the number of elements on each line
  , _foNumberFormat :: Maybe (Scientific -> B.Builder) -- ^ Formatting for numbers, with array position
  , _foChildren :: [FieldOptions] -- ^ Options for children of this field
} deriving (Generic)

makeLenses ''FieldOptions


instance Show FieldOptions where
  show fo = "{" ++ show (fo ^. foPath) ++ " -> "
    ++ present "inline" (fo ^. foInline)
    ++ present "remove" (fo ^. foRemove)
    ++ present "wrap" (fo ^. foWrap)
    ++ present "number" (fo ^. foNumberFormat)
    ++ (if null (fo ^. foChildren) then "" else "children ")
    ++ "}"
    where
      present s mv = maybe "" (\_ -> s ++ " ") mv

instance Default FieldOptions where
  def = FieldOptions {
      _foPath = JPAny
    , _foRemove = Nothing
    , _foInline = Nothing
    , _foWrap = Nothing
    , _foNumberFormat = Nothing
    , _foChildren = []
    }

baseFieldOptions = FieldOptions {
    _foPath = JPAny
  , _foRemove = Just removeNever
  , _foInline = Just inlineNever
  , _foWrap = Nothing
  , _foNumberFormat = Just fieldGeneralFormat
  , _foChildren = []
  }

-- Overwrite field options
mergeFieldOptions :: FieldOptions -> FieldOptions -> FieldOptions
mergeFieldOptions fo1 fo2 = fo2
    & foInline .~ combine foInline fo1 fo2
    & foWrap .~ combine foWrap fo1 fo2
    & foNumberFormat .~ combine foNumberFormat fo1 fo2
  where
    combine ls f1 f2 = case (f1 ^. ls, f2 ^. ls) of
      (v1, Nothing) -> v1
      (_, v2) -> v2

-- | Collected JSON print options
data PrintOptions = PrintOptions {
    _poFields :: [(FieldOptions, Int)]
  , _poIndent :: Int
} deriving (Generic, Show)

makeLenses ''PrintOptions

buildPrintOptions :: [FieldOptions] -> PrintOptions
buildPrintOptions options = PrintOptions {
    _poFields = zip options [0..]
  , _poIndent = 2
  }

instance Default PrintOptions where
  def = buildPrintOptions [ baseFieldOptions ]

-- Look through the stack of print options and see what we can find, return a field option and basic sort index
findOptions :: [PrintOptions] -> Value -> Int -> Maybe Key -> Maybe Int -> (FieldOptions, Int)
findOptions [] _ sz _ _ = (def, sz)
findOptions (po:rest) v sz mk mi = let
    mfo = find (\(fo, _) -> matchJPath (fo ^. foPath) v mk mi) (po ^. poFields)
    sz' = length (po ^. poFields)
  in
    case mfo of
      Nothing -> findOptions rest v (sz + sz') mk mi
      Just (fo, p) -> (fo, p + sz)

makeIndentBuffer :: Int -> B.Builder
makeIndentBuffer n = B.stringUtf8 $ replicate n ' '

-- Keys with an explicit order are in specified order, then other keys sorted by string value
keyOrder :: (Maybe B.Builder, Int, Key) -> (Maybe B.Builder, Int, Key) -> Ordering
keyOrder (_, pri1, key1) (_, pri2, key2) = if pri1 == pri2 then
    key1 `compare` key2
  else
    pri1 `compare` pri2

encodeArrayElt :: [PrintOptions] -> FieldOptions -> Int -> Int -> Value -> Value -> Maybe B.Builder
encodeArrayElt pos fo p indent v sv = let
    (fo', _) = findOptions pos v 0 Nothing (Just p)
    fo'' = mergeFieldOptions fo fo'
    cfos = fo'' ^. foChildren
    pos' = if null cfos then pos else (buildPrintOptions cfos):pos
    remove = maybe False (\f -> f sv) (fo'' ^. foRemove)
  in
    if remove then
      Nothing
    else
      Just $ encodePretty' pos' fo'' indent sv

encodeField :: [PrintOptions] -> FieldOptions -> Key -> Int -> Value -> Value -> (Maybe B.Builder, Int, Key)
encodeField pos fo key indent v sv = let
    (fo', pri) = findOptions pos v 0 (Just key) Nothing
    fo'' = mergeFieldOptions fo fo'
    cfos = fo'' ^. foChildren
    pos' = if null cfos then pos else (buildPrintOptions cfos):pos
    remove = maybe False (\f -> f sv) (fo'' ^. foRemove)
  in
    if remove then
      (Nothing, pri, key)
    else
      (Just (encodePretty' pos' fo'' indent sv), pri, key)

encodePretty' :: [PrintOptions] -> FieldOptions -> Int -> Value -> B.Builder
encodePretty' _pos _fo _indent Null = "null"
encodePretty' _pos _fo _indent (Bool b) = if b then "true" else "false"
encodePretty' _pos fo _indent (Number n) = maybe fieldGeneralFormat id (fo ^. foNumberFormat) $ n
encodePretty' _pos _fo _indent (String s) = lazyByteString $ encode s
encodePretty' pos fo indent v@(Array av) = let
    po = headWithDefault def pos
    inline = maybe False (\f -> f v) (fo ^. foInline)
    indent' = indent + (po ^. poIndent)
    parts' = catMaybes $ map (\(p', v') -> encodeArrayElt pos fo p' indent' v v') $ zip [0..] (V.toList av)
    chunks = maybe [parts'] (\n -> chunksOf n parts') (fo ^. foWrap)
    indentBuffer = makeIndentBuffer indent
    indentBuffer' = makeIndentBuffer indent'
    inlineBuffers vs = mconcat $ Data.List.intersperse ", " vs
  in
    if null parts' then
      "[ ]"
    else
      if inline then
        if length chunks == 1 then
          "[ " <> inlineBuffers parts' <> " ]"
        else
          "["
          <> (fst $ foldl (\(bs, follow) -> \c -> (bs <> (if follow then "," else mempty) <> "\n" <> indentBuffer' <> inlineBuffers c, True)) (mempty, False) chunks)
          <> "\n" <> indentBuffer <> "]"
      else
        "["
        <> (fst $ foldl (\(bs, follow) -> \b -> (bs <> (if follow then "," else mempty) <> "\n" <> indentBuffer' <> b, True)) (mempty, False) parts')
        <> "\n" <> indentBuffer <> "]"
encodePretty' pos fo indent v@(Object km) = let
    po = headWithDefault def pos
    inline = maybe False (\f -> f v) (fo ^. foInline)
    indent' = indent + (po ^. poIndent)
    indentBuffer = makeIndentBuffer indent
    indentBuffer' = makeIndentBuffer indent'
    parts' = map (\k -> encodeField pos fo k indent' v $ maybe Null id $ KM.lookup k km) (KM.keys km)
    parts'' = sortBy keyOrder parts'
  in
    if null parts'' then
      "{ }"
    else
      if inline then
        "{ "
        <> (fst $ foldl (\(bs, follow) -> \(mb, _, k) -> maybe (bs, follow) (\b -> (bs <> (if follow then ", " else mempty) <> (B.stringUtf8 $ show k) <> ": " <> b, True)) mb) (mempty, False) parts'')
        <> " }"
      else
        "{"
        <> (fst $ foldl (\(bs, follow) -> \(mb, _, k) -> maybe (bs, follow) (\b -> (bs <> (if follow then "," else mempty) <> "\n" <> indentBuffer' <> (B.stringUtf8 $ show k) <> ": " <> b, True)) mb) (mempty, False) parts'')
        <> "\n" <> indentBuffer <> "}"

encodePretty :: PrintOptions -> Value -> LB.ByteString
encodePretty po val = B.toLazyByteString $ encodePretty' [po] def 0 val

_traceL :: Int -> Value -> String
_traceL _n Null = "null"
_traceL _n (Bool b) = show b
_traceL _n (Number n) = show n
_traceL _n (String s) = "\"" ++ (T.unpack s) ++ "\""
_traceL n (Array vs) = "[" ++ segs ++ "]"
  where
    segs = if n <= 0 then
        ""
      else
        Data.List.intercalate ", " (map (_traceL (n - 1)) (V.toList vs))
_traceL n (Object km) = "{" ++ segs ++ "}"
  where
    segs = if n <= 0 then
        ""
      else
        Data.List.intercalate ", " (map (\(k, v) -> show k ++ ":" ++ _traceL (n - 1) v) (KM.toList km))