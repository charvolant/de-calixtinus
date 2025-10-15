{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Utilities
Description : 
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Some useful common functions
-}
module Data.Util (
    backupFilePath
  , canonicalise
  , categorise
  , ceilingBy
  , commaJoin
  , expandPath
  , floorBy
  , foldDirectory
  , foldDirectoryIO
  , headWithDefault
  , headWithError
  , initOrEmpty
  , lastWithDefault
  , lastWithError
  , listUnions
  , loopM
  , maybeMax
  , maybeMin
  , maybeSum
  , partition
  , roundBy
  , scanDirectory
  , selectFromList
  , tailOrEmpty
  , toFileName
  , unique
) where

import Control.Monad
import Data.Char (isLetter, isPunctuation)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath

-- | Select elements from a list that are in a set, keeping the order of the list
selectFromList :: (Ord a) => S.Set a -- ^ The elements to select
  -> [a] -- ^ The source list
  -> [a] -- ^ The resulting list of selected elements, keeping the order of the source list
selectFromList sel = filter (\v -> S.member v sel)

-- Construct a union of a set of elements, only including elements that have not already been seen
-- and preserving the implicit order of the input list
listUnions :: (Eq a, Foldable c) => [c a] -> [a]
listUnions elts = foldl (\acc coll -> foldl (\acc' v -> if elem v acc' then acc' else acc' ++ [v]) acc coll) [] elts

-- | Basic canonicalisation of common European accented characters
--   OK for Camino placenames
canonicalise' :: Char -> Char
canonicalise' '\x00c0' = 'A' -- A grave
canonicalise' '\x00c1' = 'A' -- A acute
canonicalise' '\x00c2' = 'A' -- A curcumflex
canonicalise' '\x00c3' = 'A' -- A tilde
canonicalise' '\x00c4' = 'A' -- A diaresis
canonicalise' '\x00c5' = 'A' -- A ring
canonicalise' '\x00c6' = 'A' -- AE
canonicalise' '\x00c7' = 'C' -- C cedilla
canonicalise' '\x00c8' = 'E' -- E grave
canonicalise' '\x00c9' = 'E' -- E acute
canonicalise' '\x00ca' = 'E' -- E curcumflex
canonicalise' '\x00cb' = 'E' -- E diaresis
canonicalise' '\x00cc' = 'I' -- I grave
canonicalise' '\x00cd' = 'I' -- I acute
canonicalise' '\x00ce' = 'I' -- I curcumflex
canonicalise' '\x00cf' = 'I' -- I diaresis
canonicalise' '\x00d0' = 'D' -- Eth
canonicalise' '\x00d1' = 'N' -- N tilde
canonicalise' '\x00d2' = 'O' -- O grave
canonicalise' '\x00d3' = 'O' -- O acute
canonicalise' '\x00d4' = 'O' -- O curcumflex
canonicalise' '\x00d5' = 'O' -- O tilde
canonicalise' '\x00d6' = 'O' -- O diaresis
canonicalise' '\x00d8' = 'O' -- O slash
canonicalise' '\x00d9' = 'U' -- U grave
canonicalise' '\x00da' = 'U' -- U acute
canonicalise' '\x00db' = 'U' -- U curcumflex
canonicalise' '\x00dc' = 'U' -- U diaresis
canonicalise' '\x00dd' = 'Y' -- Y acute
canonicalise' '\x00de' = 'T' -- thorn
canonicalise' '\x00df' = 's' -- sharp s
canonicalise' '\x00e0' = 'a' -- a grave
canonicalise' '\x00e1' = 'a' -- a acute
canonicalise' '\x00e2' = 'a' -- a curcumflex
canonicalise' '\x00e3' = 'a' -- a tilde
canonicalise' '\x00e4' = 'a' -- a diaresis
canonicalise' '\x00e5' = 'a' -- a ring
canonicalise' '\x00e6' = 'a' -- ae
canonicalise' '\x00e7' = 'c' -- c cedilla
canonicalise' '\x00e8' = 'e' -- e grave
canonicalise' '\x00e9' = 'e' -- e acute
canonicalise' '\x00ea' = 'e' -- e curcumflex
canonicalise' '\x00eb' = 'e' -- e diaresis
canonicalise' '\x00ec' = 'i' -- i grave
canonicalise' '\x00ed' = 'i' -- i acute
canonicalise' '\x00ee' = 'i' -- i curcumflex
canonicalise' '\x00ef' = 'i' -- i diaresis
canonicalise' '\x00f0' = 'd' -- eth
canonicalise' '\x00f1' = 'n' -- N tilde
canonicalise' '\x00f2' = 'o' -- o grave
canonicalise' '\x00f3' = 'o' -- o acute
canonicalise' '\x00f4' = 'o' -- o curcumflex
canonicalise' '\x00f5' = 'o' -- o tilde
canonicalise' '\x00f6' = 'o' -- o diaresis
canonicalise' '\x00f8' = 'o' -- o slash
canonicalise' '\x00f9' = 'u' -- u grave
canonicalise' '\x00fa' = 'u' -- u acute
canonicalise' '\x00fb' = 'u' -- u curcumflex
canonicalise' '\x00fc' = 'u' -- u diaresis
canonicalise' '\x00fd' = 'y' -- Y acute
canonicalise' '\x00fe' = 't' -- thorn
canonicalise' '\x00ff' = 'y' -- y diaresis
canonicalise' '\x0100' = 'A' -- A macron
canonicalise' '\x0101' = 'a' -- a macron
canonicalise' '\x0106' = 'C' -- C acute
canonicalise' '\x0107' = 'c' -- c acute
canonicalise' '\x0108' = 'C' -- C circumflex
canonicalise' '\x0109' = 'c' -- c circumfles
canonicalise' '\x010a' = 'C' -- C dot
canonicalise' '\x010b' = 'c' -- c dot
canonicalise' '\x010c' = 'C' -- C caron
canonicalise' '\x010d' = 'c' -- c caron
canonicalise' '\x010e' = 'D' -- D caron
canonicalise' '\x010f' = 'd' -- d caron
canonicalise' '\x0110' = 'D' -- D stroke
canonicalise' '\x0111' = 'd' -- d stroke
canonicalise' '\x0112' = 'E' -- E macron
canonicalise' '\x0113' = 'e' -- e macron
canonicalise' '\x0116' = 'E' -- E dot
canonicalise' '\x0117' = 'e' -- e dot
canonicalise' '\x0118' = 'E' -- E ogonek
canonicalise' '\x0119' = 'e' -- e ogonek
canonicalise' '\x011a' = 'E' -- E caron
canonicalise' '\x011b' = 'e' -- e caron
canonicalise' '\x0128' = 'I' -- I tilde
canonicalise' '\x0129' = 'i' -- i tilde
canonicalise' '\x012a' = 'I' -- I macron
canonicalise' '\x012b' = 'i' -- i macron
canonicalise' '\x012e' = 'I' -- I ogonek
canonicalise' '\x012f' = 'i' -- i ogonek
canonicalise' '\x0130' = 'I' -- I dot
canonicalise' '\x0131' = 'i' -- i dotless
canonicalise' '\x013b' = 'L' -- L cedilla
canonicalise' '\x013c' = 'l' -- l cedilla
canonicalise' '\x0141' = 'L' -- L stroke
canonicalise' '\x0142' = 'l' -- l stroke
canonicalise' '\x0143' = 'N' -- N acute
canonicalise' '\x0144' = 'n' -- n acute
canonicalise' '\x0145' = 'N' -- N cedilla
canonicalise' '\x0146' = 'n' -- n cedilla
canonicalise' '\x014a' = 'N' -- Eng
canonicalise' '\x014b' = 'n' -- eng
canonicalise' '\x014c' = 'O' -- O macron
canonicalise' '\x014d' = 'o' -- o macron
canonicalise' '\x0152' = 'O' -- OE
canonicalise' '\x0153' = 'o' -- oe
canonicalise' '\x0158' = 'R' -- R caron
canonicalise' '\x0159' = 'r' -- r caron
canonicalise' '\x015a' = 'S' -- S acute
canonicalise' '\x015b' = 's' -- s acute
canonicalise' '\x015e' = 'S' -- S cedilla
canonicalise' '\x015f' = 's' -- s cedilla
canonicalise' '\x0160' = 'S' -- S caron
canonicalise' '\x0161' = 's' -- s caron
canonicalise' '\x0164' = 'T' -- T caron
canonicalise' '\x0165' = 't' -- t caron
canonicalise' '\x0168' = 'U' -- U tilde
canonicalise' '\x0169' = 'u' -- u tilde
canonicalise' '\x016a' = 'U' -- U macron
canonicalise' '\x016b' = 'u' -- u macron
canonicalise' '\x016e' = 'U' -- U ring
canonicalise' '\x016f' = 'u' -- u ring
canonicalise' '\x0170' = 'U' -- U double acute
canonicalise' '\x0171' = 'u' -- u double acute
canonicalise' '\x0174' = 'W' -- W circumflex
canonicalise' '\x0175' = 'w' -- w circumflex
canonicalise' '\x0176' = 'Y' -- Y circumflex
canonicalise' '\x0177' = 'y' -- y circumflex
canonicalise' '\x0178' = 'Y' -- Y umlaut
canonicalise' '\x0179' = 'Z' -- Z acute
canonicalise' '\x017a' = 'z' -- z acute
canonicalise' '\x017b' = 'Z' -- Z dot
canonicalise' '\x017c' = 'z' -- z dot
canonicalise' '\x017d' = 'Z' -- Z caron
canonicalise' '\x017e' = 'z' -- z caron
canonicalise' '\x01cd' = 'A' -- A caron
canonicalise' '\x01ce' = 'a' -- a caron
canonicalise' '\x01cf' = 'I' -- I caron
canonicalise' '\x01d0' = 'i' -- i caron
canonicalise' '\x01d1' = 'O' -- O caron
canonicalise' '\x01d2' = 'o' -- o caron
canonicalise' '\x01d3' = 'U' -- U caron
canonicalise' '\x01d4' = 'u' -- u caron
canonicalise' '\x0218' = 'S' -- S comma below
canonicalise' '\x0219' = 's' -- s comma below
canonicalise' '\x021a' = 'T' -- T comma below
canonicalise' '\x021b' = 't' -- t comma below
canonicalise' '\x1e0e' = 'D' -- D line below
canonicalise' '\x1e0f' = 'd' -- d line below
canonicalise' '\x1e3a' = 'L' -- L line below
canonicalise' '\x1e3b' = 'l' -- l line below
canonicalise' '\x1e48' = 'N' -- N line below
canonicalise' '\x1e49' = 'n' -- n line below
canonicalise' '\x1e5e' = 'R' -- R line below
canonicalise' '\x1e5f' = 'r' -- r line below
canonicalise' '\x1e6e' = 'T' -- T line below
canonicalise' '\x1e6f' = 't' -- t line below
canonicalise' '\x1e9e' = 'S' -- Capital sharp s
canonicalise' '\x1ebc' = 'E' -- E circumflex acute
canonicalise' '\x1ebd' = 'e' -- e circumflex acute
canonicalise' c = c


-- | Canonicalise text, removing accents and diacritics
canonicalise :: T.Text -> T.Text
canonicalise t = T.map canonicalise' t

partition' :: Eq t => (a -> t) -> [a] -> t -> [a] -> [(t, [a])]
partition' _classifier [] cl segment = [(cl, reverse segment)]
partition' classifier (s:source) cl segment = if cl' == cl then 
    partition' classifier source cl (s:segment) 
  else 
    (cl, reverse segment):(partition' classifier source cl' [s])
  where cl' = classifier s

-- | Split a sorted list into a partition, based on some sort of partition function
partition :: (Eq b) => (a -> b) -- ^ The classifier function, produces the element to split the list on
  -> [a] -- ^ The source list
  -> [(b, [a])] -- ^ A resulting list of category - elements that fit the category pairs
partition _classifier [] = []
partition classifier (s:source) = partition' classifier source (classifier s) [s]

-- | Choose a category based on initial letter
categorise' :: Char -> T.Text
categorise' 'A' = "A-E"
categorise' 'B' = "A-E"
categorise' 'C' = "A-E"
categorise' 'D' = "A-E"
categorise' 'E' = "A-E"
categorise' 'F' = "F-M"
categorise' 'G' = "F-M"
categorise' 'H' = "F-M"
categorise' 'I' = "F-M"
categorise' 'J' = "F-M"
categorise' 'K' = "F-M"
categorise' 'L' = "F-M"
categorise' 'M' = "F-M"
categorise' 'N' = "N-T"
categorise' 'O' = "N-T"
categorise' 'P' = "N-T"
categorise' 'Q' = "N-T"
categorise' 'R' = "N-T"
categorise' 'S' = "N-T"
categorise' 'T' = "N-T"
categorise' 'U' = "U-Z"
categorise' 'V' = "U-Z"
categorise' 'W' = "U-Z"
categorise' 'X' = "U-Z"
categorise' 'Y' = "U-Z"
categorise' 'Z' = "U-Z"
categorise' _ = "..."

-- | Divide text entries into alhpabetic groups
categorise :: T.Text -> T.Text
categorise "" = "..."
categorise v = categorise' $ T.head $ canonicalise $ T.toUpper $ T.take 1 v


-- | Convert a piece of text into something that will pass muster as a file name
toFileName :: T.Text -> T.Text
toFileName v = T.filter isLetter $ canonicalise v

-- | Add two maybe numbers together
maybeSum :: (Num a) => Maybe a -> Maybe a -> Maybe a
maybeSum Nothing Nothing = Nothing
maybeSum Nothing a = a
maybeSum a Nothing = a
maybeSum (Just a) (Just b) = Just (a + b)

-- | Minimum of two maybe numbers
maybeMin :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maybeMin Nothing Nothing = Nothing
maybeMin Nothing a = a
maybeMin a Nothing = a
maybeMin (Just a) (Just b) =Just $ min a b

-- | Maximum of two maybe numbers
maybeMax :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maybeMax Nothing Nothing = Nothing
maybeMax Nothing a = a
maybeMax a Nothing = a
maybeMax (Just a) (Just b) =Just $ max a b

-- | Create a unique list in list order with the first unique element first
unique :: (Ord a) => [a] -> [a]
unique l = unique' S.empty l

unique' _seen [] = []
unique' seen (v:t) = if S.member v seen then unique' seen t else v:(unique' (S.insert v seen) t)

-- | Join a list of pieces of text.
--   Use commas if there is no obvious punctuation at the end, otherwise use spaces
commaJoin :: [T.Text] -> T.Text
commaJoin [] = ""
commaJoin (t:r) = let
  t' = T.strip t
  sp = isPunctuation $ T.last t'
  r' = commaJoin r
  in
    if T.null r' then
      t'
    else if sp then
      t' <> " " <> r'
    else
      t' <> ", " <> r'


-- | Recursively scan a directory, performing an action on each file
scanDirectory :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> FilePath -> IO ()
scanDirectory faction daction dir = do
  list <- listDirectory dir
  forM_ list (\f -> do
      let ff = dir </> f
      isFile <- doesFileExist ff
      when isFile (faction ff)
      when (not isFile) (do
        scanDirectory faction daction ff
        )
    )
  daction dir

-- | Recursively scan a directory, folding a value as we go
foldDirectory :: FilePath -> (a -> FilePath -> a) -> a -> IO a
foldDirectory dir folder val = do
  list <- listDirectory dir
  value <- foldM (\v -> \f -> do
      let ff = dir </> f
      isFile <- doesFileExist ff
      if isFile then
          return $ folder v ff
       else
          foldDirectory ff folder v
    ) val list
  return value

-- | Recursively scan a directory, folding a value as we go
foldDirectoryIO :: FilePath -> (a -> FilePath -> IO a) -> a -> IO a
foldDirectoryIO dir folder val = do
  list <- listDirectory dir
  value <- foldM (\v -> \f -> do
      let ff = dir </> f
      isFile <- doesFileExist ff
      if isFile then
          folder v ff
       else
          foldDirectoryIO ff folder v
    ) val list
  return value

-- | Return the first value from a list, if any, satisfying the given predicate.
--  Borrowed from Control.Monad.Loop
firstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstM _ [] = return Nothing
firstM p (x:xs) = do
        q <- p x
        if q
                then return (Just x)
                else firstM p xs

-- Create an backup file path with an unused filename as the backup
backupFilePath :: FilePath -> IO FilePath
backupFilePath file = do
  let dir = takeDirectory file
  let base = takeBaseName file
  let ext = takeExtension file
  let names = map (\i -> dir </> (base <> "-" <> show i) <.> ext) [1 :: Int ..]
  mname <- firstM (\f -> do
    exist <- doesFileExist f
    return $ not exist) names
  return $ maybe (error ("Can't create backup for " ++ file)) id mname



-- | A monadic loop, where the predicate returns 'Left' as a seed for the next loop
--   or 'Right' to abort the loop.
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
    res <- act x
    case res of
        Left x' -> loopM act x'
        Right v -> pure v

-- | Expand a path with environment variables.
--   Any environment variable of the form `$VAR` is expanded into the equivalent environment variable
--   The $TMP variable refers to the temporary file directory
--   Only simple substitions will work, "$TMP/dir" works, as will "/var/$HOME" but "$TMP-1/dir" or "${TMP} will not
expandPath :: FilePath -> IO FilePath
expandPath path = do
  let pieces = splitDirectories path
  pieces' <- mapM expandPath' pieces
  return $ joinPath pieces'

expandPath' :: FilePath -> IO FilePath
expandPath' piece@('$':name) = case name of
  "TMP" -> getTemporaryDirectory
  _ -> do
    mval <- lookupEnv name
    return $ maybe piece id mval
expandPath' piece = return piece

-- | Get the head of a list or a default value
headWithDefault :: a -> [a] -> a
headWithDefault d [] = d
headWithDefault _d (h:_) = h

-- | Get the head of a known non-empty list or throw an error
headWithError :: [a] -> a
headWithError l = headWithDefault (error "Unexpected empty list") l

-- | Get the last element of a list or a default value
lastWithDefault :: a -> [a] -> a
lastWithDefault d [] = d
lastWithDefault _d (h:t) = NE.last (h NE.:| t)

-- | Get the head of a known non-empty list or a default value
lastWithError :: [a] -> a
lastWithError l = lastWithDefault (error "Unexpected empty list") l

-- | Get the tail of a list, or the empty list if the list is already empty
tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty (_:t) = t

-- | Get the init of a list, or the empty list if the list is already empty
initOrEmpty :: [a] -> [a]
initOrEmpty [] = []
initOrEmpty (h:t) = NE.init (h NE.:| t)

-- | Ceiling by units of accuracy
--
-- >>> ceilingBy 0.1 4.56
-- 4.6
-- >>> ceilingBy 10 56
-- 60
ceilingBy :: (RealFrac a) => a -> a -> a
ceilingBy b v = b * fromInteger (ceiling $ v / b)

-- | Floor by units of accuracy
--
-- >>> floorBy 0.1 4.56
-- 4.5
-- >>> floorBy 10 56
-- 50
floorBy :: (RealFrac a) => a -> a -> a
floorBy b v = b * fromInteger (floor $ v / b)

-- | Round by units of accuracy
--
-- >>> roundBy 0.1 4.56
-- 4.6
-- >>> roundBy 10 54
-- 50
roundBy :: (RealFrac a) => a -> a -> a
roundBy b v = b * fromInteger (round $ v / b)