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
module Camino.Util (
    canonicalise
  , categorise
  , listUnions
  , partition
  , selectFromList
  , toFileName
) where

import Data.Char (isLetter)
import qualified Data.Set as S
import qualified Data.Text as T
 
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
canonicalise' '\x00df' = 'S' -- sharp S
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
categorise v = categorise' $ T.head $ canonicalise $ T.toUpper $ T.take 1 v


-- | Convert a piece of text into something that will pass muster as a file name
toFileName :: T.Text -> T.Text
toFileName v = T.filter isLetter $ canonicalise v