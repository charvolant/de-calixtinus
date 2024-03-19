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
) where

import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.ICU.Char (Bool_(..), property)
import Data.Text.ICU.Normalize2 (NormalizationMode(..), normalize)
 
-- | Select elements from a list that are in a set, keeping the order of the list
selectFromList :: (Ord a, Eq a) => S.Set a -- ^ The elements to select
  -> [a] -- ^ The source list
  -> [a] -- ^ The resulting list of selected elements, keeping the order of the source list
selectFromList sel = filter (\v -> S.member v sel)

-- Construct a union of a set of elements, only including elements that have not already been seen
-- and preserving the implicit order of the input list
listUnions :: (Eq a, Foldable c) => [c a] -> [a]
listUnions elts = foldl (\acc coll -> foldl (\acc' v -> if elem v acc' then acc' else acc' ++ [v]) acc coll) [] elts


-- | Canonicalise text, removing accents and diacritics
canonicalise :: T.Text -> T.Text
canonicalise t = T.filter (not . property Diacritic) (normalize NFD t)

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