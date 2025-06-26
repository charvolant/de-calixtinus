{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Partial Ordeing
Description : Handling of partial orders and topological sorts
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

A module that allows the definition of a partial order
-}
module Data.Partial (
  PartialOrder,

  topologicalSort
) where

import Data.Foldable (foldl')
import Data.List (partition)


-- | A partial order over a.
--   If the partial order evaluates to true, then the first argument is less than or equal to the second argument.
--   It does not follow that if false, the second argument is less than or equal to the first argument
type PartialOrder a = a -> a -> Bool

-- | Topological sort
topologicalSort :: (Eq a) => PartialOrder a -- ^ The partial order to use
  -> [a] -- ^ The unsorted elements
  -> [a] -- ^ The sorted elements
topologicalSort _po [] = []
topologicalSort _po x@[ _ ] = x
topologicalSort po x@(h:_) = let
    x' = foldl' (\v1 -> \v2 -> if po v2 v1 then v2 else v1) h x
    (front, back) = partition (\v -> v == x') x
  in
    front ++ topologicalSort po back


