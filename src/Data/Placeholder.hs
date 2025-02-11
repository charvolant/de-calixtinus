{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Placeholder
Description : Use placeholders for objects in Aeson
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Allow objects to be converted from/to an identifier.

This is useful for serialising data where there needs to be references to the data elsewhere and
there isn't enough contextual information yet.

-}

module Data.Placeholder (
    Dereferencer(..)
  , Normaliser(..)
  , Placeholder(..)

  , normaliseReferences
  , placeholderLabel
) where

import Data.List (find, partition)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- | Something that can be represented by a placeholder within a particular context
--   In this case, the placeholder has a key type of @k@
class (Eq k, Ord k, Show k) => Placeholder k a | a -> k where
  -- | The placeholder identifier
  placeholderID :: a -- ^ The item to identify
    -> k -- ^ The resulting identifier
  -- | Create a placeholder for an identifier
  placeholder :: k -- ^ The identifier
    -> a -- ^ A placeholder
  -- | Is this a placeholder instance?
  isPlaceholder :: a -> Bool
  -- | Get the internal dependencies of this placeholder
  --   By default, this returns an empty set, override this if you have internal structure that needs resolving
  internalReferences :: a -- ^ The item
    -> S.Set a
  internalReferences _ = S.empty

class (Placeholder k a) => Normaliser k a ctx | a -> ctx where
  -- | Normalise an item, with references normalised
  --   By default, this returns the original item, override this if you have an internal structure that needs resolving
  normalise :: ctx -- The context to rebuild in
    -> a -- ^ The source object
    -> a -- ^ The resulting

class (Placeholder k a) => Dereferencer k a ctx where
  -- | Dereference an item from a context.
  --   Look up the non-placeholder item in the context and return it or the original object if not found
  dereference :: ctx -- The context to use as a dereferencer
    -> a -- ^ The source object
    -> a -- ^ The resulting, de-referenced object
  -- | Dereference over some sort of functor
  dereferenceF :: (Functor f) => ctx -- The context to use as a dereferencer
    -> f a -- ^ The source object
    -> f a -- ^ The resulting, de-referenced object
  dereferenceF ctx items = fmap (dereference ctx) items
  -- | Dereference over a set of items
  dereferenceS :: (Ord a) => ctx -- The context to use as a dereferencer
    -> S.Set a -- ^ The source object
    -> S.Set a -- ^ The resulting, de-referenced object
  dereferenceS ctx items = S.map (dereference ctx) items


-- | We can always make a dereferecer out of a list
instance (Placeholder k a) => Dereferencer k a [a] where
  dereference ctx v = maybe v id (find (\x -> placeholderID x == vid) ctx) where vid = placeholderID v


-- | We can always make a dereferecer out of a map
instance (Placeholder k a) => Dereferencer k a (M.Map k a) where
  dereference ctx v = maybe v id (M.lookup (placeholderID v) ctx)

-- | We can always make a dereferecer out of a mapping function
instance (Placeholder k a) => Dereferencer k a (k -> Maybe a) where
  dereference ctx v = maybe v id (ctx (placeholderID v))

-- | Normalise internal references
--   This takes a list of items with placeholder references and proceeds to rebuild the set with correct references.
--   This implementation uses
normaliseReferences :: (Normaliser k a (M.Map k a)) => [a] -> [a]
normaliseReferences items = normaliseReferences' items M.empty

normaliseReferences' :: (Normaliser k a (M.Map k a)) => [a] -> M.Map k a -> [a]
normaliseReferences' [] seen = M.elems seen
normaliseReferences' remaining seen = let
    known = M.keysSet seen
    (ready, remaining') = partition (\item -> S.isSubsetOf (S.map placeholderID $ internalReferences item) known) remaining
    rebuilt = if null ready then error ("Unable to normalise " ++ show (map placeholderID remaining)) else map (\item -> normalise seen item) ready
    seen' = M.union seen (M.fromList $ map (\item -> (placeholderID item, item)) rebuilt)
  in
    normaliseReferences' remaining' seen'

-- | Get a placeholder label that identifies the placeholder and if it is actually a placeholder
--   Useful for generating debugging summaries
placeholderLabel :: (Placeholder T.Text a) => a -> T.Text
placeholderLabel p = if isPlaceholder p then placeholderID p <> "*" else placeholderID p
