{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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
    Placeholder(..)
) where

-- | Something that can be represented by a placeholder within a particular context
class Placeholder ctx a | a -> ctx where
  -- | The placeholder identifier
  placeholderID :: a -- ^ The item to identify
    -> String -- ^ The resulting identifier
  -- | Create a placeholder for an identifier
  placeholder :: String -- ^ The identifier
    -> a -- ^ A placeholder
  -- | Normalise an object of this sort
  normalise :: ctx -- ^ The context, this can be used to lookup the un-normalised version and see if the context already has a value
    -> a -- ^ The un-normalised item, presumably a placeholder
    -> a -- ^ The normalised item