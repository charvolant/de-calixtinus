{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Cache
Description : Cache data and expire it after a while
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX



-}
module Data.Cache (
    Cache(..)
  , CompositeCache(..)
  , FileCache(..)
  , MemCache(..)

  , compositeCache
  , defaultFileCache
  , defaultMemCache
  , defaultMemCacheWithExpiry
) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Char (isAlphaNum)
import Data.List ((!?), singleton, sortBy)
import qualified Data.Map as M
import Data.Time.Clock
import Data.Util
import System.Directory
import System.FilePath
import System.IO

-- | A cache.
--   Caches store information up to some limit and will then expire unused cache items
class Cache c k v | c -> k, c -> v where
  -- The number of entries in the cache
  cacheEntries :: c -- ^ The cache
    -> IO Int -- ^ The current number of entries
  -- See if an item is in the cache
  cacheLookup :: k -- ^ The key to lookup
    -> c -- ^ The cache
    -> IO (Maybe v, c) -- ^ The found key and the updated cache
  -- Put an item into the cache
  cachePut :: k -- ^ The key
    -> v -- ^ The cache value
    -> c -- ^ The cache to update
    -> IO c -- ^ The resulting cache
  -- Delete an item from the cache
  cacheDelete :: k -- ^ The key of the object to delete
    -> c -- ^ The cache to delete from
    -> IO c -- ^ The updated cache
  -- Free up space for new entries in the cache, if the cache has a size policy
  cacheFree :: Int -- ^ The number of spaces to ensure are free
    -> c -- ^ The cache to expire
    -> IO c -- ^ The updated cache
  -- Expire any entries in a cache, if the cache has an expiry policy
  cacheExpire ::  c -- ^ The cache to expire
    -> IO c -- ^ The updated cache
  -- Load the cache from whatever backing store is used, if any
  cacheLoad :: c -- The unloaded cache
    -> IO c -- ^ The loaded cache

-- | A cache entry.
--   A common entry type for a cache to allow ease of expiry
data (Ord k) => CacheEntry k v = CacheEntry {
    ceKey :: k
  , ceValue :: v
  , ceCreated :: UTCTime
  , ceAccessed :: UTCTime
}

instance (Ord k, ToJSON k, ToJSON v) => ToJSON (CacheEntry k v) where
  toJSON (CacheEntry key' value' created' accessed') = object [
      "key" .= key'
    , "value" .= value'
    , "created" .= created'
    , "accessed" .= accessed'
    ]

instance (Ord k, FromJSON k, FromJSON v) => FromJSON (CacheEntry k v) where
  parseJSON (Object v) = do
    key' <- v .: "key"
    value' <- v .: "value"
    created' <- v .: "created"
    accessed' <- v .: "accessed"
    return $ CacheEntry key' value' created' accessed'
  parseJSON v = typeMismatch "object" v

instance (Ord k, Show k, Show v) => Show (CacheEntry k v) where
  show ce = "{" ++ show (ceKey ce) ++ ", " ++ show (ceValue ce) ++ ", " ++ show (ceCreated ce) ++ ", " ++ show (ceAccessed ce) ++ "}"

data CachePolicy k v = CachePolicy {
    cpSize :: Maybe Int -- ^ The maximum size of the cache
  , cpRetain :: Maybe NominalDiffTime -- ^ The maximum age of data in the cache, if absent than data is returned indefinately
  , cpScan :: Maybe NominalDiffTime -- ^ The minimum time before a rescan for expiry, if not set then rescans occur any time requested
  , cpRemove :: Int -> [CacheEntry k v] -> [k] -- ^ How to choose which cache entries to remove
}

rescanCheck :: CachePolicy k v -> Maybe UTCTime -> UTCTime -> Maybe UTCTime
rescanCheck (CachePolicy _ Nothing _ _) _ _ = Nothing
rescanCheck (CachePolicy _ (Just retain) Nothing _) _ current = Just $ addUTCTime (negate retain) current
rescanCheck (CachePolicy _ (Just retain) _ _) Nothing current = Just $ addUTCTime (negate retain) current
rescanCheck (CachePolicy _ (Just retain) (Just sc) _) (Just ls) current = if addUTCTime sc ls <= current then (Just $ addUTCTime (negate retain) current) else Nothing

toRetainTime :: (Real t) => t -> NominalDiffTime
toRetainTime v = secondsToNominalDiffTime $ realToFrac v

toScanTime :: (Real t) => t -> NominalDiffTime
toScanTime t = secondsToNominalDiffTime sc
  where sc = max 0.1 (realToFrac t / 10.0)

-- | Least recently used cache removal policy
cacheRemovalLRU :: (Ord k) => Int -> [CacheEntry k v] -> [k]
cacheRemovalLRU remove entries = let
    ordered = sortBy (\ce1 -> \ce2 -> compare (ceAccessed ce1) (ceAccessed ce2)) entries
  in
    map ceKey (take remove ordered)

-- | An in-memory cache.
--   The cache policy provides
data (Ord k) => MemCache k v = MemCache {
    mcPolicy :: CachePolicy k v
  , mcEntries :: M.Map k (CacheEntry k v) -- ^ The list of current entries
  , mcLastScan :: Maybe UTCTime -- The time of the last expiry scan
}

instance (Ord k) => Cache (MemCache k v) k v where
  cacheEntries cache = return $ M.size $ mcEntries cache
  cacheLookup key cache = do
    let mce = M.lookup key (mcEntries cache)
    current <- getCurrentTime
    return $ maybe (Nothing, cache) (\ce -> let
        cache' = cache {
          mcEntries = M.adjust (\ce' -> ce' { ceAccessed = current }) key (mcEntries cache)
        }
      in
        (Just (ceValue ce), cache')
      ) mce
  cachePut key value cache = do
    current <- getCurrentTime
    cache' <- cacheFree 1 cache
    let ce = CacheEntry key value current current
    let cache'' = cache' {
      mcEntries = M.insert key ce (mcEntries cache')
    }
    return cache''
  cacheDelete key cache = do
    let cache' = cache {
      mcEntries = M.delete key (mcEntries cache)
    }
    return cache'
  cacheFree space cache = maybe (return cache) (\sz -> let
        entries = mcEntries cache
        removals = max 0 ((space + M.size entries) - sz)
        remove = (cpRemove $ mcPolicy cache) removals (M.elems entries)
        entries' = foldr (\k -> \es -> M.delete k es) entries remove
      in
        return $ cache { mcEntries = entries' }
      ) (cpSize $ mcPolicy cache)
  cacheExpire cache = do
    current <- getCurrentTime
    return $ maybe cache (\expire -> let
        entries = mcEntries cache
        expired = M.filter (\ce -> ceAccessed ce < expire) entries
        entries' = M.difference entries expired
      in
        cache { mcEntries = entries', mcLastScan = Just current }
      ) (rescanCheck (mcPolicy cache) (mcLastScan cache) current)
  cacheLoad cache = return cache

-- | Create a default memory cache with a specific size
--   The default cache has a least recently used removal policy
defaultMemCache :: (Ord k) => Int -> MemCache k v
defaultMemCache size = MemCache {
      mcPolicy = CachePolicy {
        cpSize = Just size
        , cpRetain = Nothing
        , cpScan = Nothing
        , cpRemove = cacheRemovalLRU
      }
    , mcEntries = M.empty
    , mcLastScan = Nothing
  }

-- | Create a default memory cache with a specific size and expiry time
--   The default cache has a least recently used removal policy
defaultMemCacheWithExpiry :: (Ord k, Real t) => Int -> t -> MemCache k v
defaultMemCacheWithExpiry size expiry = MemCache {
      mcPolicy = CachePolicy {
          cpSize = Just size
        , cpRetain = Just $ toRetainTime expiry
        , cpScan = Just $ toScanTime expiry
        , cpRemove = cacheRemovalLRU
      }
    , mcEntries = M.empty
    , mcLastScan = Nothing
  }


-- | A file-based cache.
--   Cache entries are stored as JSON files, with the file system providing
data (Ord k, ToJSON v, FromJSON v) => FileCache k v = FileCache {
    fcPolicy :: CachePolicy k v
  , fcRoot :: FilePath -- ^ The root location of a file
  , fcNamer :: k -> String -- ^ Convert a key into a string that gives the base of a file name without extensions, path etc
  , fcLastScan :: Maybe UTCTime -- ^ The time of the last scan
}

fileForKey :: (Ord k, ToJSON v, FromJSON v) => FileCache k v -> k -> FilePath
fileForKey cache key = let
    filename = (fcNamer cache) key
    root = fcRoot cache
    level0 = root </> maybe "_0" singleton (filename !? 0)
    level1 = level0 </> maybe "_0" singleton (filename !? 1)
  in
    level1 </> filename <.> "json"

readCacheFile :: (FromJSON v) => FilePath -> IO v
readCacheFile path = do
  lb <- SB.readFile path
  let ev = eitherDecodeStrict lb
  case ev of
    Left ex -> error ex
    Right value -> return value

writeCacheFile :: (ToJSON v) => FilePath -> v -> IO ()
writeCacheFile path value =
  withFile path WriteMode (\h -> do
    let encoded = encode value
    LB.hPut h encoded
    hFlush h
    )

instance (Ord k, Show k, ToJSON v, FromJSON v) => Cache (FileCache k v) k v where
  cacheEntries cache = foldDirectory (fcRoot cache) (\c -> \_f -> c + 1) 0
  cacheLookup key cache = do
    let path = fileForKey cache key
    cache1 <- cacheExpire cache
    exists <- doesFileExist path
    if not exists then
      return $! (Nothing, cache1)
    else do
      value <- readCacheFile path
      return $! (value, cache1)
  cachePut key value cache = do
    let path = fileForKey cache key
    let dir = takeDirectory path
    cache1 <- cacheExpire cache
    cache2 <- cacheFree 1 cache1
    createDirectoryIfMissing True dir
    writeCacheFile path value
    return cache2
  cacheDelete       key cache = do
    let path = fileForKey cache key
    cache1 <- cacheExpire cache
    exists <- doesFileExist path
    when exists (removeFile path)
    return cache1
  cacheFree _entries cache = return cache -- Ignores size for now
  cacheExpire cache = do
    current <- getCurrentTime
    maybe
      (return $! cache)
      (\expire -> do
        scanDirectory
          (\f -> do
            access <- getAccessTime f
            when (access < expire) (removeFile f)
          )
          (\_d -> return ())
          (fcRoot cache)
        return $! cache { fcLastScan = Just current }
      )
      (rescanCheck (fcPolicy cache) (fcLastScan cache) current)
  cacheLoad cache = do
    cache1 <- cacheExpire cache
    return cache1

-- | Create a default file cache with a specific lcation and expiry time
--   The file key is just the string version of the key
--   The rescan interval is every 10th of the longevity
defaultFileCache :: (Ord k, Show k, FromJSON v, ToJSON v, Real t) => FilePath -> t -> FileCache k v
defaultFileCache root expiry = FileCache {
      fcPolicy = CachePolicy {
        cpSize = Nothing
        , cpRetain = Just $ toRetainTime expiry
        , cpScan = Just $ toScanTime expiry
        , cpRemove = cacheRemovalLRU
      }
    , fcRoot = root
    , fcNamer = filter isAlphaNum . show
    , fcLastScan = Nothing
  }

-- | A composite cache with a primary and secondary cache
--   The primary cache is usually small and allows fast retrieval.
--   The secondary cache is slower but provides larger, persistent storage.
data (Ord k, Cache c1 k v, Cache c2 k v) => CompositeCache c1 c2 k v = CompositeCache {
    ccPrimary :: c1 -- The primary cache
  , ccSecondary :: c2 -- The secondary cache
}

instance (Ord k, Cache c1 k v, Cache c2 k v) => Cache (CompositeCache c1 c2 k v) k v where
  cacheEntries cache = cacheEntries (ccPrimary cache)
  cacheLookup key cache = do
    (v1, c1) <- cacheLookup key (ccPrimary cache)
    case v1 of
      Nothing -> do
        (v2, c2) <- cacheLookup key (ccSecondary cache)
        case v2 of
          Nothing -> return (v2, cache { ccPrimary = c1, ccSecondary = c2 })
          (Just v2') -> do
            c1' <- cachePut key v2' c1
            return (v2, cache { ccPrimary = c1', ccSecondary = c2 })
      (Just _) -> return (v1, cache { ccPrimary = c1 })
  cachePut key value cache = do
    primary' <- cachePut key value (ccPrimary cache)
    secondary' <- cachePut key value (ccSecondary cache)
    return cache { ccPrimary = primary', ccSecondary = secondary' }
  cacheDelete key cache = do
    primary' <- cacheDelete key (ccPrimary cache)
    secondary' <- cacheDelete key (ccSecondary cache)
    return cache { ccPrimary = primary', ccSecondary = secondary' }
  cacheFree entries cache = do
    primary' <- cacheFree entries (ccPrimary cache)
    secondary' <- cacheFree entries (ccSecondary cache)
    return cache { ccPrimary = primary', ccSecondary = secondary' }
  cacheExpire cache = do
    primary' <- cacheExpire (ccPrimary cache)
    secondary' <- cacheExpire (ccSecondary cache)
    return cache { ccPrimary = primary', ccSecondary = secondary' }
  cacheLoad cache = do
    primary' <- cacheLoad (ccPrimary cache)
    secondary' <- cacheLoad (ccSecondary cache)
    return cache { ccPrimary = primary', ccSecondary = secondary' }

-- | Create a composite cache out of two other caches
compositeCache :: (Ord k, Cache c1 k v, Cache c2 k v) => c1 -> c2 -> CompositeCache c1 c2 k v
compositeCache primary secondary = CompositeCache {
    ccPrimary = primary
  , ccSecondary = secondary
  }