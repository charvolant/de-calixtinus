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

A store of items, where items can be removed from the cache before the cache gets too large or when the items
get too old.

Caches are, essentially opaqueish handles to IO-like stores.
All interactions take place within the `IO` monad and `IORef`s are used to hold mutable state.

-}
module Data.Cache (
    Cache(..)
  , CacheLogger
  , IsNamer(..)

  , cacheDelete
  , cacheEntries
  , cacheExpire
  , cacheLookup
  , cachePut
  , newCompositeCache
  , newDummyCache
  , newFileCache
  , newMemCache
  , newMemCacheWithExpiry
) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Char (isAlphaNum)
import Data.IORef
import Data.List ((!?), singleton)
import qualified Data.Map as M
import Data.Text (Text, unpack)
import Data.Time.Clock
import Data.Util
import System.Directory
import System.FilePath
import System.IO

-- A cache logging function
type CacheLogger = String -> IO ()

-- | The null logging action
nullLog :: CacheLogger
nullLog _msg = return ()

-- | Items of this sort can be used to name keys, files etc
class IsNamer a where
  toName :: a -> String

instance IsNamer String where
  toName = filter (\c -> isAlphaNum c || c == '_' || c == '-')

instance IsNamer Text where
  toName = toName . unpack

-- | The
data CachePolicy k v = CachePolicy {
    cpSize :: Maybe Int -- ^ The maximum size of the cache
  , cpRetain :: Maybe NominalDiffTime -- ^ The maximum age of data in the cache, if absent than data is returned indefinately
  , cpScan :: Maybe NominalDiffTime -- ^ The minimum time before a rescan for expiry, if not set then rescans occur any time requested
}

-- | A cache
data (Ord k) => Cache k v = Cache {
    caID :: String -- ^ The name, for logging
  , caPolicy :: CachePolicy k v -- ^ The retentionm and expiry policy for this cache
  , caEntries :: Cache k v -> IO Int  -- ^ Return the current number of entries in the cache
  , caLookup :: Cache k v -> k -> IO (Maybe v) -- ^ Lookup a value in the cache
  , caPut :: Cache k v -> k -> v -> IO () -- ^ Put a value into the cache
  , caDelete :: Cache k v -> k -> IO () -- ^ Delete a key from the cache
  , caExpire :: Cache k v -> IO () -- ^ Expire stale entries in the cache
  , caLog :: CacheLogger -- ^ Log information about the cache
}

-- | Get the number of entries in the cache
cacheEntries :: (Ord k) => Cache k v -> IO Int
cacheEntries cache = (caEntries cache) cache

-- | Lookup a value in a cache
cacheLookup :: (Ord k) => Cache k v -> k -> IO (Maybe v)
cacheLookup cache key = (caLookup cache) cache key

-- | Put a value into the cache
cachePut :: (Ord k) => Cache k v -> k -> v -> IO ()
cachePut cache key value = (caPut cache) cache key value

-- | Delete a key from the cache. If the key is not there, then nothing is done
cacheDelete :: (Ord k) => Cache k v -> k -> IO ()
cacheDelete cache key = (caDelete cache) cache key

-- Check the cache for expired entries
cacheExpire :: (Ord k) => Cache k v -> IO ()
cacheExpire cache = (caExpire cache) cache

-- | Log an message about the cache
cacheLog :: (Ord k) => Cache k v -> String -> IO ()
cacheLog cache msg = (caLog cache) (caID cache ++ ": " ++ msg)

-- | A cache entry.
--   A common entry type for a cache to allow ease of expiry calculations
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

toRetainTime :: (Real t) => t -> NominalDiffTime
toRetainTime v = secondsToNominalDiffTime $ realToFrac v

toScanTime :: (Real t) => t -> NominalDiffTime
toScanTime t = secondsToNominalDiffTime sc
  where sc = max 0.1 (realToFrac t / 10.0)

-- | Least recently used cache removal policy
cacheRemovalLRU :: (Ord k) => CacheEntry k v -> CacheEntry k v -> Ordering
cacheRemovalLRU ce1 ce2 = compare (ceAccessed ce1) (ceAccessed ce2)

-- | Create a cache that doesn't cache anything
newDummyCache :: (Ord k) => String -> Cache k v
newDummyCache cid = Cache {
    caID = cid
  , caPolicy = CachePolicy Nothing Nothing Nothing
  , caEntries = \_c -> return 0
  , caLookup = \_c -> \_k -> return Nothing
  , caPut = \_c -> \_k -> \_v -> return ()
  , caDelete = \_c -> \_k -> return ()
  , caExpire = \_c -> return ()
  , caLog = nullLog
  }

-- | An in-memory cache.
--   The cache policy provides rules for removing entries
data (Ord k) => MemCache k v = MemCache {
    mcEntries :: IORef (M.Map k (CacheEntry k v)) -- ^ The list of current entries
  , mcLastScan :: IORef UTCTime -- The time of the last expiry scan
}

memCacheEntries :: (Ord k) => MemCache k v -> Cache k v  -> IO Int
memCacheEntries cache _base = do
  entries <- readIORef (mcEntries cache)
  return $ M.size entries

memCacheLookup :: (Ord k) => MemCache k v -> Cache k v  -> k -> IO (Maybe v)
memCacheLookup cache base key = do
  memCacheExpire cache base
  current <- getCurrentTime
  result <- atomicModifyIORef' (mcEntries cache) (memCacheLookup' current key)
  return result

memCacheLookup' :: (Ord k) => UTCTime -> k -> M.Map k (CacheEntry k v) -> (M.Map k (CacheEntry k v), Maybe v)
memCacheLookup' current key entries = let
    mce = M.lookup key entries
    entries' = maybe entries (\ce -> M.insert key (ce { ceAccessed = current }) entries) mce
  in
    (entries', ceValue <$> mce)

memCachePut :: (Ord k) => MemCache k v -> Cache k v  -> k -> v -> IO ()
memCachePut cache base key value = do
  current <- getCurrentTime
  memCacheFree cache base 1
  memCacheExpire cache base
  atomicModifyIORef' (mcEntries cache) (memCachePut' current key value)

memCachePut' :: (Ord k) => UTCTime -> k -> v -> M.Map k (CacheEntry k v) -> (M.Map k (CacheEntry k v), ())
memCachePut' current key value entries = let
    mce = M.lookup key entries
    ce' = maybe (CacheEntry { ceKey = key, ceValue = value, ceCreated = current, ceAccessed = current }) (\ce -> ce { ceValue = value, ceAccessed = current }) mce
    entries' = M.insert key ce' entries
  in
    (entries', ())

memCacheDelete :: (Ord k) => MemCache k v -> Cache k v -> k -> IO ()
memCacheDelete cache _base key =
  atomicModifyIORef' (mcEntries cache) (memCacheDelete' key)

memCacheDelete' :: (Ord k) => k -> M.Map k (CacheEntry k v) -> (M.Map k (CacheEntry k v), ())
memCacheDelete' key entries = (M.delete key entries, ())

memCacheFree :: (Ord k) => MemCache k v -> Cache k v -> Int -> IO ()
memCacheFree cache base free = case cpSize $ caPolicy base of
  Nothing -> return ()
  (Just capacity) -> do
    entries <- memCacheEntries cache base
    let removals = entries + free - capacity
    when (removals > 0) $ cacheLog base ("Freeing " ++ show removals ++ " entries")
    atomicModifyIORef' (mcEntries cache) (memCacheFree' cacheRemovalLRU removals)

memCacheFree' :: (Ord k) => (CacheEntry k v -> CacheEntry k v -> Ordering) -> Int ->  M.Map k (CacheEntry k v) -> (M.Map k (CacheEntry k v), ())
memCacheFree' chooser n entries
  | n <= 0 = (entries, ())
  | otherwise = let
      mremove = M.foldr (\ce -> \cm -> maybe (Just ce) (\cm' -> if chooser ce cm' == LT then (Just ce) else cm) cm) Nothing entries
    in case mremove of
      Nothing -> (entries, ())
      (Just remove) -> memCacheFree' chooser (n - 1) (M.delete (ceKey remove) entries)

memCacheExpire :: (Ord k) => MemCache k v -> Cache k v -> IO ()
memCacheExpire cache base = case cpRetain $ caPolicy base of
  Nothing -> return ()
  (Just expiry) -> case cpScan $ caPolicy base of
    Nothing -> memCacheExpiry' cache expiry
    (Just scan) -> do
      current <- getCurrentTime
      lastScan <- readIORef (mcLastScan cache)
      if addUTCTime scan lastScan < current then do
        cacheLog base "Scanning for expiry"
        memCacheExpiry' cache expiry
      else
        return ()

memCacheExpiry' :: (Ord k) => MemCache k v -> NominalDiffTime -> IO ()
memCacheExpiry' cache expiry = do
  current <- getCurrentTime
  atomicModifyIORef' (mcEntries cache) (memCacheExpiry'' (addUTCTime (negate expiry) current))
  writeIORef (mcLastScan cache) current

memCacheExpiry'' :: (Ord k) => UTCTime -> M.Map k (CacheEntry k v) -> (M.Map k (CacheEntry k v), ())
memCacheExpiry'' cutoff entries = (M.filter (\ce -> ceAccessed ce >= cutoff) entries, ())

-- | Create a default memory cache with a specific size
--   The default cache has a least recently used removal policy
newMemCache' :: (Ord k, Real t) => String -> Maybe CacheLogger -> Maybe Int -> Maybe t -> IO (Cache k v)
newMemCache' ident mlogger msize mexpiry = do
  current <- getCurrentTime
  entries <- newIORef M.empty
  scan <- newIORef current
  let memCache = MemCache {
        mcEntries = entries
      , mcLastScan = scan
    }
  return $ Cache {
      caID = ident
    , caPolicy = CachePolicy {
        cpSize = msize
        , cpRetain = toRetainTime <$> mexpiry
        , cpScan = toScanTime <$> mexpiry
      }
    , caEntries = memCacheEntries memCache
    , caLookup = memCacheLookup memCache
    , caPut = memCachePut memCache
    , caDelete = memCacheDelete memCache
    , caExpire = memCacheExpire memCache
    , caLog = maybe nullLog id mlogger
    }

-- | A memory cache with a size and no expiry time
newMemCache :: (Ord k) => String -> Maybe CacheLogger -> Int -> IO (Cache k v)
newMemCache ident mlogger size = newMemCache' ident mlogger (Just size) (Nothing :: Maybe Float)

-- | A memory cache with a size and an expiry time
newMemCacheWithExpiry :: (Ord k, Real t) => String -> Maybe CacheLogger -> Int -> t -> IO (Cache k v)
newMemCacheWithExpiry ident mlogger size expiry = newMemCache' ident mlogger (Just size) (Just expiry)

-- | A file-based cache.
--   Cache entries are stored as JSON files, with the file system providing information about creation and access time
data (IsNamer k, Ord k, ToJSON v, FromJSON v) => FileCache k v = FileCache {
    fcRoot :: FilePath -- ^ The root location of a file
  , fcLastScan :: IORef UTCTime -- ^ The time of the last scan
}

fileForKey :: (IsNamer k) => FilePath -> k -> FilePath
fileForKey root key = let
    filename = toName key
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

fileCacheEntries :: FileCache k v -> Cache k v -> IO Int
fileCacheEntries cache _base = foldDirectory (fcRoot cache) (\c -> \_f -> c + 1) 0

fileCacheLookup :: (IsNamer k, Ord k, FromJSON v) => FileCache k v -> Cache k v  -> k -> IO (Maybe v)
fileCacheLookup cache base key = do
  let path = fileForKey (fcRoot cache) key
  fileCacheExpire False cache base
  exists <- doesFileExist path
  if not exists then
    return Nothing
  else do
    value <- readCacheFile path
    return $ Just value

fileCachePut :: (IsNamer k, Ord k, ToJSON v) => FileCache k v -> Cache k v  -> k -> v -> IO ()
fileCachePut cache base key value = do
  let path = fileForKey (fcRoot cache) key
  let dir = takeDirectory path
  fileCacheExpire False cache base
  fileCacheFree cache base 1
  createDirectoryIfMissing True dir
  writeCacheFile path value

fileCacheDelete :: (IsNamer k, Ord k) => FileCache k v -> Cache k v  -> k -> IO ()
fileCacheDelete cache base key = do
  let path = fileForKey (fcRoot cache) key
  fileCacheExpire False cache base
  exists <- doesFileExist path
  when exists (removeFile path)

fileCacheFree :: (Ord k) => FileCache k v -> Cache k v -> Int -> IO ()
fileCacheFree cache base free = case cpSize $ caPolicy base of
  Nothing -> return ()
  (Just capacity) -> do
    entries <- fileCacheEntries cache base
    let removals = entries + free - capacity
    when (removals > 0) $ cacheLog base ("Freeing " ++ show removals ++ " entries")
    fileCacheFree' (fcRoot cache) removals

fileCacheFree' :: FilePath -> Int ->  IO ()
fileCacheFree' root n
  | n <= 0 = return ()
  | otherwise = do
      mremove <- foldDirectoryIO root fileCacheFree'' Nothing
      case mremove of
        Nothing -> return ()
        (Just (remove, _ac)) -> removeFile remove

fileCacheFree'' :: Maybe (FilePath, UTCTime) -> FilePath -> IO (Maybe (FilePath, UTCTime))
fileCacheFree'' mc f = do
  ac <- getAccessTime f
  return $ case mc of
    Nothing -> Just (f, ac)
    Just (_f, ac') -> if ac < ac' then Just (f, ac) else mc

fileCacheExpire :: (Ord k) => Bool -> FileCache k v -> Cache k v -> IO ()
fileCacheExpire force cache base = case cpRetain $ caPolicy base of
  Nothing -> return ()
  (Just expiry) -> case cpScan $ caPolicy base of
    Nothing -> fileCacheExpiry' cache expiry
    (Just scan) -> do
      current <- getCurrentTime
      lastScan <- readIORef (fcLastScan cache)
      if force || addUTCTime scan lastScan < current then do
        cacheLog base "Scanning for expiry"
        fileCacheExpiry' cache expiry
      else
        return ()

fileCacheExpiry' :: FileCache k v -> NominalDiffTime -> IO ()
fileCacheExpiry' cache expiry = do
  current <- getCurrentTime
  let expire = addUTCTime (negate expiry) current
  scanDirectory
    (\f -> do
      access <- getAccessTime f
      when (access < expire) (removeFile f)
    )
    (\_d -> return ())
    (fcRoot cache)
  writeIORef (fcLastScan cache) current

-- | Create a default memory cache with a specific size
--   The default cache has a least recently used removal policy
newFileCache' :: (IsNamer k, Ord k, ToJSON v, FromJSON v, Real t) => String -> Maybe CacheLogger -> FilePath -> Maybe Int -> Maybe t -> IO (Cache k v)
newFileCache' ident mlogger root msize mexpiry = do
  current <- getCurrentTime
  lastScan <- newIORef current
  let fileCache = FileCache {
        fcRoot = root
      , fcLastScan = lastScan
    }
  let cache = Cache {
      caID = ident
    , caPolicy = CachePolicy {
      cpSize = msize
      , cpRetain = toRetainTime <$> mexpiry
      , cpScan = toScanTime <$> mexpiry
      }
    , caEntries = fileCacheEntries fileCache
    , caLookup = fileCacheLookup fileCache
    , caPut = fileCachePut fileCache
    , caDelete = fileCacheDelete fileCache
    , caExpire = fileCacheExpire False fileCache
    , caLog = maybe nullLog id mlogger
    }
  createDirectoryIfMissing True root
  putStrLn "Made directory"
  cacheLog cache ("File directory " ++ root)
  fileCacheExpire True fileCache cache
  return cache

-- | A new file cache with a root on the filesystem and an expiry time
-- | File roots may have environment/tmp variables in them, such as $HOME, expended by `expandPath`
newFileCache :: (IsNamer k, Ord k, ToJSON v, FromJSON v, Real t) => String -> Maybe CacheLogger -> FilePath -> t -> IO (Cache k v)
newFileCache ident mlogger root expiry = do
  root' <- expandPath root
  newFileCache' ident mlogger root' Nothing (Just expiry)

-- Composite caches are composed out of a primary (faster, smaller size) and seconary (slower, larger size) pair

compositeCacheEntries :: (Ord k) => Cache k v -> Cache k v -> Cache k v -> IO Int
compositeCacheEntries primary _secodnary _base = cacheEntries primary

compositeCacheLookup :: (Ord k) => Cache k v -> Cache k v -> Cache k v -> k -> IO (Maybe v)
compositeCacheLookup primary secondary _base key = do
  mresult <- cacheLookup primary key
  case mresult of
    Nothing -> do
      mresult' <- cacheLookup secondary key
      case mresult' of
        Nothing -> return Nothing
        (Just result') -> do
          cachePut primary key result'
          return mresult'
    (Just _) -> return mresult

compositeCachePut :: (Ord k) => Cache k v -> Cache k v -> Cache k v  -> k -> v -> IO ()
compositeCachePut primary secondary _base key value = do
  cachePut primary key value
  cachePut secondary key value

compositeCacheDelete :: (Ord k) => Cache k v -> Cache k v -> Cache k v -> k -> IO ()
compositeCacheDelete primary secondary _base key = do
  cacheDelete primary key
  cacheDelete secondary key

compositeCacheExpire :: (Ord k) => Cache k v -> Cache k v -> Cache k v -> IO ()
compositeCacheExpire primary secondary _base = do
  cacheExpire primary
  cacheExpire secondary

-- | Create a composite cache from two caches
newCompositeCache :: (Ord k) => String -> Cache k v -> Cache k v -> Cache k v
newCompositeCache ident primary secondary = Cache {
      caID = ident
    , caPolicy = CachePolicy {
          cpSize = Nothing
        , cpRetain = Nothing
        , cpScan = Nothing
        }
    , caEntries = compositeCacheEntries primary secondary
    , caLookup = compositeCacheLookup primary secondary
    , caPut = compositeCachePut primary secondary
    , caDelete = compositeCacheDelete primary secondary
    , caExpire = compositeCacheExpire primary secondary
    , caLog = caLog primary
  }
