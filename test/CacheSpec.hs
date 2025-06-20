{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module CacheSpec(testCache) where

import Test.HUnit
import Data.Cache
import Control.Concurrent (threadDelay)
import System.Directory
import System.FilePath
import Data.Util
import TestUtils

testCache :: Test
testCache = TestList [
      TestLabel "MemCache" testMemCache
    , TestLabel "FileCache" testFileCache
    , TestLabel "CompositeCache" testCompositeCache
  ]

testMemCache = TestList [
    TestLabel "testMemCacheCreate" testMemCacheCreate
  , TestLabel "testMemCachePut" testMemCachePut
  , TestLabel "testMemCacheExpire" testMemCacheExpire
  ]

testMemCacheCreate = TestList [
    testMemCacheCreate1
  ]

testMemCacheCreate1 = TestCase (do
 let cache = defaultMemCache 1 :: MemCache String String
 entries <- cacheEntries cache
 assertEqual "MemCache create 1" 0 entries
 )

testMemCachePut = TestList [
    testMemCachePut1, testMemCachePut2, testMemCachePut3, testMemCachePut4, testMemCachePut5
  ]

testMemCachePut1 = TestCase (do
  let cache = defaultMemCache 2 :: MemCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "MemCache put 1 1" 1 entries1
  (value1, cache2) <- cacheLookup "Key1" cache1
  assertEqual "MemCache put 1 2" (Just "Value1") value1
  (value2, _cache3) <- cacheLookup "Key2" cache2
  assertEqual "MemCache put 1 3" Nothing value2
  )

testMemCachePut2 = TestCase (do
  let cache = defaultMemCache 2 :: MemCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "MemCache put 2 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  entries2 <- cacheEntries cache2
  assertEqual "MemCache put 2 2" 2 entries2
  (value1, cache3) <- cacheLookup "Key1" cache2
  assertEqual "MemCache put 1 2" (Just "Value1") value1
  (value2, _cache4) <- cacheLookup "Key2" cache3
  assertEqual "MemCache put 1 3" (Just "Value2") value2
  )

testMemCachePut3 = TestCase (do
  let cache = defaultMemCache 2 :: MemCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "MemCache put 3 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  entries2 <- cacheEntries cache2
  assertEqual "MemCache put 3 2" 2 entries2
  cache3 <- cachePut "Key3" "Value3" cache2
  entries3 <- cacheEntries cache3
  assertEqual "MemCache put 3 3" 2 entries3
  )

testMemCachePut4 = TestCase (do
  let cache = defaultMemCache 2 :: MemCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  threadDelay 10
  entries1 <- cacheEntries cache1
  assertEqual "MemCache put 4 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  threadDelay 10
  entries2 <- cacheEntries cache2
  assertEqual "MemCache put 4 2" 2 entries2
  cache3 <- cachePut "Key3" "Value3" cache2
  entries3 <- cacheEntries cache3
  assertEqual "MemCache put 4 3" 2 entries3
  (value1, cache4) <- cacheLookup "Key1" cache3
  assertEqual "MemCache put 4 4" Nothing value1
  (value2, cache5) <- cacheLookup "Key2" cache4
  assertEqual "MemCache put 4 5" (Just "Value2") value2
  (value3, _cache6) <- cacheLookup "Key3" cache5
  assertEqual "MemCache put 4 6" (Just "Value3") value3
  )

testMemCachePut5 = TestCase (do
  let cache = defaultMemCache 2 :: MemCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  threadDelay 10
  entries1 <- cacheEntries cache1
  assertEqual "MemCache put 4 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  threadDelay 10
  (_, cache3) <- cacheLookup "Key1" cache2
  entries3 <- cacheEntries cache3
  assertEqual "MemCache put 4 2" 2 entries3
  cache4 <- cachePut "Key3" "Value3" cache3
  entries4 <- cacheEntries cache4
  assertEqual "MemCache put 4 3" 2 entries4
  (value1, cache5) <- cacheLookup "Key1" cache4
  assertEqual "MemCache put 4 4" (Just "Value1") value1
  (value2, cache6) <- cacheLookup "Key2" cache5
  assertEqual "MemCache put 4 5" Nothing value2
  (value3, _cache7) <- cacheLookup "Key3" cache6
  assertEqual "MemCache put 4 6" (Just "Value3") value3
  )

testMemCacheExpire = TestList [
    testMemCacheExpire1, testMemCacheExpire2
  ]

testMemCacheExpire1 = TestCase (do
  let cache = defaultMemCacheWithExpiry 2 0.2 :: MemCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "MemCache expire 1 1" 1 entries1
  threadDelay 300000
  cache2 <- cacheExpire cache1
  entries2 <- cacheEntries cache2
  assertEqual "MemCache expire 1 2" 0 entries2
  )

testMemCacheExpire2 = TestCase (do
  let cache = defaultMemCacheWithExpiry 2 0.2 :: MemCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  threadDelay 150000
  cache2 <- cachePut "Key2" "Value2" cache1
  cache3 <- cacheExpire cache2
  entries3 <- cacheEntries cache3
  assertEqual "MemCache expire 1 1" 2 entries3
  threadDelay 150000
  cache4 <- cacheExpire cache3
  entries4 <- cacheEntries cache4
  assertEqual "MemCache expire 1 2" 1 entries4
  )

testFileCache = TestList [
    TestLabel "testFileCacheCreate" testFileCacheCreate
  , TestLabel "testFileCachePut" testFileCachePut
  , TestLabel "testFileCacheExpire" testFileCacheExpire
  ]

testFileCacheCreate = TestList [
    testFileCacheCreate1
  ]

testFileCacheCreate1 = TestCase (do
 testdir <- openTestDir
 let cache = defaultFileCache testdir 1 :: FileCache String String
 entries <- cacheEntries cache
 assertEqual "FileCache create 1" 0 entries
 closeTestDir testdir
 )

testFileCachePut = TestList [
    testFileCachePut1, testFileCachePut2, testFileCachePut3, testFileCachePut4, testFileCachePut5
  ]

testFileCachePut1 = TestCase (do
  testdir <- openTestDir
  let cache = defaultFileCache testdir 1 :: FileCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "FileCache put 1 1" 1 entries1
  (value1, cache2) <- cacheLookup "Key1" cache1
  assertEqual "FileCache put 1 2" (Just "Value1") value1
  (value2, _cache3) <- cacheLookup "Key2" cache2
  assertEqual "FileCache put 1 3" Nothing value2
  closeTestDir testdir
  )

testFileCachePut2 = TestCase (do
  testdir <- openTestDir
  let cache = defaultFileCache testdir 1 :: FileCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "FileCache put 2 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  entries2 <- cacheEntries cache2
  assertEqual "FileCache put 2 2" 2 entries2
  (value1, cache3) <- cacheLookup "Key1" cache2
  assertEqual "FileCache put 1 2" (Just "Value1") value1
  (value2, _cache4) <- cacheLookup "Key2" cache3
  assertEqual "FileCache put 1 3" (Just "Value2") value2
  closeTestDir testdir
  )

testFileCachePut3 = TestCase (do
  testdir <- openTestDir
  let cache = defaultFileCache testdir 1 :: FileCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "FileCache put 3 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  entries2 <- cacheEntries cache2
  assertEqual "FileCache put 3 2" 2 entries2
  cache3 <- cachePut "Key3" "Value3" cache2
  entries3 <- cacheEntries cache3
  assertEqual "FileCache put 3 3" 3 entries3
  closeTestDir testdir
  )

testFileCachePut4 = TestCase (do
  testdir <- openTestDir
  let cache = defaultFileCache testdir 1 :: FileCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  threadDelay 10
  entries1 <- cacheEntries cache1
  assertEqual "FileCache put 4 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  threadDelay 10
  entries2 <- cacheEntries cache2
  assertEqual "FileCache put 4 2" 2 entries2
  cache3 <- cachePut "Key3" "Value3" cache2
  entries3 <- cacheEntries cache3
  assertEqual "FileCache put 4 3" 3 entries3
  (value1, cache4) <- cacheLookup "Key1" cache3
  assertEqual "FileCache put 4 4" (Just "Value1") value1
  (value2, cache5) <- cacheLookup "Key2" cache4
  assertEqual "FileCache put 4 5" (Just "Value2") value2
  (value3, _cache6) <- cacheLookup "Key3" cache5
  assertEqual "FileCache put 4 6" (Just "Value3") value3
  closeTestDir testdir
  )

testFileCachePut5 = TestCase (do
  testdir <- openTestDir
  let cache = defaultFileCache testdir 1 :: FileCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  threadDelay 10
  entries1 <- cacheEntries cache1
  assertEqual "FileCache put 4 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  threadDelay 10
  (_, cache3) <- cacheLookup "Key1" cache2
  entries3 <- cacheEntries cache3
  assertEqual "FileCache put 4 2" 2 entries3
  cache4 <- cachePut "Key3" "Value3" cache3
  entries4 <- cacheEntries cache4
  assertEqual "FileCache put 4 3" 3 entries4
  (value1, cache5) <- cacheLookup "Key1" cache4
  assertEqual "FileCache put 4 4" (Just "Value1") value1
  (value2, cache6) <- cacheLookup "Key2" cache5
  assertEqual "FileCache put 4 5" (Just "Value2") value2
  (value3, _cache7) <- cacheLookup "Key3" cache6
  assertEqual "FileCache put 4 6" (Just "Value3") value3
  closeTestDir testdir
  )

testFileCacheExpire = TestList [
    testFileCacheExpire1, testFileCacheExpire2
  ]

testFileCacheExpire1 = TestCase (do
  testdir <- openTestDir
  let cache = defaultFileCache testdir 0.2 :: FileCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "FileCache expire 1 1" 1 entries1
  threadDelay 300000
  cache2 <- cacheExpire cache1
  entries2 <- cacheEntries cache2
  assertEqual "FileCache expire 1 2" 0 entries2
  closeTestDir testdir
  )

testFileCacheExpire2 = TestCase (do
  testdir <- openTestDir
  let cache = defaultFileCache testdir 0.2 :: FileCache String String
  cache1 <- cachePut "Key1" "Value1" cache
  threadDelay 150000
  cache2 <- cachePut "Key2" "Value2" cache1
  cache3 <- cacheExpire cache2
  entries3 <- cacheEntries cache3
  assertEqual "FileCache expire 1 1" 2 entries3
  threadDelay 150000
  cache4 <- cacheExpire cache3
  entries4 <- cacheEntries cache4
  assertEqual "FileCache expire 1 2" 1 entries4
  closeTestDir testdir
  )

testCompositeCache = TestList [
    TestLabel "testCompositeCacheCreate" testCompositeCacheCreate
  , TestLabel "testCompositeCachePut" testCompositeCachePut
  , TestLabel "testCompositeCacheExpire" testCompositeCacheExpire
  ]

testCompositeCacheCreate = TestList [
    testCompositeCacheCreate1
  ]

testCompositeCacheCreate1 = TestCase (do
  testdir <- openTestDir
  let primary =  defaultMemCache 2 :: MemCache String String
  let secondary = defaultFileCache testdir 0.2 :: FileCache String String
  let cache = compositeCache primary secondary
  entries <- cacheEntries cache
  assertEqual "CompositeCache create 1" 0 entries
  closeTestDir testdir
  )

testCompositeCachePut = TestList [
    testCompositeCachePut1, testCompositeCachePut2, testCompositeCachePut3, testCompositeCachePut4, testCompositeCachePut5
  ]

testCompositeCachePut1 = TestCase (do
  testdir <- openTestDir
  let primary =  defaultMemCache 2 :: MemCache String String
  let secondary = defaultFileCache testdir 0.2 :: FileCache String String
  let cache = compositeCache primary secondary
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "CompositeCache put 1 1" 1 entries1
  (value1, cache2) <- cacheLookup "Key1" cache1
  assertEqual "CompositeCache put 1 2" (Just "Value1") value1
  (value2, _cache3) <- cacheLookup "Key2" cache2
  assertEqual "CompositeCache put 1 3" Nothing value2
  closeTestDir testdir
  )

testCompositeCachePut2 = TestCase (do
  testdir <- openTestDir
  let primary =  defaultMemCache 2 :: MemCache String String
  let secondary = defaultFileCache testdir 0.2 :: FileCache String String
  let cache = compositeCache primary secondary
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "CompositeCache put 2 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  entries2 <- cacheEntries cache2
  assertEqual "CompositeCache put 2 2" 2 entries2
  (value1, cache3) <- cacheLookup "Key1" cache2
  assertEqual "CompositeCache put 1 2" (Just "Value1") value1
  (value2, _cache4) <- cacheLookup "Key2" cache3
  assertEqual "CompositeCache put 1 3" (Just "Value2") value2
  closeTestDir testdir
  )

testCompositeCachePut3 = TestCase (do
  testdir <- openTestDir
  let primary =  defaultMemCache 2 :: MemCache String String
  let secondary = defaultFileCache testdir 0.2 :: FileCache String String
  let cache = compositeCache primary secondary
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "CompositeCache put 3 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  entries2 <- cacheEntries cache2
  assertEqual "CompositeCache put 3 2" 2 entries2
  cache3 <- cachePut "Key3" "Value3" cache2
  entries3 <- cacheEntries cache3
  assertEqual "CompositeCache put 3 3" 2 entries3
  closeTestDir testdir
  )

testCompositeCachePut4 = TestCase (do
  testdir <- openTestDir
  let primary =  defaultMemCache 2 :: MemCache String String
  let secondary = defaultFileCache testdir 0.2 :: FileCache String String
  let cache = compositeCache primary secondary
  cache1 <- cachePut "Key1" "Value1" cache
  threadDelay 10
  entries1 <- cacheEntries cache1
  assertEqual "CompositeCache put 4 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  threadDelay 10
  entries2 <- cacheEntries cache2
  assertEqual "CompositeCache put 4 2" 2 entries2
  cache3 <- cachePut "Key3" "Value3" cache2
  entries3 <- cacheEntries cache3
  assertEqual "CompositeCache put 4 3" 2 entries3
  (value1, cache4) <- cacheLookup "Key1" cache3
  assertEqual "CompositeCache put 4 4" (Just "Value1") value1
  (value2, cache5) <- cacheLookup "Key2" cache4
  assertEqual "CompositeCache put 4 5" (Just "Value2") value2
  (value3, _cache6) <- cacheLookup "Key3" cache5
  assertEqual "CompositeCache put 4 6" (Just "Value3") value3
  closeTestDir testdir
  )

testCompositeCachePut5 = TestCase (do
  testdir <- openTestDir
  let primary =  defaultMemCache 2 :: MemCache String String
  let secondary = defaultFileCache testdir 0.2 :: FileCache String String
  let cache = compositeCache primary secondary
  cache1 <- cachePut "Key1" "Value1" cache
  threadDelay 10
  entries1 <- cacheEntries cache1
  assertEqual "CompositeCache put 4 1" 1 entries1
  cache2 <- cachePut "Key2" "Value2" cache1
  threadDelay 10
  (_, cache3) <- cacheLookup "Key1" cache2
  entries3 <- cacheEntries cache3
  assertEqual "CompositeCache put 4 2" 2 entries3
  cache4 <- cachePut "Key3" "Value3" cache3
  entries4 <- cacheEntries cache4
  assertEqual "CompositeCache put 4 3" 2 entries4
  (value1, cache5) <- cacheLookup "Key1" cache4
  assertEqual "CompositeCache put 4 4" (Just "Value1") value1
  (value2, cache6) <- cacheLookup "Key2" cache5
  assertEqual "CompositeCache put 4 5" (Just "Value2") value2
  (value3, _cache7) <- cacheLookup "Key3" cache6
  assertEqual "CompositeCache put 4 6" (Just "Value3") value3
  closeTestDir testdir
  )

testCompositeCacheExpire = TestList [
    testCompositeCacheExpire1, testCompositeCacheExpire2
  ]

testCompositeCacheExpire1 = TestCase (do
  testdir <- openTestDir
  let primary =  defaultMemCacheWithExpiry 2 0.2 :: MemCache String String
  let secondary = defaultFileCache testdir 0.2 :: FileCache String String
  let cache = compositeCache primary secondary
  cache1 <- cachePut "Key1" "Value1" cache
  entries1 <- cacheEntries cache1
  assertEqual "CompositeCache expire 1 1" 1 entries1
  threadDelay 300000
  cache2 <- cacheExpire cache1
  entries2 <- cacheEntries cache2
  assertEqual "CompositeCache expire 1 2" 0 entries2
  closeTestDir testdir
  )

testCompositeCacheExpire2 = TestCase (do
  testdir <- openTestDir
  let primary =  defaultMemCacheWithExpiry 2 0.2 :: MemCache String String
  let secondary = defaultFileCache testdir 0.2 :: FileCache String String
  let cache = compositeCache primary secondary
  cache1 <- cachePut "Key1" "Value1" cache
  threadDelay 150000
  cache2 <- cachePut "Key2" "Value2" cache1
  cache3 <- cacheExpire cache2
  entries3 <- cacheEntries cache3
  assertEqual "CompositeCache expire 1 1" 2 entries3
  threadDelay 150000
  cache4 <- cacheExpire cache3
  entries4 <- cacheEntries cache4
  assertEqual "CompositeCache expire 1 2" 1 entries4
  closeTestDir testdir
  )
