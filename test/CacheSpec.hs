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
 cache <- newMemCache "test" Nothing 1 :: IO (Cache String String)
 entries <- cacheEntries cache
 assertEqual "MemCache create 1" 0 entries
 )

testMemCachePut = TestList [
    testMemCachePut1, testMemCachePut2, testMemCachePut3, testMemCachePut4, testMemCachePut5
  ]

testMemCachePut1 = TestCase (do
  cache <- newMemCache "test" Nothing 2 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "MemCache put 1 1" 1 entries1
  value1 <- cacheLookup cache "Key1"
  assertEqual "MemCache put 1 2" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "MemCache put 1 3" Nothing value2
  )

testMemCachePut2 = TestCase (do
  cache <- newMemCache "test" Nothing 2 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "MemCache put 2 1" 1 entries1
  cachePut cache "Key2" "Value2"
  entries2 <- cacheEntries cache
  assertEqual "MemCache put 2 2" 2 entries2
  value1 <- cacheLookup cache "Key1"
  assertEqual "MemCache put 1 2" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "MemCache put 1 3" (Just "Value2") value2
  )

testMemCachePut3 = TestCase (do
  cache <- newMemCache "test" Nothing 2 :: IO (Cache String String)
  cachePut cache "Key1" "Value1" 
  entries1 <- cacheEntries cache
  assertEqual "MemCache put 3 1" 1 entries1
  cachePut cache "Key2" "Value2"
  entries2 <- cacheEntries cache
  assertEqual "MemCache put 3 2" 2 entries2
  cachePut cache "Key3" "Value3"
  entries3 <- cacheEntries cache
  assertEqual "MemCache put 3 3" 2 entries3
  )

testMemCachePut4 = TestCase (do
  cache <- newMemCache "test" Nothing 2 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  threadDelay 10
  entries1 <- cacheEntries cache
  assertEqual "MemCache put 4 1" 1 entries1
  cachePut cache "Key2" "Value2"
  threadDelay 10
  entries2 <- cacheEntries cache
  assertEqual "MemCache put 4 2" 2 entries2
  cachePut cache "Key3" "Value3"
  entries3 <- cacheEntries cache
  assertEqual "MemCache put 4 3" 2 entries3
  value1 <- cacheLookup cache "Key1"
  assertEqual "MemCache put 4 4" Nothing value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "MemCache put 4 5" (Just "Value2") value2
  value3 <- cacheLookup cache "Key3"
  assertEqual "MemCache put 4 6" (Just "Value3") value3
  )

testMemCachePut5 = TestCase (do
  cache <- newMemCache "test" Nothing 2 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  threadDelay 10
  entries1 <- cacheEntries cache
  assertEqual "MemCache put 4 1" 1 entries1
  cachePut cache "Key2" "Value2"
  threadDelay 10
  _value <- cacheLookup cache "Key1"
  entries3 <- cacheEntries cache
  assertEqual "MemCache put 4 2" 2 entries3
  cachePut cache "Key3" "Value3"
  entries4 <- cacheEntries cache
  assertEqual "MemCache put 4 3" 2 entries4
  value1 <- cacheLookup cache "Key1"
  assertEqual "MemCache put 4 4" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "MemCache put 4 5" Nothing value2
  value3 <- cacheLookup cache "Key3"
  assertEqual "MemCache put 4 6" (Just "Value3") value3
  )

testMemCacheExpire = TestList [
    testMemCacheExpire1, testMemCacheExpire2
  ]

testMemCacheExpire1 = TestCase (do
  cache <- newMemCacheWithExpiry "test" Nothing 2 0.2 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "MemCache expire 1 1" 1 entries1
  threadDelay 300000
  cacheExpire cache
  entries2 <- cacheEntries cache
  assertEqual "MemCache expire 1 2" 0 entries2
  )

testMemCacheExpire2 = TestCase (do
  cache <- newMemCacheWithExpiry "test" Nothing 2 0.2 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  threadDelay 150000
  cachePut cache "Key2" "Value2"
  cacheExpire cache
  entries3 <- cacheEntries cache
  assertEqual "MemCache expire 1 1" 2 entries3
  threadDelay 150000
  cacheExpire cache
  entries4 <- cacheEntries cache
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
 cache <- newFileCache "test1" Nothing testdir 1 :: IO (Cache String String)
 entries <- cacheEntries cache
 assertEqual "FileCache create 1" 0 entries
 closeTestDir testdir
 )

testFileCachePut = TestList [
    testFileCachePut1, testFileCachePut2, testFileCachePut3, testFileCachePut4, testFileCachePut5
  ]

testFileCachePut1 = TestCase (do
  testdir <- openTestDir
  cache <- newFileCache "test1" Nothing testdir 1 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "FileCache put 1 1" 1 entries1
  value1 <- cacheLookup cache "Key1"
  assertEqual "FileCache put 1 2" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "FileCache put 1 3" Nothing value2
  closeTestDir testdir
  )

testFileCachePut2 = TestCase (do
  testdir <- openTestDir
  cache <- newFileCache "test1" Nothing testdir 1 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "FileCache put 2 1" 1 entries1
  cachePut cache "Key2" "Value2"
  entries2 <- cacheEntries cache
  assertEqual "FileCache put 2 2" 2 entries2
  value1 <- cacheLookup cache "Key1"
  assertEqual "FileCache put 1 2" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "FileCache put 1 3" (Just "Value2") value2
  closeTestDir testdir
  )

testFileCachePut3 = TestCase (do
  testdir <- openTestDir
  cache <- newFileCache "test1" Nothing testdir 1 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "FileCache put 3 1" 1 entries1
  cachePut cache "Key2" "Value2"
  entries2 <- cacheEntries cache
  assertEqual "FileCache put 3 2" 2 entries2
  cachePut cache "Key3" "Value3"
  entries3 <- cacheEntries cache
  assertEqual "FileCache put 3 3" 3 entries3
  closeTestDir testdir
  )

testFileCachePut4 = TestCase (do
  testdir <- openTestDir
  cache <- newFileCache "test1" Nothing testdir 1 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  threadDelay 10
  entries1 <- cacheEntries cache
  assertEqual "FileCache put 4 1" 1 entries1
  cachePut cache "Key2" "Value2"
  threadDelay 10
  entries2 <- cacheEntries cache
  assertEqual "FileCache put 4 2" 2 entries2
  cachePut cache "Key3" "Value3"
  entries3 <- cacheEntries cache
  assertEqual "FileCache put 4 3" 3 entries3
  value1 <- cacheLookup cache "Key1"
  assertEqual "FileCache put 4 4" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "FileCache put 4 5" (Just "Value2") value2
  value3 <- cacheLookup cache "Key3"
  assertEqual "FileCache put 4 6" (Just "Value3") value3
  closeTestDir testdir
  )

testFileCachePut5 = TestCase (do
  testdir <- openTestDir
  cache <- newFileCache "test1" Nothing testdir 1 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  threadDelay 10
  entries1 <- cacheEntries cache
  assertEqual "FileCache put 4 1" 1 entries1
  cachePut cache "Key2" "Value2"
  threadDelay 10
  _value <- cacheLookup cache "Key1"
  entries3 <- cacheEntries cache
  assertEqual "FileCache put 4 2" 2 entries3
  cachePut cache "Key3" "Value3"
  entries4 <- cacheEntries cache
  assertEqual "FileCache put 4 3" 3 entries4
  value1 <- cacheLookup cache "Key1"
  assertEqual "FileCache put 4 4" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "FileCache put 4 5" (Just "Value2") value2
  value3 <- cacheLookup cache "Key3"
  assertEqual "FileCache put 4 6" (Just "Value3") value3
  closeTestDir testdir
  )

testFileCacheExpire = TestList [
    testFileCacheExpire1, testFileCacheExpire2
  ]

testFileCacheExpire1 = TestCase (do
  testdir <- openTestDir
  cache <- newFileCache "test1" Nothing testdir 0.2 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "FileCache expire 1 1" 1 entries1
  threadDelay 300000
  cacheExpire cache
  entries2 <- cacheEntries cache
  assertEqual "FileCache expire 1 2" 0 entries2
  closeTestDir testdir
  )

testFileCacheExpire2 = TestCase (do
  testdir <- openTestDir
  cache <- newFileCache "test1" Nothing testdir 0.2 :: IO (Cache String String)
  cachePut cache "Key1" "Value1"
  threadDelay 150000
  cachePut cache "Key2" "Value2"
  cacheExpire cache
  entries3 <- cacheEntries cache
  assertEqual "FileCache expire 1 1" 2 entries3
  threadDelay 150000
  cacheExpire cache
  entries4 <- cacheEntries cache
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
  primary <- newMemCache "test-1" Nothing 2 :: IO (Cache String String)
  secondary <- newFileCache "test-2" Nothing testdir 0.2 :: IO (Cache String String)
  let cache = newCompositeCache "test" primary secondary
  entries <- cacheEntries cache
  assertEqual "CompositeCache create 1" 0 entries
  closeTestDir testdir
  )

testCompositeCachePut = TestList [
    testCompositeCachePut1, testCompositeCachePut2, testCompositeCachePut3, testCompositeCachePut4, testCompositeCachePut5
  ]

testCompositeCachePut1 = TestCase (do
  testdir <- openTestDir
  primary <- newMemCache "test-1" Nothing 2 :: IO (Cache String String)
  secondary <- newFileCache "test-2" Nothing testdir 0.2 :: IO (Cache String String)
  let cache = newCompositeCache "test" primary secondary
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "CompositeCache put 1 1" 1 entries1
  value1 <- cacheLookup cache "Key1"
  assertEqual "CompositeCache put 1 2" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "CompositeCache put 1 3" Nothing value2
  closeTestDir testdir
  )

testCompositeCachePut2 = TestCase (do
  testdir <- openTestDir
  primary <- newMemCache "test-1" Nothing 2 :: IO (Cache String String)
  secondary <- newFileCache "test-2" Nothing testdir 0.2 :: IO (Cache String String)
  let cache = newCompositeCache "test" primary secondary
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "CompositeCache put 2 1" 1 entries1
  cachePut cache "Key2" "Value2"
  entries2 <- cacheEntries cache
  assertEqual "CompositeCache put 2 2" 2 entries2
  value1 <- cacheLookup cache "Key1"
  assertEqual "CompositeCache put 1 2" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "CompositeCache put 1 3" (Just "Value2") value2
  closeTestDir testdir
  )

testCompositeCachePut3 = TestCase (do
  testdir <- openTestDir
  primary <- newMemCache "test-1" Nothing 2 :: IO (Cache String String)
  secondary <- newFileCache "test-2" Nothing testdir 0.2 :: IO (Cache String String)
  let cache = newCompositeCache "test" primary secondary
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "CompositeCache put 3 1" 1 entries1
  cachePut cache "Key2" "Value2"
  entries2 <- cacheEntries cache
  assertEqual "CompositeCache put 3 2" 2 entries2
  cachePut cache "Key3" "Value3"
  entries3 <- cacheEntries cache
  assertEqual "CompositeCache put 3 3" 2 entries3
  closeTestDir testdir
  )

testCompositeCachePut4 = TestCase (do
  testdir <- openTestDir
  primary <- newMemCache "test-1" Nothing 2 :: IO (Cache String String)
  secondary <- newFileCache "test-2" Nothing testdir 0.2 :: IO (Cache String String)
  let cache = newCompositeCache "test" primary secondary
  cachePut cache "Key1" "Value1"
  threadDelay 10
  entries1 <- cacheEntries cache
  assertEqual "CompositeCache put 4 1" 1 entries1
  cachePut cache "Key2" "Value2"
  threadDelay 10
  entries2 <- cacheEntries cache
  assertEqual "CompositeCache put 4 2" 2 entries2
  cachePut cache "Key3" "Value3"
  entries3 <- cacheEntries cache
  assertEqual "CompositeCache put 4 3" 2 entries3
  value1 <- cacheLookup cache "Key1"
  assertEqual "CompositeCache put 4 4" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "CompositeCache put 4 5" (Just "Value2") value2
  value3 <- cacheLookup cache "Key3"
  assertEqual "CompositeCache put 4 6" (Just "Value3") value3
  closeTestDir testdir
  )

testCompositeCachePut5 = TestCase (do
  testdir <- openTestDir
  primary <- newMemCache "test-1" Nothing 2 :: IO (Cache String String)
  secondary <- newFileCache "test-2" Nothing testdir 0.2 :: IO (Cache String String)
  let cache = newCompositeCache "test" primary secondary
  cachePut cache "Key1" "Value1"
  threadDelay 10
  entries1 <- cacheEntries cache
  assertEqual "CompositeCache put 4 1" 1 entries1
  cachePut cache "Key2" "Value2"
  threadDelay 10
  _value <- cacheLookup cache "Key1"
  entries3 <- cacheEntries cache
  assertEqual "CompositeCache put 4 2" 2 entries3
  cachePut cache "Key3" "Value3"
  entries4 <- cacheEntries cache
  assertEqual "CompositeCache put 4 3" 2 entries4
  value1 <- cacheLookup cache "Key1"
  assertEqual "CompositeCache put 4 4" (Just "Value1") value1
  value2 <- cacheLookup cache "Key2"
  assertEqual "CompositeCache put 4 5" (Just "Value2") value2
  value3 <- cacheLookup cache "Key3"
  assertEqual "CompositeCache put 4 6" (Just "Value3") value3
  closeTestDir testdir
  )

testCompositeCacheExpire = TestList [
    testCompositeCacheExpire1, testCompositeCacheExpire2
  ]

testCompositeCacheExpire1 = TestCase (do
  testdir <- openTestDir
  primary <- newMemCacheWithExpiry "test-1" Nothing 2 0.2 :: IO (Cache String String)
  secondary <- newFileCache "test-2" Nothing testdir 0.2 :: IO (Cache String String)
  let cache = newCompositeCache "test" primary secondary
  cachePut cache "Key1" "Value1"
  entries1 <- cacheEntries cache
  assertEqual "CompositeCache expire 1 1" 1 entries1
  threadDelay 300000
  cacheExpire cache
  entries2 <- cacheEntries cache
  assertEqual "CompositeCache expire 1 2" 0 entries2
  closeTestDir testdir
  )

testCompositeCacheExpire2 = TestCase (do
  testdir <- openTestDir
  primary <- newMemCacheWithExpiry "test-1" Nothing 2 0.2 :: IO (Cache String String)
  secondary <- newFileCache "test-2" Nothing testdir 0.2 :: IO (Cache String String)
  let cache = newCompositeCache "test" primary secondary
  cachePut cache "Key1" "Value1"
  threadDelay 150000
  cachePut cache "Key2" "Value2"
  cacheExpire cache
  entries3 <- cacheEntries cache
  assertEqual "CompositeCache expire 1 1" 2 entries3
  threadDelay 150000
  cacheExpire cache
  entries4 <- cacheEntries cache
  assertEqual "CompositeCache expire 1 2" 1 entries4
  closeTestDir testdir
  )
