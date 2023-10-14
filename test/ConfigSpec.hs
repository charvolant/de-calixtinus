{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec(testConfig) where

import Test.HUnit
import Camino.Config
import Data.Map as M
import Data.Text

testConfig1 = Config $ M.fromList [
    ("foo", Value "True"),
    ("bar", Value "Hello"),
    ("baz", Config $ M.fromList [
      ("foo", Value "100"),
      ("bar", Value "10.1")
    ])
  ] 

testConfig2 = readConfig "{ \
   \    \"foo\": \"T\", \
   \    \"bar\": \"Hello\", \
   \    \"baz\": { \
   \      \"foo\": \"100\", \
   \      \"bar\": \"10.1\" \
   \    } \
   \  }"

testConfig = TestList [
  TestLabel "GetConfigValue" testGetConfigValue,
  TestLabel "GetConfigValue" testReadConfig
  ]
  
testGetConfigValue = TestList [
  testGetConfigValue1, testGetConfigValue2, testGetConfigValue3, testGetConfigValue4, testGetConfigValue5,
  testGetConfigValue6
  
  ]

testGetConfigValue1 = TestCase (assertEqual "getConfigValue 1" True (getConfigValue "foo" False testConfig1))

testGetConfigValue2 = TestCase (assertEqual "getConfigValue 2" "Hello" (getConfigValue "bar" "Nothing" testConfig1 :: String))

testGetConfigValue3 = TestCase (assertEqual "getConfigValue 3" 100 (getConfigValue "baz.foo" 20 testConfig1 :: Int))

testGetConfigValue4 = TestCase (assertEqual "getConfigValue 4" 10.1 (getConfigValue "baz.bar" 20 testConfig1 :: Double))

testGetConfigValue5 = TestCase (assertEqual "getConfigValue 5" "10.1" (getConfigValue "baz.bar" "" testConfig1 :: Text))

testGetConfigValue6 = TestCase (assertEqual "getConfigValue 6" "Whoops" (getConfigValue "baz.bar1" "Whoops" testConfig1 :: Text))

 
testReadConfig = TestList [
  testReadConfig1, testReadConfig2, testReadConfig3, testReadConfig4, testReadConfig5, testReadConfig6
  ]

testReadConfig1 = TestCase (assertEqual "readConfig 1" True (getConfigValue "foo" False testConfig2))

testReadConfig2 = TestCase (assertEqual "readConfig 2" "Hello" (getConfigValue "bar" "Nothing" testConfig2 :: String))

testReadConfig3 = TestCase (assertEqual "readConfig 3" 100 (getConfigValue "baz.foo" 20 testConfig2 :: Int))

testReadConfig4 = TestCase (assertEqual "readConfig 4" 10.1 (getConfigValue "baz.bar" 20 testConfig2 :: Double))

testReadConfig5 = TestCase (assertEqual "readConfig 5" "10.1" (getConfigValue "baz.bar" "" testConfig2 :: Text))

testReadConfig6 = TestCase (assertEqual "readConfig 6" "Whoops" (getConfigValue "baz.bar1" "Whoops" testConfig2 :: Text))
