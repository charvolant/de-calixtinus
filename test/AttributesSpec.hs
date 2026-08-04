{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module AttributesSpec(testAttributes) where

import Test.HUnit
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Data.Attributes
import qualified Data.Map as M
import TestUtils
import Text.RawString.QQ

testAttributes :: Test
testAttributes = TestList [
    TestLabel "Attribute Value" testAttributeValue
  ]

av1 = NullV

avs1 = [r|null|] :: LB.ByteString

av2 = BooleanV True

avs2 = [r|true|] :: LB.ByteString

av3 = IntegerV 154

avs3 = [r|154|] :: LB.ByteString

av4 = DecimalV 45.2

avs4 = [r|45.2|] :: LB.ByteString

av5 = StringV "Hello"

avs5 = [r|"Hello"|] :: LB.ByteString

av6 = ListV [av2, av3]

avs6 = [r|
[
  true,
  154
]
|] :: LB.ByteString

a1 = Attributes $ M.fromList [("av1", av1), ("av2", av2)]

av7 = ObjectV a1

avs7 = [r|
{
  "av1": null,
  "av2": true
}
|] :: LB.ByteString

testAttributeValue :: Test
testAttributeValue = TestList [
    TestLabel "JSON" testAttributeValueJSON
  ]

testAttributeValueJSON = TestList [
  testAttributeValueJSON1, testAttributeValueJSON2, testAttributeValueJSON3, testAttributeValueJSON4,
  testAttributeValueJSON5, testAttributeValueJSON6, testAttributeValueJSON7, testAttributeValueJSON8,
  testAttributeValueJSON9, testAttributeValueJSON10, testAttributeValueJSON11, testAttributeValueJSON12,
  testAttributeValueJSON13, testAttributeValueJSON14
 ]

testAttributeValueJSON1 = TestCase (assertEqualBSStripped "Test Attribute Value JSON 1" avs1 (encode av1))

testAttributeValueJSON2 = TestCase (assertEqual "Test Attribute Value JSON 2" (Right av1) (eitherDecode avs1))

testAttributeValueJSON3 = TestCase (assertEqualBSStripped "Test Attribute Value JSON 3" avs2 (encode av2))

testAttributeValueJSON4 = TestCase (assertEqual "Test Attribute Value JSON 4" (Right av2) (eitherDecode avs2))

testAttributeValueJSON5 = TestCase (assertEqualBSStripped "Test Attribute Value JSON 5" avs3 (encode av3))

testAttributeValueJSON6 = TestCase (assertEqual "Test Attribute Value JSON 6" (Right av3) (eitherDecode avs3))

testAttributeValueJSON7 = TestCase (assertEqualBSStripped "Test Attribute Value JSON 7" avs4 (encode av4))

testAttributeValueJSON8 = TestCase (assertEqual "Test Attribute Value JSON 8" (Right av4) (eitherDecode avs4))

testAttributeValueJSON9 = TestCase (assertEqualBSStripped "Test Attribute Value JSON 9" avs5(encode av5))

testAttributeValueJSON10 = TestCase (assertEqual "Test Attribute Value JSON 10" (Right av5) (eitherDecode avs5))

testAttributeValueJSON11 = TestCase (assertEqualBSStripped "Test Attribute Value JSON 11" avs6 (encode av6))

testAttributeValueJSON12 = TestCase (assertEqual "Test Attribute Value JSON 12" (Right av6) (eitherDecode avs6))

testAttributeValueJSON13 = TestCase (assertEqualBSStripped "Test Attribute Value JSON 13" avs7 (encode av7))

testAttributeValueJSON14 = TestCase (assertEqual "Test Attribute Value JSON 14" (Right av7) (eitherDecode avs7))
