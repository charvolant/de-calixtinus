module TestUtils where
  
import Test.HUnit
import Data.Maybe

assertFloatEqual :: String -> Float -> Float -> Float -> Assertion
assertFloatEqual msg expected actual precision =
  assertBool (msg ++ " expected " ++ show expected ++ " but got " ++ show actual) (abs (expected - actual) < precision)

assertMaybeFloatEqual :: String -> Maybe Float -> Maybe Float -> Float -> Assertion
assertMaybeFloatEqual msg expected actual precision
  | isNothing expected && isJust actual = assertBool (msg ++ " expected " ++ show expected ++ " but got " ++ show actual) False
  | isJust expected && isNothing actual = assertBool (msg ++ " expected " ++ show expected ++ " but got " ++ show actual) False
  | isNothing expected && isNothing actual = assertBool msg True
  | otherwise = assertBool (msg ++ " expected " ++ show expected ++ " but got " ++ show actual) (abs (fromJust expected - fromJust actual) < precision)


