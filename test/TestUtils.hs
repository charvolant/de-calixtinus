{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module TestUtils where
  
import Test.HUnit
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import Data.Util
import System.Directory
import System.FilePath

assertFloatEqual :: String -> Float -> Float -> Float -> Assertion
assertFloatEqual msg expected actual precision =
  assertBool (msg ++ " expected " ++ show expected ++ " but got " ++ show actual) (abs (expected - actual) < precision)

assertMaybeFloatEqual :: String -> Maybe Float -> Maybe Float -> Float -> Assertion
assertMaybeFloatEqual msg expected actual precision
  | isNothing expected && isJust actual = assertBool (msg ++ " expected " ++ show expected ++ " but got " ++ show actual) False
  | isJust expected && isNothing actual = assertBool (msg ++ " expected " ++ show expected ++ " but got " ++ show actual) False
  | isNothing expected && isNothing actual = assertBool msg True
  | otherwise = assertBool (msg ++ " expected " ++ show expected ++ " but got " ++ show actual) (abs (fromJust expected - fromJust actual) < precision)

assertEqualStripped :: String -> String -> String -> Assertion
assertEqualStripped msg expected actual = let
    expected' = T.filter (not . isSpace) (T.pack expected)
    actual' = T.filter (not . isSpace) (T.pack actual)
    (match, p, s1, s2) = case T.commonPrefixes expected' actual' of
      Nothing -> (False, 0, expected', actual')
      Just (common, s1', s2') -> (T.length s1' == 0 && T.length s2' == 0, T.length common, s1', s2')
  in
    assertBool (msg ++ " mismatch at " ++ show p ++ "\"..." ++ take 20 (T.unpack s1) ++ "\" and \"..." ++ take 20 (T.unpack s2)) match

openTestDir :: IO FilePath
openTestDir = do
  tmpdir <- getTemporaryDirectory
  testdir <- loopM (\n -> do
    let f = tmpdir </> ("test" ++ show n)
    exist <- doesDirectoryExist f
    return $ if exist then Left (n + 1) else Right f
    ) 0
  createDirectoryIfMissing True testdir
  return testdir

closeTestDir :: FilePath -> IO ()
closeTestDir dir = do
  scanDirectory (\f -> removeFile f) (\d -> removeDirectory d) dir

