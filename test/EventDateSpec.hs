{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module EventDateSpec(testEventDate) where

import Test.HUnit
import Control.Monad.Reader
import Data.Event
import Data.Event.Date
import Data.Localised
import Data.Region
import qualified Data.Set as S
import Data.Time.Calendar

testEventDate :: Test
testEventDate = TestList [
       TestLabel "NthWeekOf" testNthWeekOf
     , TestLabel "Calendar" testEventCalendar
 ]

sampleCalendarConfig1 = createCalendarConfig [
    ("Easter", wildcardText "Easter", ListCalendar (S.fromList [
      fromGregorian 2022 April 17, 
      fromGregorian 2023 April 9, 
      fromGregorian 2024 March 31, 
      fromGregorian 2025 April 20, 
      fromGregorian 2026 April 5
      ]))
  ]
  
sampleRegionConfig1 = createRegionConfig [Region "Test" (wildcardText "Test Region") Country Nothing Nothing S.empty (Just rootLocale) [NamedCalendar "Easter"] ]

data TestConfig = TestConfig CalendarConfig RegionConfig

instance HasCalendarConfig TestConfig where
  getCalendarConfig (TestConfig config _) = config

instance HasRegionConfig TestConfig where
  getRegionConfig (TestConfig _ config) = config

sampleConfig1 = TestConfig sampleCalendarConfig1 sampleRegionConfig1

testNthWeekOf = TestList [
  testNthWeekOf1, testNthWeekOf2, testNthWeekOf3, testNthWeekOf4, testNthWeekOf5, testNthWeekOf6, testNthWeekOf7, testNthWeekOf8,
  testNthWeekOf9
  ]
  
testNthWeekOf1 = TestCase $ assertEqual "NthWeekOf 1" (fromGregorian 2024 July 5) (nthWeekOnOf First Friday (fromGregorian 2024 July 1))

testNthWeekOf2 = TestCase $ assertEqual "NthWeekOf 2" (fromGregorian 2024 July 26) (nthWeekOnOf Last Friday (fromGregorian 2024 July 1))

testNthWeekOf3 = TestCase $ assertEqual "NthWeekOf 3" (fromGregorian 2024 July 26) (nthWeekOnOf Last Friday (fromGregorian 2024 July 30))

testNthWeekOf4 = TestCase $ assertEqual "NthWeekOf 4" (fromGregorian 2024 June 10) (nthWeekOnOf Second Monday (fromGregorian 2024 June 11))

testNthWeekOf5 = TestCase $ assertEqual "NthWeekOf 5" (fromGregorian 2024 June 16) (nthWeekOnOf Third Sunday (fromGregorian 2024 June 11))

testNthWeekOf6 = TestCase $ assertEqual "NthWeekOf 6" (fromGregorian 2024 June 27) (nthWeekOnOf Fourth Thursday (fromGregorian 2024 June 11))

testNthWeekOf7 = TestCase $ assertEqual "NthWeekOf 7" (fromGregorian 2024 June 27) (nthWeekOnOf Fifth Thursday (fromGregorian 2024 June 11))

testNthWeekOf8 = TestCase $ assertEqual "NthWeekdOf 8" (fromGregorian 2024 May 31) (nthWeekOnOf Fifth Friday (fromGregorian 2024 May 15))

testNthWeekOf9 = TestCase $ assertEqual "NthWeekdOf 8" (fromGregorian 2023 February 28) (nthWeekOnOf Last Tuesday (fromGregorian 2023 February 26))

testEventCalendar = TestList [
    TestLabel "OnOrBefore" testEventCalendarOnOrBefore,
    TestLabel "OnOrAfter" testEventCalendarOnOrAfter,
    TestLabel "InCalendar" testInCalendar
  ]

testEventCalendarOnOrBefore = let
    tests = [
      testEventCalendarOnOrBefore1, testEventCalendarOnOrBefore2, testEventCalendarOnOrBefore3, testEventCalendarOnOrBefore4,
      testEventCalendarOnOrBefore5, testEventCalendarOnOrBefore6, testEventCalendarOnOrBefore7, testEventCalendarOnOrBefore8,
      testEventCalendarOnOrBefore9, testEventCalendarOnOrBefore10, testEventCalendarOnOrBefore11, testEventCalendarOnOrBefore12,
      testEventCalendarOnOrBefore13, testEventCalendarOnOrBefore14, testEventCalendarOnOrBefore15, testEventCalendarOnOrBefore16,
      testEventCalendarOnOrBefore17, testEventCalendarOnOrBefore18, testEventCalendarOnOrBefore19, testEventCalendarOnOrBefore20
     ]
  in
    TestList $ map (\t -> runReader t sampleConfig1) tests

testEventCalendarOnOrBefore1 = do
    let calendar = Daily
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 1 1" (fromGregorian 2023 March 26) before1
      assertEqual "EventCalendar OnOrBefore 1 2" (fromGregorian 2024 June 7) before2
      assertEqual "EventCalendar OnOrBefore 1 3" (fromGregorian 2025 October 12) before3
      )

testEventCalendarOnOrBefore2 = do
    let calendar = Weekly (S.singleton Wednesday)
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 2 1" (fromGregorian 2023 March 22) before1
      assertEqual "EventCalendar OnOrBefore 2 2" (fromGregorian 2024 June 5) before2
      assertEqual "EventCalendar OnOrBefore 2 3" (fromGregorian 2025 October 8) before3
      )

testEventCalendarOnOrBefore3 = do
    let calendar = Monthly (S.singleton 15)
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 3 1" (fromGregorian 2023 March 15) before1
      assertEqual "EventCalendar OnOrBefore 3 2" (fromGregorian 2024 May 15) before2
      assertEqual "EventCalendar OnOrBefore 3 3" (fromGregorian 2025 September 15) before3
      )

testEventCalendarOnOrBefore4 = do
    let calendar = Monthly (S.fromList [10, 15, 27])
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 4 1" (fromGregorian 2023 March 15) before1
      assertEqual "EventCalendar OnOrBefore 4 2" (fromGregorian 2024 May 27) before2
      assertEqual "EventCalendar OnOrBefore 4 3" (fromGregorian 2025 October 10) before3
      )

testEventCalendarOnOrBefore5 = do
    let calendar = Yearly (S.singleton May)
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 5 1" (fromGregorian 2022 May 1) before1
      assertEqual "EventCalendar OnOrBefore 5 2" (fromGregorian 2024 May 1) before2
      assertEqual "EventCalendar OnOrBefore 5 3" (fromGregorian 2025 May 1) before3
      )

testEventCalendarOnOrBefore6 = do
    let calendar = Yearly (S.fromList [May, September])
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 6 1" (fromGregorian 2022 September 1) before1
      assertEqual "EventCalendar OnOrBefore 6 2" (fromGregorian 2024 May 1) before2
      assertEqual "EventCalendar OnOrBefore 6 3" (fromGregorian 2025 September 1) before3
      )

testEventCalendarOnOrBefore7 = do
    let calendar = Yearly (S.fromList [February, June])
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 7 1" (fromGregorian 2023 February 1) before1
      assertEqual "EventCalendar OnOrBefore 7 2" (fromGregorian 2024 June 1) before2
      assertEqual "EventCalendar OnOrBefore 7 3" (fromGregorian 2025 June 1) before3
      )

testEventCalendarOnOrBefore8 = do
    let calendar = DayOfYear (S.fromList [(December, 25), (June, 21)])
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 8 1" (fromGregorian 2022 December 25) before1
      assertEqual "EventCalendar OnOrBefore 8 2" (fromGregorian 2023 December 25) before2
      assertEqual "EventCalendar OnOrBefore 8 3" (fromGregorian 2025 June 21) before3
      )

testEventCalendarOnOrBefore9 = do
    let calendar = RangeCalendar (DayOfYear (S.fromList [(December, 25), (June, 21)])) (Weekly (S.singleton Friday))
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 9 1" (fromGregorian 2022 December 25) before1
      assertEqual "EventCalendar OnOrBefore 9 2" (fromGregorian 2023 December 25) before2
      assertEqual "EventCalendar OnOrBefore 9 3" (fromGregorian 2025 June 21) before3
      )

testEventCalendarOnOrBefore10 = do
    let calendar = UnionCalendar [Weekly (S.singleton Monday), Monthly (S.singleton 11)]
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 10 1" (fromGregorian 2023 March 20) before1
      assertEqual "EventCalendar OnOrBefore 10 2" (fromGregorian 2024 June 3) before2
      assertEqual "EventCalendar OnOrBefore 10 3" (fromGregorian 2025 October 11) before3
      )

testEventCalendarOnOrBefore11 = do
    let calendar = IntersectionCalendar [Weekly (S.singleton Monday), Monthly (S.fromList [1, 2, 3, 5, 8, 13, 21])]
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 11 1" (fromGregorian 2023 March 13) before1
      assertEqual "EventCalendar OnOrBefore 11 2" (fromGregorian 2024 June 3) before2
      assertEqual "EventCalendar OnOrBefore 11 3" (fromGregorian 2025 September 8) before3
      )

testEventCalendarOnOrBefore12 = do
    let calendar = InvertedCalendar (Monthly (S.fromList [6, 7, 12]))
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 12 1" (fromGregorian 2023 March 26) before1
      assertEqual "EventCalendar OnOrBefore 12 2" (fromGregorian 2024 June 5) before2
      assertEqual "EventCalendar OnOrBefore 12 3" (fromGregorian 2025 October 11) before3
      )

testEventCalendarOnOrBefore13 = do
    let calendar = NthDayAfter 17 (Monthly (S.fromList [6, 7, 12]))
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 13 1" (fromGregorian 2023 March 23) before1 -- Rewinds to 6th
      assertEqual "EventCalendar OnOrBefore 13 2" (fromGregorian 2024 May 29) before2
      assertEqual "EventCalendar OnOrBefore 13 3" (fromGregorian 2025 September 29) before3
      )

testEventCalendarOnOrBefore14 = do
    let calendar = NthWeekday First Friday
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 14 1" (fromGregorian 2023 March 3) before1
      assertEqual "EventCalendar OnOrBefore 14 2" (fromGregorian 2024 June 7) before2
      assertEqual "EventCalendar OnOrBefore 14 3" (fromGregorian 2025 October 3) before3
      )

testEventCalendarOnOrBefore15 = do
    let calendar = NthWeekday Last Tuesday
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 15 1" (fromGregorian 2023 February 28) before1
      assertEqual "EventCalendar OnOrBefore 15 2" (fromGregorian 2024 May 28) before2
      assertEqual "EventCalendar OnOrBefore 15 3" (fromGregorian 2025 September 30) before3
      )

testEventCalendarOnOrBefore16 = do
    let calendar = NthWeekdayAfter 2 Tuesday (Monthly (S.fromList [6, 7, 12]))
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 16 1" (fromGregorian 2023 March 21) before1
      assertEqual "EventCalendar OnOrBefore 16 2" (fromGregorian 2024 May 21) before2
      assertEqual "EventCalendar OnOrBefore 16 3" (fromGregorian 2025 September 23) before3
      )

testEventCalendarOnOrBefore17 = do
    let calendar = ListCalendar (S.fromList [fromGregorian 2022 September 2, fromGregorian 2023 April 2, fromGregorian 2024 May 15, fromGregorian 2024 December 16])
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 17 1" (fromGregorian 2022 September 2) before1
      assertEqual "EventCalendar OnOrBefore 17 2" (fromGregorian 2024 May 15) before2
      assertEqual "EventCalendar OnOrBefore 17 3" (fromGregorian 2024 December 16) before3
      )

testEventCalendarOnOrBefore18 = do
    let calendar = NamedCalendar "Easter"
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 18 1" (fromGregorian 2022 April 17) before1
      assertEqual "EventCalendar OnOrBefore 18 2" (fromGregorian 2024 March 31) before2
      assertEqual "EventCalendar OnOrBefore 18 3" (fromGregorian 2025 April 20) before3
      )

testEventCalendarOnOrBefore19 = do
    let calendar = Conditional (NamedCalendar "Easter") (wildcardText "But not too eastery")
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 19 1" (fromGregorian 2022 April 17) before1
      assertEqual "EventCalendar OnOrBefore 19 2" (fromGregorian 2024 March 31) before2
      assertEqual "EventCalendar OnOrBefore 19 3" (fromGregorian 2025 April 20) before3
      )
      
testEventCalendarOnOrBefore20 = do
    let calendar = PublicHoliday "Test"
    before1 <- calendarDateOnOrBefore calendar (fromGregorian 2023 March 26)
    before2 <- calendarDateOnOrBefore calendar (fromGregorian 2024 June 7)
    before3 <- calendarDateOnOrBefore calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrBefore 20 1" (fromGregorian 2022 April 17) before1
      assertEqual "EventCalendar OnOrBefore 20 2" (fromGregorian 2024 March 31) before2
      assertEqual "EventCalendar OnOrBefore 20 3" (fromGregorian 2025 April 20) before3
      )


testEventCalendarOnOrAfter = let
    tests = [
      testEventCalendarOnOrAfter1, testEventCalendarOnOrAfter2, testEventCalendarOnOrAfter3, testEventCalendarOnOrAfter4,
      testEventCalendarOnOrAfter5, testEventCalendarOnOrAfter6, testEventCalendarOnOrAfter7, testEventCalendarOnOrAfter8,
      testEventCalendarOnOrAfter9, testEventCalendarOnOrAfter10, testEventCalendarOnOrAfter11, testEventCalendarOnOrAfter12,
      testEventCalendarOnOrAfter13, testEventCalendarOnOrAfter14, testEventCalendarOnOrAfter15, testEventCalendarOnOrAfter16,
      testEventCalendarOnOrAfter17, testEventCalendarOnOrAfter18, testEventCalendarOnOrAfter19, testEventCalendarOnOrAfter20
     ]
  in
    TestList $ map (\t -> runReader t sampleConfig1) tests

testEventCalendarOnOrAfter1 = do
    let calendar = Daily
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 1 1" (fromGregorian 2023 March 26) after1
      assertEqual "EventCalendar OnOrAfter 1 2" (fromGregorian 2024 June 7) after2
      assertEqual "EventCalendar OnOrAfter 1 3" (fromGregorian 2025 October 12) after3
      )

testEventCalendarOnOrAfter2 = do
    let calendar = Weekly (S.singleton Wednesday)
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 2 1" (fromGregorian 2023 March 29) after1
      assertEqual "EventCalendar OnOrAfter 2 2" (fromGregorian 2024 June 12) after2
      assertEqual "EventCalendar OnOrAfter 2 3" (fromGregorian 2025 October 15) after3
      )

testEventCalendarOnOrAfter3 = do
    let calendar = Monthly (S.singleton 15)
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 3 1" (fromGregorian 2023 April 15) after1
      assertEqual "EventCalendar OnOrAfter 3 2" (fromGregorian 2024 June 15) after2
      assertEqual "EventCalendar OnOrAfter 3 3" (fromGregorian 2025 October 15) after3
      )

testEventCalendarOnOrAfter4 = do
    let calendar = Monthly (S.fromList [10, 15, 27])
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 4 1" (fromGregorian 2023 March 27) after1
      assertEqual "EventCalendar OnOrAfter 4 2" (fromGregorian 2024 June 10) after2
      assertEqual "EventCalendar OnOrAfter 4 3" (fromGregorian 2025 October 15) after3
      )

testEventCalendarOnOrAfter5 = do
    let calendar = Yearly (S.singleton May)
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 5 1" (fromGregorian 2023 May 31) after1
      assertEqual "EventCalendar OnOrAfter 5 2" (fromGregorian 2025 May 31) after2
      assertEqual "EventCalendar OnOrAfter 5 3" (fromGregorian 2026 May 31) after3
      )

testEventCalendarOnOrAfter6 = do
    let calendar = Yearly (S.fromList [May, September])
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 6 1" (fromGregorian 2023 May 31) after1
      assertEqual "EventCalendar OnOrAfter 6 2" (fromGregorian 2024 September 30) after2
      assertEqual "EventCalendar OnOrAfter 6 3" (fromGregorian 2026 May 31) after3
      )

testEventCalendarOnOrAfter7 = do
    let calendar = Yearly (S.fromList [February, June])
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 7 1" (fromGregorian 2023 June 30) after1
      assertEqual "EventCalendar OnOrAfter 7 2" (fromGregorian 2024 June 30) after2
      assertEqual "EventCalendar OnOrAfter 7 3" (fromGregorian 2026 February 28) after3
      )

testEventCalendarOnOrAfter8 = do
    let calendar = DayOfYear (S.fromList [(December, 25), (June, 21)])
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 8 1" (fromGregorian 2023 June 21) after1
      assertEqual "EventCalendar OnOrAfter 8 2" (fromGregorian 2024 June 21) after2
      assertEqual "EventCalendar OnOrAfter 8 3" (fromGregorian 2025 December 25) after3
      )

testEventCalendarOnOrAfter9 = do
    let calendar = RangeCalendar (DayOfYear (S.fromList [(December, 25), (June, 21)])) (Weekly (S.singleton Friday))
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 9 1" (fromGregorian 2023 June 21) after1
      assertEqual "EventCalendar OnOrAfter 9 2" (fromGregorian 2024 June 21) after2
      assertEqual "EventCalendar OnOrAfter 9 3" (fromGregorian 2025 December 25) after3 -- Range end is friday after dec 25
      )

testEventCalendarOnOrAfter10 = do
    let calendar = UnionCalendar [Weekly (S.singleton Monday), Monthly (S.singleton 11)]
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 10 1" (fromGregorian 2023 March 27) after1
      assertEqual "EventCalendar OnOrAfter 10 2" (fromGregorian 2024 June 11) after2 -- Skips to 11th
      assertEqual "EventCalendar OnOrAfter 10 3" (fromGregorian 2025 October 13) after3
      )

testEventCalendarOnOrAfter11 = do
    let calendar = IntersectionCalendar [Weekly (S.singleton Monday), Monthly (S.fromList [1, 2, 3, 5, 8, 13, 21])]
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7) 
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 11 1" (fromGregorian 2023 April 3) after1
      assertEqual "EventCalendar OnOrAfter 11 2" (fromGregorian 2024 July 1) after2
      assertEqual "EventCalendar OnOrAfter 11 3" (fromGregorian 2025 October 13) after3
      )

testEventCalendarOnOrAfter12 = do
    let calendar = InvertedCalendar (Monthly (S.fromList [6, 7, 12]))
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 12 1" (fromGregorian 2023 March 26) after1
      assertEqual "EventCalendar OnOrAfter 12 2" (fromGregorian 2024 June 8) after2
      assertEqual "EventCalendar OnOrAfter 12 3" (fromGregorian 2025 October 13) after3
      )

testEventCalendarOnOrAfter13 = do
    let calendar = NthDayAfter 17 (Monthly (S.fromList [6, 7, 12]))
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 13 1" (fromGregorian 2023 March 29) after1
      assertEqual "EventCalendar OnOrAfter 13 2" (fromGregorian 2024 June 24) after2 
      assertEqual "EventCalendar OnOrAfter 13 3" (fromGregorian 2025 October 24) after3
      )

testEventCalendarOnOrAfter14 = do
    let calendar = NthWeekday First Friday
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 14 1" (fromGregorian 2023 April 7) after1
      assertEqual "EventCalendar OnOrAfter 14 2" (fromGregorian 2024 June 7) after2
      assertEqual "EventCalendar OnOrAfter 14 3" (fromGregorian 2025 November 7) after3
      )

testEventCalendarOnOrAfter15 = do
    let calendar = NthWeekday Last Tuesday
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 15 1" (fromGregorian 2023 March 28) after1
      assertEqual "EventCalendar OnOrAfter 15 2" (fromGregorian 2024 June 25) after2
      assertEqual "EventCalendar OnOrAfter 15 3" (fromGregorian 2025 October 28) after3
      )

testEventCalendarOnOrAfter16 = do
    let calendar = NthWeekdayAfter 2 Tuesday (Monthly (S.fromList [6, 7, 12]))
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 16 1" (fromGregorian 2023 April 18) after1
      assertEqual "EventCalendar OnOrAfter 16 2" (fromGregorian 2024 June 18) after2
      assertEqual "EventCalendar OnOrAfter 16 3" (fromGregorian 2025 October 14) after3 -- Second Tuesday after the 6th
      )

testEventCalendarOnOrAfter17 = do
    let calendar = ListCalendar (S.fromList [fromGregorian 2022 September 2, fromGregorian 2023 April 2, fromGregorian 2024 May 15, fromGregorian 2024 December 16])
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 17 1" (fromGregorian 2023 April 2) after1
      assertEqual "EventCalendar OnOrAfter 17 2" (fromGregorian 2024 December 16) after2
      assertEqual "EventCalendar OnOrAfter 17 3" (fromGregorian 2025 October 12) after3
      )

testEventCalendarOnOrAfter18 = do
    let calendar = NamedCalendar "Easter"
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 18 1" (fromGregorian 2023 April 9) after1
      assertEqual "EventCalendar OnOrAfter 18 2" (fromGregorian 2025 April 20) after2
      assertEqual "EventCalendar OnOrAfter 18 3" (fromGregorian 2026 April 5) after3
      )

testEventCalendarOnOrAfter19 = do
    let calendar = Conditional (NamedCalendar "Easter") (wildcardText "But not too eastery")
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 18 1" (fromGregorian 2023 April 9) after1
      assertEqual "EventCalendar OnOrAfter 18 2" (fromGregorian 2025 April 20) after2
      assertEqual "EventCalendar OnOrAfter 18 3" (fromGregorian 2026 April 5) after3
      )
 
testEventCalendarOnOrAfter20 = do
    let calendar = PublicHoliday "Test"
    after1 <- calendarDateOnOrAfter calendar (fromGregorian 2023 March 26)
    after2 <- calendarDateOnOrAfter calendar (fromGregorian 2024 June 7)
    after3 <- calendarDateOnOrAfter calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "EventCalendar OnOrAfter 20 1" (fromGregorian 2023 April 9) after1
      assertEqual "EventCalendar OnOrAfter 20 2" (fromGregorian 2025 April 20) after2
      assertEqual "EventCalendar OnOrAfter 20 3" (fromGregorian 2026 April 5) after3
      )
     
testInCalendar = let
    tests = [
      testInCalendar1, testInCalendar2, testInCalendar3, testInCalendar4,
      testInCalendar5, testInCalendar6, testInCalendar7, testInCalendar8,
      testInCalendar9, testInCalendar10, testInCalendar11, testInCalendar12,
      testInCalendar13, testInCalendar14, testInCalendar15, testInCalendar16,
      testInCalendar17, testInCalendar18, testInCalendar19, testInCalendar20,
      testInCalendar21
     ]
  in
    TestList $ map (\t -> runReader t sampleConfig1) tests

testInCalendar1 = do
    let calendar = Daily
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 1 1" True incal1
      assertEqual "InCalendar 1 2" True incal2
      assertEqual "InCalendar 1 3" True incal3
      )

testInCalendar2 = do
    let calendar = Weekly (S.singleton Sunday)
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 2 1" True incal1
      assertEqual "InCalendar 2 2" False incal2
      assertEqual "InCalendar 2 3" True incal3
      )

testInCalendar3 = do
    let calendar = Monthly (S.singleton 7)
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 3 1" False incal1
      assertEqual "InCalendar 3 2" True incal2
      assertEqual "InCalendar 3 3" False incal3
      )

testInCalendar4 = do
    let calendar = Monthly (S.fromList [7, 26])
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 4 1" True incal1
      assertEqual "InCalendar 4 2" True incal2
      assertEqual "InCalendar 4 3" False incal3
      )

testInCalendar5 = do
    let calendar = Yearly (S.singleton October)
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 5 1" False incal1
      assertEqual "InCalendar 5 2" False incal2
      assertEqual "InCalendar 5 3" True incal3
      )

testInCalendar6 = do
    let calendar = Yearly (S.fromList [March, June])
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 6 1" True incal1
      assertEqual "InCalendar 6 2" True incal2
      assertEqual "InCalendar 6 3" False incal3
      )

testInCalendar7 = do
    let calendar = Yearly (S.fromList [February, June])
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 7 1" False incal1
      assertEqual "InCalendar 7 2" True incal2
      assertEqual "InCalendar 7 3" False incal3
      )

testInCalendar8 = do
    let calendar = DayOfYear (S.fromList [(December, 25), (June, 21), (October, 12)])
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 8 1" False incal1
      assertEqual "InCalendar 8 2" False incal2
      assertEqual "InCalendar 8 3" True incal3
      )

testInCalendar9 = do
    let calendar = RangeCalendar (DayOfYear (S.fromList [(June, 21)])) (DayOfYear (S.fromList [(November, 11)]))
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 9 1" False incal1
      assertEqual "InCalendar 9 2" False incal2
      assertEqual "InCalendar 9 3" True incal3
      )

testInCalendar10 = do
    let calendar = UnionCalendar [Weekly (S.singleton Friday), Monthly (S.singleton 12)]
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 10 1" False incal1
      assertEqual "InCalendar 10 2" True incal2
      assertEqual "InCalendar 10 3" True incal3
      )

testInCalendar11 = do
    let calendar = IntersectionCalendar [Weekly (S.singleton Friday), Monthly (S.fromList [1, 7, 26, 12])]
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7) 
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 11 1" False incal1
      assertEqual "InCalendar 11 2" True incal2
      assertEqual "InCalendar 11 3" False incal3
      )

testInCalendar12 = do
    let calendar = InvertedCalendar (Monthly (S.fromList [6, 7, 12]))
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 12 1" True incal1
      assertEqual "InCalendar 12 2" False incal2
      assertEqual "InCalendar 12 3" False incal3
      )

testInCalendar13 = do
    let calendar = NthDayAfter 14 (Monthly (S.fromList [6, 7, 12]))
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 13 1" True incal1
      assertEqual "InCalendar 13 2" False incal2 
      assertEqual "InCalendar 13 3" False incal3
      )

testInCalendar14 = do
    let calendar = NthWeekday First Friday
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 14 1" False incal1
      assertEqual "InCalendar 14 2" True incal2
      assertEqual "InCalendar 14 3" False incal3
      )

testInCalendar15 = do
    let calendar = NthWeekday Last Sunday
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 15 1" True incal1
      assertEqual "InCalendar 15 2" False incal2
      assertEqual "InCalendar 15 3" False incal3
      )

testInCalendar16 = do
    let calendar = NthWeekdayAfter 2 Sunday (Monthly (S.fromList [12, 30]))
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 16 1" True incal1
      assertEqual "InCalendar 16 2" False incal2
      assertEqual "InCalendar 16 3" True incal3
      )

testInCalendar17 = do
    let calendar = ListCalendar (S.fromList [fromGregorian 2022 September 2, fromGregorian 2023 April 2, fromGregorian 2024 June 7, fromGregorian 2024 December 16])
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    return $ TestCase (do
      assertEqual "InCalendar 17 1" False incal1
      assertEqual "InCalendar 17 2" True  incal2
      assertEqual "InCalendar 17 3" False incal3
      )

testInCalendar18 = do
    let calendar = NamedCalendar "Easter"
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    incal4 <- inCalendar calendar (fromGregorian 2024 March 31)
    return $ TestCase (do
      assertEqual "InCalendar 18 1" False incal1
      assertEqual "InCalendar 18 2" False incal2
      assertEqual "InCalendar 18 3" False incal3
      assertEqual "InCalendar 18 4" True incal4
      )

testInCalendar19 = do
    let calendar = Conditional (NamedCalendar "Easter") (wildcardText "But not too eastery")
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    incal4 <- inCalendar calendar (fromGregorian 2024 March 31)
    return $ TestCase (do
      assertEqual "InCalendar 18 1" False incal1
      assertEqual "InCalendar 18 2" False incal2
      assertEqual "InCalendar 18 3" False incal3
      assertEqual "InCalendar 18 4" True incal4
     )

-- Test for holy week
testInCalendar20 = do
    let easter = NamedCalendar "Easter"
    let holyWeek = NthDayAfter (-6) easter
    let calendar = RangeCalendar holyWeek easter
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    incal4 <- inCalendar calendar (fromGregorian 2025 April 20)
    incal5 <- inCalendar calendar (fromGregorian 2025 April 17)
    return $ TestCase (do
      assertEqual "InCalendar 20 1" False incal1
      assertEqual "InCalendar 20 2" False incal2
      assertEqual "InCalendar 20 3" False incal3
      assertEqual "InCalendar 20 4" True incal4
      assertEqual "InCalendar 20 5" True incal5
      )

testInCalendar21 = do
    let calendar = PublicHoliday "Test"
    incal1 <- inCalendar calendar (fromGregorian 2023 March 26)
    incal2 <- inCalendar calendar (fromGregorian 2024 June 7)
    incal3 <- inCalendar calendar (fromGregorian 2025 October 12)
    incal4 <- inCalendar calendar (fromGregorian 2024 March 31)
    return $ TestCase (do
      assertEqual "InCalendar 21 1" False incal1
      assertEqual "InCalendar 21 2" False incal2
      assertEqual "InCalendar 21 3" False incal3
      assertEqual "InCalendar 21 4" True incal4
      )
