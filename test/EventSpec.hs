{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module EventSpec(testEvent) where

import Test.HUnit
import Control.Monad.Reader
import Data.Aeson
import Data.Event
import Data.Localised
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.LocalTime

testEvent :: Test
testEvent = TestList [
       TestLabel "Calendar" testEventCalendar
     , TestLabel "Time" testEventTime
     , TestLabel "OpenHours" testOpenHours
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
  
sampleCalendarConfig2 = createCalendarConfig [
      ("Christmas", wildcardText "Christmas", DayOfYear (S.singleton (December, 25)))
    , ("KingsBirthday", wildcardText "King's Birthday", NthWeekdayAfter 2 Monday (DayOfYear (S.singleton (June, 1))))
  ]


testEventCalendar = TestList [
    TestLabel "JSON" testEventCalendarJSON,
    TestLabel "CalendarConfig" testCalendarConfig
  ]
  
testEventCalendarJSON = TestList [
  testEventCalendarToJSON1, testEventCalendarToJSON2, testEventCalendarToJSON3, testEventCalendarToJSON4, testEventCalendarToJSON5, testEventCalendarToJSON6,
  testEventCalendarToJSON7, testEventCalendarToJSON8, testEventCalendarToJSON9, testEventCalendarToJSON10, testEventCalendarToJSON11, testEventCalendarToJSON12,
  testEventCalendarToJSON13, testEventCalendarToJSON14, testEventCalendarToJSON15, testEventCalendarToJSON16, testEventCalendarToJSON17,
  testEventCalendarFromJSON1, testEventCalendarFromJSON2, testEventCalendarFromJSON3, testEventCalendarFromJSON4, testEventCalendarFromJSON5, testEventCalendarFromJSON6,
  testEventCalendarFromJSON7, testEventCalendarFromJSON8, testEventCalendarFromJSON9, testEventCalendarFromJSON10, testEventCalendarFromJSON11, testEventCalendarFromJSON12,
  testEventCalendarFromJSON13, testEventCalendarFromJSON14, testEventCalendarFromJSON15, testEventCalendarFromJSON16, testEventCalendarFromJSON17
  ]

testEventCalendarToJSON1 =
  let
    calendar = Daily
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 1 1" "{\"type\":\"daily\"}" ec
      )

testEventCalendarToJSON2 =
  let
    calendar = Weekly (S.singleton Monday)
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 2 1" "{\"days\":[\"monday\"],\"type\":\"weekly\"}" ec
      )

testEventCalendarToJSON3 =
  let
    calendar = Weekly (S.fromList [Monday, Thursday])
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 3 1" "{\"days\":[\"monday\",\"thursday\"],\"type\":\"weekly\"}" ec
      )

testEventCalendarToJSON4 =
  let
    calendar = Monthly (S.fromList [1, 5])
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 4 1" "{\"days\":[1,5],\"type\":\"monthly\"}" ec
      )

testEventCalendarToJSON5 =
  let
    calendar = Yearly (S.fromList [January, June])
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 5 1" "{\"months\":[1,6],\"type\":\"yearly\"}" ec
      )

testEventCalendarToJSON6 =
  let
    base = Yearly (S.fromList [January, June])
    condition = wildcardText "On the first Tuesday"
    calendar = Conditional base condition
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 6 1" "{\"calendar\":{\"months\":[1,6],\"type\":\"yearly\"},\"condition\":\"On the first Tuesday\",\"type\":\"conditional\"}" ec
      )

testEventCalendarToJSON7 =
  let
    calendar = DayOfYear (S.fromList [(January, 1), (December, 25)])
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 7 1" "{\"days\":[\"01-01\",\"12-25\"],\"type\":\"day-of-year\"}" ec
      )

testEventCalendarToJSON8 =
  let
    calendar = RangeCalendar (Weekly $ S.singleton Tuesday) (Weekly $ S.singleton Thursday)
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 8 1" "{\"from\":{\"days\":[\"tuesday\"],\"type\":\"weekly\"},\"to\":{\"days\":[\"thursday\"],\"type\":\"weekly\"},\"type\":\"range\"}" ec
      )

testEventCalendarToJSON9 =
  let
    calendar = UnionCalendar [(Weekly $ S.singleton Tuesday), (Weekly $ S.singleton Thursday)]
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 9 1" "{\"calendars\":[{\"days\":[\"tuesday\"],\"type\":\"weekly\"},{\"days\":[\"thursday\"],\"type\":\"weekly\"}],\"type\":\"union\"}" ec
      )

testEventCalendarToJSON10 =
  let
    calendar = IntersectionCalendar [(Weekly $ S.singleton Tuesday), (Yearly $ S.singleton August)]
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 10 1" "{\"calendars\":[{\"days\":[\"tuesday\"],\"type\":\"weekly\"},{\"months\":[8],\"type\":\"yearly\"}],\"type\":\"intersection\"}" ec
      )

testEventCalendarToJSON11 =
  let
    calendar = InvertedCalendar (Weekly $ S.singleton Tuesday)
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 11 1" "{\"calendar\":{\"days\":[\"tuesday\"],\"type\":\"weekly\"},\"type\":\"except\"}" ec
      )

testEventCalendarToJSON12 =
  let
    calendar = NthDayAfter 2 (Yearly $ S.singleton May)
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 13 1" "{\"calendar\":{\"months\":[5],\"type\":\"yearly\"},\"nth\":2,\"type\":\"nth-day-after\"}" ec
      )

testEventCalendarToJSON13 =
  let
    calendar = NthWeekday Third Tuesday
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 13 1" "{\"day\":\"tuesday\",\"nth\":\"Third\",\"type\":\"nth-weekday\"}" ec
      )

testEventCalendarToJSON14 =
  let
    calendar = NthWeekdayAfter 1 Tuesday (Monthly $ S.singleton 5)
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 14 1" "{\"calendar\":{\"days\":[5],\"type\":\"monthly\"},\"day\":\"tuesday\",\"nth\":1,\"type\":\"nth-weekday-after\"}" ec
      )

testEventCalendarToJSON15 =
  let
    calendar = ListCalendar (S.fromList [fromGregorian 2024 June 21])
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 15 1" "{\"dates\":[\"2024-06-21\"],\"type\":\"list\"}" ec
      )

testEventCalendarToJSON16 =
  let
    calendar = NamedCalendar "Easter"
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 16 1" "{\"key\":\"Easter\",\"type\":\"named\"}" ec
      )
      
testEventCalendarToJSON17 =
  let
    calendar = PublicHoliday "Spain"
    ec = encode calendar
  in
    TestCase (do
      assertEqual "EventCalendar ToJSON 17 1" "{\"region\":\"Spain\",\"type\":\"public-holiday\"}" ec
      )

testEventCalendarFromJSON1 =
  let
    calendar = decode "{\"type\":\"daily\"}" :: Maybe EventCalendar
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 1 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 1 2" Daily calendar'
      )

testEventCalendarFromJSON2 =
  let
    calendar = decode "{\"days\":[\"sunday\"],\"type\":\"weekly\"}" :: Maybe EventCalendar
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 2 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 2 2" (Weekly (S.singleton Sunday)) calendar'
      )

testEventCalendarFromJSON3 =
  let
    calendar = decode "{\"days\":[\"sunday\"],\"type\":\"weekly\"}" :: Maybe EventCalendar
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 3 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 3 2" (Weekly (S.singleton Sunday)) calendar'
      )


testEventCalendarFromJSON4 =
  let
    calendar = decode "{\"days\":[15,8],\"type\":\"monthly\"}" :: Maybe EventCalendar
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 4 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 4 2" (Monthly (S.fromList [8, 15])) calendar'
      )

testEventCalendarFromJSON5 =
  let
    calendar = decode "{\"months\":[3,9],\"type\":\"yearly\"}" :: Maybe EventCalendar
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 5 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 5 2" (Yearly (S.fromList [March, September])) calendar'
      )

testEventCalendarFromJSON6 =
  let
    calendar = decode "{\"calendar\":{\"months\":[3,9],\"type\":\"yearly\"},\"condition\":\"On the last Tuesday\",\"type\":\"conditional\"}" :: Maybe EventCalendar
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 6 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 6 2" (Conditional (Yearly (S.fromList [March, September])) (wildcardText "On the last Tuesday")) calendar'
      )


testEventCalendarFromJSON7 =
  let
    calendar = decode "{\"days\":[\"01-01\",\"12-25\"],\"type\":\"day-of-year\"}" :: Maybe EventCalendar
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 7 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 7 2" (DayOfYear (S.fromList [(January, 1), (December, 25)])) calendar'
      )

testEventCalendarFromJSON8 =
  let
    expected = RangeCalendar (Weekly $ S.singleton Tuesday) (Weekly $ S.singleton Thursday)
    calendar = decode "{\"from\":{\"days\":[\"tuesday\"],\"type\":\"weekly\"},\"to\":{\"days\":[\"thursday\"],\"type\":\"weekly\"},\"type\":\"range\"}"
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 8 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 8 1" expected calendar'
      )

testEventCalendarFromJSON9 =
  let
    expected = UnionCalendar [(Weekly $ S.singleton Tuesday), (Weekly $ S.singleton Thursday)]
    calendar = decode "{\"calendars\":[{\"days\":[\"tuesday\"],\"type\":\"weekly\"},{\"days\":[\"thursday\"],\"type\":\"weekly\"}],\"type\":\"union\"}"
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 9 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 9 2" expected calendar'
      )

testEventCalendarFromJSON10 =
  let
    expected = IntersectionCalendar [(Weekly $ S.singleton Tuesday), (Yearly $ S.singleton August)]
    calendar = decode "{\"calendars\":[{\"days\":[\"tuesday\"],\"type\":\"weekly\"},{\"months\":[8],\"type\":\"yearly\"}],\"type\":\"intersection\"}"
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 10 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 10 2" expected calendar'
      )

testEventCalendarFromJSON11 =
  let
    expected = InvertedCalendar (Weekly $ S.singleton Tuesday)
    calendar = decode "{\"calendar\":{\"days\":[\"tuesday\"],\"type\":\"weekly\"},\"type\":\"except\"}"
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 11 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 11 2" expected calendar'
      )

testEventCalendarFromJSON12 =
  let
    expected = NthDayAfter 2 (Yearly $ S.singleton May)
    calendar = decode "{\"calendar\":{\"months\":[5],\"type\":\"yearly\"},\"nth\":2,\"type\":\"nth-day-after\"}"
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 12 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 12 2" expected calendar'
      )

testEventCalendarFromJSON13 =
  let
    expected = NthWeekday Third Tuesday
    calendar = decode "{\"day\":\"tuesday\",\"nth\":\"Third\",\"type\":\"nth-weekday\"}"
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 13 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 13 2" expected calendar'
      )

testEventCalendarFromJSON14 =
  let
    expected = NthWeekdayAfter 1 Tuesday (Monthly $ S.singleton 5)
    calendar = decode "{\"calendar\":{\"days\":[5],\"type\":\"monthly\"},\"day\":\"tuesday\",\"nth\":1,\"type\":\"nth-weekday-after\"}"
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 14 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 14 2" expected calendar'
      )

testEventCalendarFromJSON15 =
  let
    expected = ListCalendar (S.fromList [fromGregorian 2024 June 21])
    calendar = decode "{\"dates\":[\"2024-06-21\"],\"type\":\"list\"}"
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 15 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 15 2" expected calendar'
      )

testEventCalendarFromJSON16 =
  let
    expected = NamedCalendar "Easter"
    calendar = decode "{\"key\":\"Easter\",\"type\":\"named\"}"
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 16 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 16 2" expected calendar'
      )

testEventCalendarFromJSON17 =
  let
    expected = PublicHoliday "Portugal"
    calendar = decode "{\"region\":\"Portugal\",\"type\":\"public-holiday\"}"
  in
    TestCase (do
      assertBool "EventCalendar FromJSON 17 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 17 2" expected calendar'
      )

testEventTime = TestList [
    TestLabel "JSON" testEventTimeJSON
  ]
  
testEventTimeJSON = TestList [
  testEventTimeToJSON1, testEventTimeToJSON2, testEventTimeToJSON3, testEventTimeToJSON4,
  testEventTimeFromJSON1, testEventTimeFromJSON2, testEventTimeFromJSON3, testEventTimeFromJSON4, testEventTimeFromJSON5
  ]

testEventTimeToJSON1 =
  let
    time = EventTime [(TimeOfDay 10 0 0, TimeOfDay 18 0 0)]
    et = encode time
  in
    TestCase (do
      assertEqual "EventTime ToJSON 1 1" "\"1000-1800\"" et
      )

testEventTimeToJSON2 =
  let
    time = EventTime [(TimeOfDay 10 0 0, TimeOfDay 11 0 0), (TimeOfDay 12 0 0, TimeOfDay 13 0 0)]
    et = encode time
  in
    TestCase (do
      assertEqual "EventTime ToJSON 2 1" "\"1000-1100, 1200-1300\"" et
      )

testEventTimeToJSON3 =
  let
    time = EventClosed
    et = encode time
  in
    TestCase (do
      assertEqual "EventTime ToJSON 3 1" "\"closed\"" et
      )

testEventTimeToJSON4 =
  let
    time = EventOpen
    et = encode time
  in
    TestCase (do
      assertEqual "EventTime ToJSON 4 1" "\"open\"" et
      )

testEventTimeFromJSON1 =
  let
    time = decode "\"1000-1800\"" :: Maybe EventTime
  in
    TestCase (do
      assertBool "EventTime FromJSON 1 1" (isJust time)
      let time' = fromJust time
      assertEqual "EventTime FromJSON 1 2" (EventTime [(TimeOfDay 10 0 0, TimeOfDay 18 0 0)]) time'
      )

testEventTimeFromJSON2 =
  let
    time = decode "\" 1000- 1800  \"" :: Maybe EventTime
  in
    TestCase (do
      assertBool "EventTime FromJSON 2 1" (isJust time)
      let time' = fromJust time
      assertEqual "EventTime FromJSON 2 2" (EventTime [(TimeOfDay 10 0 0, TimeOfDay 18 0 0)]) time'
      )


testEventTimeFromJSON3 =
  let
    time = decode "\"1000-1100, 1200-1300\"" :: Maybe EventTime
  in
    TestCase (do
      assertBool "EventTime FromJSON 3 1" (isJust time)
      let time' = fromJust time
      assertEqual "EventTime FromJSON 3 2" (EventTime [(TimeOfDay 10 0 0, TimeOfDay 11 0 0), (TimeOfDay 12 0 0, TimeOfDay 13 0 0)]) time'
      )

testEventTimeFromJSON4 =
  let
    time = decode "\"closed\"" :: Maybe EventTime
  in
    TestCase (do
      assertBool "EventTime FromJSON 4 1" (isJust time)
      let time' = fromJust time
      assertEqual "EventTime FromJSON 4 2" EventClosed time'
      )

testEventTimeFromJSON5 =
  let
    time = decode "\"open\"" :: Maybe EventTime
  in
    TestCase (do
      assertBool "EventTime FromJSON 5 1" (isJust time)
      let time' = fromJust time
      assertEqual "EventTime FromJSON 5 2" EventOpen time'
      )

testOpenHours = TestList [
    TestLabel "JSON" testOpenHoursJSON
  ]
  
testOpenHoursJSON = TestList [
  testOpenHoursToJSON1, testOpenHoursToJSON2,
  testOpenHoursFromJSON1, testOpenHoursFromJSON2, testOpenHoursFromJSON3
  ]

testOpenHoursToJSON1 =
  let
    hours = OpenHours [EventHours Daily EventOpen]
    eh = encode hours
  in
    TestCase (do
      assertEqual "OpenHours ToJSON 1 1" "{\"calendar\":{\"type\":\"daily\"},\"hours\":\"open\"}" eh
      )

testOpenHoursToJSON2 =
  let
    hours = OpenHours [EventHours (Weekly (S.singleton Friday)) (EventTime [(TimeOfDay 10 0 0, TimeOfDay 11 0 0)])]
    eh = encode hours
  in
    TestCase (do
      assertEqual "OpenHours ToJSON 1 1" "{\"calendar\":{\"days\":[\"friday\"],\"type\":\"weekly\"},\"hours\":\"1000-1100\"}" eh
      )

testOpenHoursFromJSON1 =
  let
    hours = decode "\"1000-1800\"" :: Maybe OpenHours
  in
    TestCase (do
      assertBool "OpenHours FromJSON 1 1" (isJust hours)
      let hours' = fromJust hours
      assertEqual "OpenHours FromJSON 1 2" (OpenHours [EventHours Daily (EventTime [(TimeOfDay 10 0 0, TimeOfDay 18 0 0)])]) hours'
      )

testOpenHoursFromJSON2 =
  let
    hours = decode "{\"calendar\":{\"days\":[\"sunday\"],\"type\":\"weekly\"},\"hours\":\"1000-1200\"}" :: Maybe OpenHours
    target = OpenHours [
        EventHours 
          (Weekly (S.singleton Sunday))
          (EventTime [(TimeOfDay 10 0 0, TimeOfDay 12 0 0)])
      ]
  in
    TestCase (do
      assertBool "OpenHours FromJSON 2 1" (isJust hours)
      let hours' = fromJust hours
      assertEqual "OpenHours FromJSON 2 2" target hours'
      )


testOpenHoursFromJSON3 =
  let
    hours = decode "[{\"calendar\":{\"days\":[\"sunday\"],\"type\":\"weekly\"},\"hours\":\"closed\"}, \"1000-1100, 1200-1300\"]" :: Maybe OpenHours
    target = OpenHours [
         EventHours 
          (Weekly (S.singleton Sunday))
          EventClosed
       , EventHours
          Daily
          (EventTime [(TimeOfDay 10 0 0, TimeOfDay 11 0 0), (TimeOfDay 12 0 0, TimeOfDay 13 0 0)])
      ]
  in
    TestCase (do
      assertBool "OpenHours FromJSON 3 1" (isJust hours)
      let hours' = fromJust hours
      assertEqual "OpenHours FromJSON 3 2" target hours'
      )

testCalendarConfig = TestList [
    TestLabel "JSON" testCalendarConfigJSON
  ]
  
testCalendarConfigJSON = TestList [
  testCalendarConfigToJSON1, testCalendarConfigToJSON2,
  testCalendarConfigFromJSON1, testCalendarConfigFromJSON2
  ]

testCalendarConfigToJSON1 = let
    config = sampleCalendarConfig1
    ec = encode config
  in
    TestCase (do
      assertEqual "CalendarConfig ToJSON 1 1" "[{\"calendar\":{\"dates\":[\"2022-04-17\",\"2023-04-09\",\"2024-03-31\",\"2025-04-20\",\"2026-04-05\"],\"type\":\"list\"},\"key\":\"Easter\",\"name\":\"Easter\"}]" ec
    )


testCalendarConfigToJSON2 = let
    config = sampleCalendarConfig2
    ec = encode config
  in
    TestCase (do
      assertEqual "CalendarConfig ToJSON 2 1" "[{\"calendar\":{\"days\":[\"12-25\"],\"type\":\"day-of-year\"},\"key\":\"Christmas\",\"name\":\"Christmas\"},{\"calendar\":{\"calendar\":{\"days\":[\"06-01\"],\"type\":\"day-of-year\"},\"day\":\"monday\",\"nth\":2,\"type\":\"nth-weekday-after\"},\"key\":\"KingsBirthday\",\"name\":\"King's Birthday\"}]" ec
    )

testCalendarConfigFromJSON1 =
  let
    config = decode "[{\"calendar\":{\"dates\":[\"2022-04-17\",\"2023-04-09\",\"2024-03-31\",\"2025-04-20\",\"2026-04-05\"],\"type\":\"list\"},\"key\":\"Easter\",\"name\":\"Easter\"}]" :: Maybe CalendarConfig
  in
    TestCase (do
      assertBool "CalendarConfig FromJSON 1 1" (isJust config)
      let config' = fromJust config
      assertEqual "CalendarConfig FromJSON 1 2" sampleCalendarConfig1 config'
      )

testCalendarConfigFromJSON2 =
  let
    config = decode "[{\"calendar\":{\"days\":[\"12-25\"],\"type\":\"day-of-year\"},\"key\":\"Christmas\",\"name\":\"Christmas\"},{\"calendar\":{\"calendar\":{\"days\":[\"06-01\"],\"type\":\"day-of-year\"},\"day\":\"monday\",\"nth\":2,\"type\":\"nth-weekday-after\"},\"key\":\"KingsBirthday\",\"name\":\"King's Birthday\"}]" :: Maybe CalendarConfig
  in
    TestCase (do
      assertBool "CalendarConfig FromJSON 2 1" (isJust config)
      let config' = fromJust config
      assertEqual "CalendarConfig FromJSON 2 2" sampleCalendarConfig2 config'
      )
