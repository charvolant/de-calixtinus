{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module EventSpec(testEvent) where

import Test.HUnit
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
  ]

testEventCalendar = TestList [
    TestLabel "JSON" testEventCalendarJSON
  ]
  
testEventCalendarJSON = TestList [
  testEventCalendarToJSON1, testEventCalendarToJSON2, testEventCalendarToJSON3, testEventCalendarToJSON4, testEventCalendarToJSON5, testEventCalendarToJSON6,
  testEventCalendarFromJSON1, testEventCalendarFromJSON2, testEventCalendarFromJSON3, testEventCalendarFromJSON4, testEventCalendarFromJSON5, testEventCalendarFromJSON6 
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
      assertBool "EventCalendar FromJSON 5 1" (isJust calendar)
      let calendar' = fromJust calendar
      assertEqual "EventCalendar FromJSON 5 2" (Conditional (Yearly (S.fromList [March, September])) (wildcardText "On the last Tuesday")) calendar'
      )


testEventTime = TestList [
    TestLabel "JSON" testEventTimeJSON
  ]
  
testEventTimeJSON = TestList [
  testEventTimeToJSON1, testEventTimeToJSON2,
  testEventTimeFromJSON1, testEventTimeFromJSON2, testEventTimeFromJSON3
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
      assertEqual "EventTime ToJSON 1 1" "\"1000-1100, 1200-1300\"" et
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
