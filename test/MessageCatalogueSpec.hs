{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module MessageCatalogueSpec(testMessageCatalogue) where

import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Haskell.TH
import Test.HUnit
import Text.MessageCatalogue
import Text.MessageCatalogue.Internal
import Text.Parsec (parse)
import Text.RawString.QQ
import Text.Shakespeare.I18N (Lang)
import TestUtils

testMessageCatalogue :: Test
testMessageCatalogue = TestList [
    TestLabel "Collect Message" testCollectMessage
  , TestLabel "Read Langs" testReadLangs
  , TestLabel "Parse Message" testParseMessage
  , TestLabel "Check Language" testCheckLang
  , TestLabel "Make Constructor" testMakeConstructor
  , TestLabel "Make Data Dec" testMakeDataDec
  , TestLabel "Make Lang Tag" testMakeLangTag
  , TestLabel "Make Message Clause" testMakeMessageClause
  , TestLabel "Make Catalogue Dec" testMakeCatalogueDec
  , TestLabel "Make Render Dec" testMakeRenderDec
  , TestLabel "Make Message Catalogue Decs" testMkMessageCatalogue
 ]

  
testCollectMessage = TestList [
  testCollectMessage1, testCollectMessage2, testCollectMessage3, testCollectMessage4
  ]

testCollectMessage1 = TestCase (do
  let msgs = ["Tag1: Message1", "Tag2: Message2"]
  let collected = collectMessages msgs []
  assertEqual "Collect Message 1 1" 2 (length collected)
  assertEqual "Collect Message 1 2" "Tag1: Message1" (collected !! 0)
  assertEqual "Collect Message 1 3" "Tag2: Message2" (collected !! 1)
  )

testCollectMessage2 = TestCase (do
  let msgs = ["# Comment", "Tag1: Message1"]
  let collected = collectMessages msgs []
  assertEqual "Collect Message 2 1" 1 (length collected)
  assertEqual "Collect Message 2 2" "Tag1: Message1" (collected !! 0)
  )

testCollectMessage3 = TestCase (do
  let msgs = ["# Comment", "Tag1: Message1", " more message 1"]
  let collected = collectMessages msgs []
  assertEqual "Collect Message 2 1" 1 (length collected)
  assertEqual "Collect Message 2 2" "Tag1: Message1\n more message 1" (collected !! 0)
  )

testCollectMessage4 = TestCase (do
  let msgs = ["Tag1: Message1", " more message 1", "# Comment", " yet more message 1"]
  let collected = collectMessages msgs []
  assertEqual "Collect Message 4 1" 1 (length collected)
  assertEqual "Collect Message 4 2" "Tag1: Message1\n more message 1\n yet more message 1" (collected !! 0)
  )

testReadLangs = TestList [
  testReadLangs1, testReadLangs2, testReadLangs3, testReadLangs4
  ]

testReadLangs1 = TestCase (do
  entries <- runQ $ loadLang "./test/messages1" "en.msg"
  assertBool "Read Langs 1 1" (isJust entries)
  let ca = fromJust entries
  assertEqual "Read Langs 1 2" "en" (caLang ca)
  assertEqual "Read Langs 1 3" 2 (length $ caMsgs ca)
  )

testReadLangs2 = TestCase (do
  entries <- runQ $ loadLang "./test/messages1" "ez.msg"
  assertBool "Read Langs 2 1" (isNothing entries)
  )

testReadLangs3 = TestCase (do
  entries <- runQ $ loadLang "./test/messages1" "en.txt"
  assertBool "Read Langs 3 1" (isNothing entries)
  )

testReadLangs4 = TestCase (do
  entries <- runQ $ loadLang "./test/messages2" "pt.msg"
  assertBool "Read Langs 4 1" (isJust entries)
  let ca = fromJust entries
  assertEqual "Read Langs 4 2" "pt" (caLang ca)
  assertEqual "Read Langs 4 3" 2 (length $ caMsgs ca)
  let msg = caMsgs ca !! 1
  assertEqual "ReadLangs 4 4" (mkName "Descent") (msgConstructor msg)
  assertEqual "ReadLangs 4 4" "descens\227o <div .descent>\n  ^{formatHeight sou h}" (msgBody msg)
  )

ppArgs args = map (\(Param arg argType) -> (T.pack $ nameBase arg, T.strip $ T.pack $ pprint argType)) args

testParseMessage = TestList [
  testParseMessage1, testParseMessage2, testParseMessage3, testParseMessage4,
  testParseMessage5, testParseMessage6, testParseMessage7, testParseMessage8,
  testParseMessage9, testParseMessage10, testParseMessage11, testParseMessage12
  ]

testParseMessage1 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label1: Some text"
  assertEqual "Parse Message 1 1" (mkName "Label1") (msgConstructor msg)
  assertEqual "Parse Message 1 2" [] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 1 3" "Some text" (msgBody msg)
  )

testParseMessage2 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label1 :  Some text  "
  assertEqual "Parse Message 2 1" (mkName "Label1") (msgConstructor msg)
  assertEqual "Parse Message 2 2" [] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 2 3" "Some text" (msgBody msg)
  )

testParseMessage3 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label v: Some text"
  assertEqual "Parse Message 3 1" (mkName "Label") (msgConstructor msg)
  assertEqual "Parse Message 3 2" [("v", "_")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 3 3" "Some text" (msgBody msg)
  )

testParseMessage4 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label v@Int: Some text"
  assertEqual "Parse Message 4 1" (mkName "Label") (msgConstructor msg)
  assertEqual "Parse Message 4 2" [("v", "Int")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 4 3" "Some text" (msgBody msg)
  )

testParseMessage5 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label v@(Maybe Text): Some text"
  assertEqual "Parse Message 5 1" (mkName "Label")(msgConstructor msg)
  assertEqual "Parse Message 5 2" [("v", "(Maybe Text)")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 5 3" "Some text" (msgBody msg)
  )

testParseMessage6 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label v@( Maybe  Text ): Some text"
  assertEqual "Parse Message 6 1" (mkName "Label") (msgConstructor msg)
  assertEqual "Parse Message 6 2" [("v", "(Maybe Text)")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 6 3" "Some text" (msgBody msg)
  )

testParseMessage7 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label v1 v2: Some text"
  assertEqual "Parse Message 7 1" (mkName "Label") (msgConstructor msg)
  assertEqual "Parse Message 7 2" [("v1", "_"), ("v2", "_")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 7 3" "Some text" (msgBody msg)
  )

testParseMessage8 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label v1@Int v2@Float: Some text"
  assertEqual "Parse Message 8 1" (mkName "Label") (msgConstructor msg)
  assertEqual "Parse Message 8 2" [("v1", "Int"), ("v2", "Float")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 8 3" "Some text" (msgBody msg)
  )

testParseMessage9 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label v1@(M.Map String [Int]): Some text"
  assertEqual "Parse Message 9 1" (mkName "Label")(msgConstructor msg)
  assertEqual "Parse Message 9 2" [("v1", "(M.Map String [Int])")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 9 3" "Some text" (msgBody msg)
  )

testParseMessage10 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label v1@(Int -> Text): Some text"
  assertEqual "Parse Message 10 1" (mkName "Label")(msgConstructor msg)
  assertEqual "Parse Message 10 2" [("v1", "(Int -> Text)")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 10 3" "Some text" (msgBody msg)
  )

testParseMessage11 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label v1@(Int -> [Int -> String]): Some text"
  assertEqual "Parse Message 10 1" (mkName "Label") (msgConstructor msg)
  assertEqual "Parse Message 10 2" [("v1", "(Int -> [Int -> String])")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 10 3" "Some text" (msgBody msg)
  )

testParseMessage12 = TestCase (do
  msg <- runQ $ parseMessage "en" "Label v1@(Int -> Text) v2@([Int]): Some text"
  assertEqual "Parse Message 10 1" (mkName "Label") (msgConstructor msg)
  assertEqual "Parse Message 10 2" [("v1", "(Int -> Text)"), ("v2", "([Int])")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 10 3" "Some text" (msgBody msg)
  )

intType = ConT $ mkName "Int"
floatType = ConT $ mkName "Float"
floatListType = ListT `AppT` floatType
mapType = ConT $ mkName "M.Map"
intFloatMapType = mapType `AppT` intType `AppT` floatType

lang11 = Catalogue "en" [
    Msg (mkName "L1") [] "The message 1"
  , Msg (mkName "L2") [] "The message 2"
  ]

lang12 = Catalogue "fr" [
    Msg (mkName "L1") [] "Le message 1"
  , Msg (mkName "L2") [] "Le message 2"
  ]

lang13 = Catalogue "es" [
    Msg (mkName "L1") [] "El mensaje 1"
  , Msg (mkName "L2") [] "El mensaje 2"
  , Msg (mkName "L3") [] "El mensaje 3"
  ]

lang21 = Catalogue "en" [
     Msg (mkName "L1") [Param (mkName "x") intType] "The message #{x}"
   , Msg (mkName "L2") [Param (mkName "y") floatListType] "The message #{y}"
   ]

lang22 = Catalogue "fr" [
     Msg (mkName "L1") [Param (mkName "x") WildCardT] "Le #{x} message"
   , Msg (mkName "L2") [Param (mkName "y") floatListType] "Le message #{y}"
   ]

lang23 = Catalogue "es" [
     Msg (mkName "L1") [Param (mkName "x") floatType] "El mensaje #{x}"
   , Msg (mkName "L2") [Param (mkName "y") WildCardT] "El mensaje #{y}"
   ]

lang24 = Catalogue "pt" [
     Msg (mkName "L1") [Param (mkName "x") WildCardT, Param (mkName "y") WildCardT] "A mensagem #{x}"
   , Msg (mkName "L2") [Param (mkName "y") WildCardT] "A mensagem #{y}"
   ]

defaultMessageContext base = MessageContext {
      mcAppType = Just $ ConT $ mkName "TestApp"
    , mcRender = mkName "renderTestMessage"
    , mcMsg = Param (mkName "msg") (ConT $ mkName "TestMessage")
    , mcLocale = Param (mkName "lang") (ConT $ mkName "Lang")
    , mcLocales = Param (mkName "langs") (ListT `AppT` ConT ''Lang)
    , mcMarkupType = htmlType
    , mcContext = [Param (mkName "master") (ConT $ mkName "Test")]
    , mcBase = base
}

testCheckLang = TestList [
  testCheckLang1, testCheckLang2, testCheckLang3, testCheckLang4,
  testCheckLang5, testCheckLang6, testCheckLang7
  ]

testCheckLang1 = TestCase (do
  result <- runQ $ checkLang S.empty lang11 lang12
  assertBool "Check Lang 1" result
  )

testCheckLang2 = TestCase (do
  result <- runQ $ checkLang S.empty lang11 lang13
  assertBool "Check Lang 2" (not result)
  )


testCheckLang3 = TestCase (do
  result <- runQ $ checkLang S.empty lang21 lang22
  assertBool "Check Lang 3" result
  )

testCheckLang4 = TestCase (do
  result <- runQ $ checkLang S.empty lang22 lang21
  assertBool "Check Lang 4" (not result)
  )

testCheckLang5 = TestCase (do
  result <- runQ $ checkLang S.empty lang21 lang23
  assertBool "Check Lang 5" (not result)
  )

testCheckLang6 = TestCase (do
  result <- runQ $ checkLang S.empty lang21 lang24
  assertBool "Check Lang 6" (not result)
  )

testCheckLang7 = TestCase (do
  result <- runQ $ checkLang (S.singleton (mkName "x")) lang21 lang21
  assertBool "Check Lang 7" (not result)
  )

testMakeConstructor = TestList [
  testMakeConstructor1, testMakeConstructor2, testMakeConstructor3, testMakeConstructor4,
  testMakeConstructor5
  ]

testMakeConstructor1 = TestCase (do
  constructor <- runQ $ makeConstructor "en" $ Msg (mkName "L1") [] "A message"
  assertEqual "Make Constructor 1 1" "L1" (pprint constructor)
  )

testMakeConstructor2 = TestCase (do
  constructor <- runQ $ makeConstructor "en" $ Msg (mkName "L2") [Param (mkName "x") floatType] "A message"
  assertEqual "Make Constructor 2 1" "L2 Float" (pprint constructor)
  )

testMakeConstructor3 = TestCase (do
  constructor <- runQ $ makeConstructor "en" $ Msg (mkName "L3") [Param (mkName "x")  intFloatMapType] "A message"
  assertEqual "Make Constructor 3 1" "L3 (M.Map Int Float)" (pprint constructor)
  )

testMakeConstructor4 = TestCase (do
  let atype = either (\l -> error $ "Make Constructor 4 2:" ++ show l) id $ parse typeDecParserTop "" "(M.Map Int Float)"
  constructor <- runQ $ makeConstructor "en" $ Msg (mkName "L4") [Param (mkName "x") atype] "A message"
  assertEqual "Make Constructor 4 2" "L4 (M.Map Int Float)" (pprint constructor)
  )

testMakeConstructor5 = TestCase (do
  constructor <- runQ $ makeConstructor "en" $ Msg (mkName "L5") [Param (mkName "x") intType, Param (mkName "y") floatType] "A message"
  assertEqual "Make Constructor 3 1" "L5 Int Float" (pprint constructor)
  )

testMakeDataDec = TestList [
  testMakeDataDec1, testMakeDataDec2
  ]

testMakeDataDec1 = TestCase (do
  constructor <- runQ $ makeDataDec (defaultMessageContext lang11)
  assertEqualStripped "Make DataDec 1 1" "data TestMessage = L1 | L2 deriving (Eq, Show)" (pprint constructor)
  )

testMakeDataDec2 = TestCase (do
  constructor <- runQ $ makeDataDec (defaultMessageContext lang21)
  assertEqualStripped "Make DataDec 2 1" "data TestMessage = L1 Int | L2 [Float] deriving (Eq, Show)" (pprint constructor)
  )

testMakeLangTag = TestList [
  testMakeLangTag1, testMakeLangTag2
  ]

testMakeLangTag1 = TestCase $ assertEqual "Make LangTag 1" "ES" (makeLangTag "es")

testMakeLangTag2 = TestCase $ assertEqual "Make LangTag 2" "PT_BR" (makeLangTag "pt-BR")

testMakeMessageClause = TestList [
  testMakeMessageClause1, testMakeMessageClause2, testMakeMessageClause3, testMakeMessageClause4
  ]

clause1 = [r|
_ _ (Label1) = GHC.Internal.Maybe.Just (GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 9 "<binary data>")))
|]

testMakeMessageClause1 = TestCase (do
  let ctx = defaultMessageContext lang11
  msg <- runQ $ parseMessage "en" "Label1: Something"
  result <- runQ $ makeMsgClause ctx "en" (Just msg) msg
  assertEqualStripped "Make Message Clause 1" clause1 (pprint result)
  )

clause2 = [r|
_ _ (Label1) = GHC.Internal.Maybe.Nothing
|]

testMakeMessageClause2 = TestCase (do
  let ctx = defaultMessageContext lang11
  base <- runQ $ parseMessage "en" "Label1: Something"
  msg <- runQ $ parseMessage "es" "Label1: <open tag"
  result <- runQ $ makeMsgClause ctx "es" (Just base) msg
  assertEqualStripped "Make Message Clause 2" clause2 (pprint result)
  )

clause3 = [r|
_ _ (Label1 v) = GHC.Internal.Maybe.Just (do {GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 10 "<binary data>"));
                                              GHC.Internal.Base.id (Text.Blaze.Html.toHtml v)})
|]

testMakeMessageClause3 = TestCase (do
  let ctx = defaultMessageContext lang11
  msg <- runQ $ parseMessage "en" "Label1 v@Int: Something #{v}"
  result <- runQ $ makeMsgClause ctx "en" (Just msg) msg
  assertEqualStripped "Make Message Clause 3" clause3 (pprint result)
  )

clause4 = [r|
_ _ (Label1 _) = GHC.Internal.Maybe.Nothing
|]

testMakeMessageClause4 = TestCase (do
  let ctx = defaultMessageContext lang11
  base <- runQ $ parseMessage "en" "Label1 v@Int: Something"
  msg <- runQ $ parseMessage "es" "Label1 v: Something #{v"
  result <- runQ $ makeMsgClause ctx "es" (Just base) msg
  assertEqualStripped "Make Message Clause 4" clause4 (pprint result)
  )

testMakeCatalogueDec = TestList [
  testMakeCatalogueDec1, testMakeCatalogueDec2
  ]

cd1 = [r|
renderTestMessage_EN _ _ (L1) = GHC.Internal.Maybe.Just (GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 13 "<binary data>")))
renderTestMessage_EN _ _ (L2) = GHC.Internal.Maybe.Just (GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 13 "<binary data>")))
renderTestMessage_EN _ _ msg = error ("Unrecognised message " ++ GHC.Internal.Show.show msg)
|]

testMakeCatalogueDec1 = TestCase (do
  dec <- runQ $ makeCatalogueDec (defaultMessageContext lang11) lang11
  assertEqualStripped "Make CatalogueDec 1 1" cd1 (pprint dec)
  )

cd2 = [r|
renderTestMessage_FR _ _ (L1 x) = GHC.Internal.Maybe.Just (do {GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 3 "<binary data>"));
                                                               GHC.Internal.Base.id (Text.Blaze.Html.toHtml x);
                                                               GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 8 "<binary data>"))})
renderTestMessage_FR _ _ (L2 y) = GHC.Internal.Maybe.Just (do {GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 11 "<binary data>"));
                                                               GHC.Internal.Base.id (Text.Blaze.Html.toHtml y)})
renderTestMessage_FR _ _ _ = Nothing
|]

testMakeCatalogueDec2 = TestCase (do
  dec <- runQ $ makeCatalogueDec (defaultMessageContext lang21) lang22
  assertEqualStripped "Make CatalogueDec 2 1" cd2 (pprint dec)
  )
  
testMakeRenderDec = TestList [
  testMakeRenderDec1, testMakeRenderDec2
  ]

rd1 = [r|
renderTestMessage :: Test ->
                     [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage master langs msg = renderTestMessage' master langs langs msg
renderTestMessage' :: Test ->
                      [Text.Shakespeare.I18N.Lang] ->
                      [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage' master [] langs msg = GHC.Internal.Data.Maybe.maybe (Text.Blaze.Html.toHtml (GHC.Internal.Show.show msg)) GHC.Internal.Base.id (renderTestMessage_EN master langs msg)
renderTestMessage' master ("en" : lang_r) langs msg = GHC.Internal.Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Internal.Base.id (renderTestMessage_EN master langs msg)
renderTestMessage' master ("fr" : lang_r) langs msg = GHC.Internal.Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Internal.Base.id (renderTestMessage_FR master langs msg)
renderTestMessage' master (_ : lang_r) langs msg = renderTestMessage' master lang_r langs msg
|]

testMakeRenderDec1 = TestCase (do
  dec <- runQ $ makeRenderDec (defaultMessageContext lang11) [lang11, lang12]
  assertEqualStripped "Make RenderDec 1 1" rd1 (pprint dec)
  )

rd2 = [r|
renderTestMessage :: Test ->
                     [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage master langs msg = renderTestMessage' master langs langs msg
renderTestMessage' :: Test ->
                      [Text.Shakespeare.I18N.Lang] ->
                      [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage' master [] langs msg = GHC.Internal.Data.Maybe.maybe (Text.Blaze.Html.toHtml (GHC.Internal.Show.show msg)) GHC.Internal.Base.id (renderTestMessage_EN master langs msg)
renderTestMessage' master ("en" : lang_r) langs msg = GHC.Internal.Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Internal.Base.id (renderTestMessage_EN master langs msg)
renderTestMessage' master ("fr" : lang_r) langs msg = GHC.Internal.Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Internal.Base.id (renderTestMessage_FR master langs msg)
renderTestMessage' master ("es" : lang_r) langs msg = GHC.Internal.Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Internal.Base.id (renderTestMessage_ES master langs msg)
renderTestMessage' master (_ : lang_r) langs msg = renderTestMessage' master lang_r langs msg
|]

testMakeRenderDec2 = TestCase (do
  dec <- runQ $ makeRenderDec (defaultMessageContext lang21) [lang21, lang22, lang23]
  assertEqualStripped "Make RenderDec 2 1" rd2 (pprint dec)
  )


testMkMessageCatalogue = TestList [
  testMkMessageCatalogue1, testMkMessageCatalogue2
  ]

mk1 = [r|
data TestMessage
    = Label1
    | Label2
    deriving (Eq, Show)
renderTestMessage_EN _ _ (Label1) = GHC.Internal.Maybe.Just (GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 15 "<binary data>")))
renderTestMessage_EN _ _ (Label2) = GHC.Internal.Maybe.Just (GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 15 "<binary data>")))
renderTestMessage_EN _ _ msg = error ("Unrecognised message " ++ GHC.Internal.Show.show msg)
renderTestMessage_FR _ _ (Label1) = GHC.Internal.Maybe.Just (GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 16 "<binary data>")))
renderTestMessage_FR _ _ (Label2) = GHC.Internal.Maybe.Just (GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 16 "<binary data>")))
renderTestMessage_FR _ _ _ = Nothing
renderTestMessage :: Test ->
                     [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage master locales msg = renderTestMessage' master locales locales msg
renderTestMessage' :: Test ->
                      [Text.Shakespeare.I18N.Lang] ->
                      [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage' master [] locales msg = GHC.Internal.Data.Maybe.maybe (Text.Blaze.Html.toHtml (GHC.Internal.Show.show msg)) GHC.Internal.Base.id (renderTestMessage_EN master locales msg)
renderTestMessage' master ("en" : locale_r) locales msg = GHC.Internal.Data.Maybe.maybe (renderTestMessage' master locale_r locales msg) GHC.Internal.Base.id (renderTestMessage_EN master locales msg)
renderTestMessage' master ("fr" : locale_r) locales msg = GHC.Internal.Data.Maybe.maybe (renderTestMessage' master locale_r locales msg) GHC.Internal.Base.id (renderTestMessage_FR master locales msg)
renderTestMessage' master (_ : locale_r) locales msg = renderTestMessage' master locale_r locales msg
instance Text.Shakespeare.I18N.RenderMessage Test TestMessage
    where {Text.Shakespeare.I18N.renderMessage master langs msg = Text.MessageCatalogue.Internal.renderMarkupToText (renderTestMessage master locales msg)
                                                   where {locales = GHC.Internal.Base.map (GHC.Internal.Data.String.fromString GHC.Internal.Base.. Data.Text.Show.unpack) langs}}
|]

testMkMessageCatalogue1 = TestCase (do
  dec <- runQ $ mkMessageCatalogueSimple "Test" "./test/messages1" "en"
  assertEqualStripped "Make MkMessageCatalogue 1 1" mk1 (pprint dec)
  )

mk2 = [r|
data TestMsg
    = Ascent Float
    | Descent Float
    deriving (Eq, Show)
renderTestMsg_EN _ _ (Ascent h) = GHC.Internal.Maybe.Just (do {GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 29 "<binary data>"));
                                                               GHC.Internal.Base.id (Text.Blaze.Html.toHtml (sformat int h));
                                                               GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 7 "<binary data>"))})
renderTestMsg_EN _ _ (Descent h) = GHC.Internal.Maybe.Just (do {GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 31 "<binary data>"));
                                                                GHC.Internal.Base.id (Text.Blaze.Html.toHtml (sformat int h));
                                                                GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 7 "<binary data>"))})
renderTestMsg_EN _ _ msg = error ("Unrecognised message " ++ GHC.Internal.Show.show msg)
renderTestMsg_PT _ _ (Ascent h) = GHC.Internal.Maybe.Just (do {GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 32 "<binary data>"));
                                                               GHC.Internal.Base.id (Text.Blaze.Html.toHtml (sformat int h));
                                                               GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 7 "<binary data>"))})
renderTestMsg_PT sou _ (Descent h) = GHC.Internal.Maybe.Just (do {GHC.Internal.Base.id (Text.Blaze.Internal.unsafeByteString (Data.ByteString.Internal.Type.unsafePackLenLiteral 25 "<binary data>"));
                                                                  formatHeight sou h})
renderTestMsg_PT _ _ _ = Nothing
renderTestMsg :: SystemOfUnits ->
                 [Locale] -> TestMsg -> Text.Blaze.Html.Html
renderTestMsg sou locales msg = renderTestMsg' sou locales locales msg
renderTestMsg' :: SystemOfUnits ->
                  [Locale] -> [Locale] -> TestMsg -> Text.Blaze.Html.Html
renderTestMsg' sou [] locales msg = GHC.Internal.Data.Maybe.maybe (Text.Blaze.Html.toHtml (GHC.Internal.Show.show msg)) GHC.Internal.Base.id (renderTestMsg_EN sou locales msg)
renderTestMsg' sou ("en" : locale_r) locales msg = GHC.Internal.Data.Maybe.maybe (renderTestMsg' sou locale_r locales msg) GHC.Internal.Base.id (renderTestMsg_EN sou locales msg)
renderTestMsg' sou ("pt" : locale_r) locales msg = GHC.Internal.Data.Maybe.maybe (renderTestMsg' sou locale_r locales msg) GHC.Internal.Base.id (renderTestMsg_PT sou locales msg)
renderTestMsg' sou (_ : locale_r) locales msg = renderTestMsg' sou locale_r locales msg
instance Text.Shakespeare.I18N.RenderMessage a TestMsg
    where {Text.Shakespeare.I18N.renderMessage _ langs msg = Text.MessageCatalogue.Internal.renderMarkupToText (renderTestMsg Data.Default.Internal.def locales msg)
                                                   where {locales = GHC.Internal.Base.map (GHC.Internal.Data.String.fromString GHC.Internal.Base.. Data.Text.Show.unpack) langs}}
|]
testMkMessageCatalogue2 = TestCase (do
  dec <- runQ $ mkMessageCatalogue (mkName "TestMsg") (mkName "Locale") [("sou", mkName "SystemOfUnits")] "./test/messages2" "en" (Just $ mkName "TestApp")
  assertEqualStripped "Make MkMessageCatalogue 2 1" mk2 (pprint dec)
  )
