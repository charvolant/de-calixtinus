{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module MessageCatalogueSpec(testMessageCatalogue) where

import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Language.Haskell.TH
import Test.HUnit
import Text.MessageCatalogue
import Text.MessageCatalogue.Internal
import Text.Parsec (parse)
import Text.RawString.QQ
import Text.Shakespeare.I18N (Lang)
import TestUtils
import Codec.Xlsx (DynFilterType(DynFilterM1))

testMessageCatalogue :: Test
testMessageCatalogue = TestList [
    TestLabel "Collect Message" testCollectMessage
  , TestLabel "Read Langs" testReadLangs
  , TestLabel "Parse Message" testParseMessage
  , TestLabel "Check Language" testCheckLang
  , TestLabel "Make Constructor" testMakeConstructor
  , TestLabel "Make Data Dec" testMakeDataDec
  , TestLabel "Make Lang Tag" testMakeLangTag
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
  entries <- loadLang "./test/messages1" "en.msg"
  assertBool "Read Langs 1 1" (isJust entries)
  let ca = fromJust entries
  assertEqual "Read Langs 1 2" "en" (caLang ca)
  assertEqual "Read Langs 1 3" 2 (length $ caMsgs ca)
  )

testReadLangs2 = TestCase (do
  entries <- loadLang "./test/messages1" "ez.msg"
  assertBool "Read Langs 2 1" (isNothing entries)
  )

testReadLangs3 = TestCase (do
  entries <- loadLang "./test/messages1" "en.txt"
  assertBool "Read Langs 3 1" (isNothing entries)
  )

testReadLangs4 = TestCase (do
  entries <- loadLang "./test/messages2" "pt.msg"
  assertBool "Read Langs 4 1" (isJust entries)
  let ca = fromJust entries
  assertEqual "Read Langs 4 2" "pt" (caLang ca)
  assertEqual "Read Langs 4 3" 2 (length $ caMsgs ca)
  let msg = caMsgs ca !! 1
  assertEqual "ReadLangs 4 4" "Descent" (msgConstructor msg)
  assertEqual "ReadLangs 4 4" "descens\227o <div .descent>\n  ^{formatHeight sou h}" (msgBody msg)
  )

ppArgs args = map (\(Param arg argType) -> (T.pack $ nameBase arg, T.strip $ T.pack $ pprint argType)) args

testParseMessage = TestList [
  testParseMessage1, testParseMessage2, testParseMessage3, testParseMessage4,
  testParseMessage5, testParseMessage6, testParseMessage7, testParseMessage8,
  testParseMessage9, testParseMessage10, testParseMessage11, testParseMessage12
  ]

testParseMessage1 = TestCase (do
  msg <- parseMessage "en" "Label1: Some text"
  assertEqual "Parse Message 1 1" "Label1" (msgConstructor msg)
  assertEqual "Parse Message 1 2" [] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 1 3" "Some text" (msgBody msg)
  )

testParseMessage2 = TestCase (do
  msg <- parseMessage "en" "Label1 :  Some text  "
  assertEqual "Parse Message 2 1" "Label1" (msgConstructor msg)
  assertEqual "Parse Message 2 2" [] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 2 3" "Some text" (msgBody msg)
  )

testParseMessage3 = TestCase (do
  msg <- parseMessage "en" "Label v: Some text"
  assertEqual "Parse Message 3 1" "Label" (msgConstructor msg)
  assertEqual "Parse Message 3 2" [("v", "_")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 3 3" "Some text" (msgBody msg)
  )

testParseMessage4 = TestCase (do
  msg <- parseMessage "en" "Label v@Int: Some text"
  assertEqual "Parse Message 4 1" "Label" (msgConstructor msg)
  assertEqual "Parse Message 4 2" [("v", "Int")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 4 3" "Some text" (msgBody msg)
  )

testParseMessage5 = TestCase (do
  msg <- parseMessage "en" "Label v@(Maybe Text): Some text"
  assertEqual "Parse Message 5 1" "Label" (msgConstructor msg)
  assertEqual "Parse Message 5 2" [("v", "(Maybe Text)")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 5 3" "Some text" (msgBody msg)
  )

testParseMessage6 = TestCase (do
  msg <- parseMessage "en" "Label v@( Maybe  Text ): Some text"
  assertEqual "Parse Message 6 1" "Label" (msgConstructor msg)
  assertEqual "Parse Message 6 2" [("v", "(Maybe Text)")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 6 3" "Some text" (msgBody msg)
  )

testParseMessage7 = TestCase (do
  msg <- parseMessage "en" "Label v1 v2: Some text"
  assertEqual "Parse Message 7 1" "Label" (msgConstructor msg)
  assertEqual "Parse Message 7 2" [("v1", "_"), ("v2", "_")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 7 3" "Some text" (msgBody msg)
  )

testParseMessage8 = TestCase (do
  msg <- parseMessage "en" "Label v1@Int v2@Float: Some text"
  assertEqual "Parse Message 8 1" "Label" (msgConstructor msg)
  assertEqual "Parse Message 8 2" [("v1", "Int"), ("v2", "Float")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 8 3" "Some text" (msgBody msg)
  )

testParseMessage9 = TestCase (do
  msg <- parseMessage "en" "Label v1@(M.Map String [Int]): Some text"
  assertEqual "Parse Message 9 1" "Label" (msgConstructor msg)
  assertEqual "Parse Message 9 2" [("v1", "(M.Map String [Int])")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 9 3" "Some text" (msgBody msg)
  )

testParseMessage10 = TestCase (do
  msg <- parseMessage "en" "Label v1@(Int -> Text): Some text"
  assertEqual "Parse Message 10 1" "Label" (msgConstructor msg)
  assertEqual "Parse Message 10 2" [("v1", "(Int -> Text)")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 10 3" "Some text" (msgBody msg)
  )

testParseMessage11 = TestCase (do
  msg <- parseMessage "en" "Label v1@(Int -> [Int -> String]): Some text"
  assertEqual "Parse Message 10 1" "Label" (msgConstructor msg)
  assertEqual "Parse Message 10 2" [("v1", "(Int -> [Int -> String])")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 10 3" "Some text" (msgBody msg)
  )

testParseMessage12 = TestCase (do
  msg <- parseMessage "en" "Label v1@(Int -> Text) v2@([Int]): Some text"
  assertEqual "Parse Message 10 1" "Label" (msgConstructor msg)
  assertEqual "Parse Message 10 2" [("v1", "(Int -> Text)"), ("v2", "([Int])")] (ppArgs $ msgParams msg)
  assertEqual "Parse Message 10 3" "Some text" (msgBody msg)
  )

intType = ConT $ mkName "Int"
floatType = ConT $ mkName "Float"
floatListType = ListT `AppT` floatType
mapType = ConT $ mkName "M.Map"
intFloatMapType = mapType `AppT` intType `AppT` floatType

lang11 = Catalogue "en" [
    Msg "L1" [] "The message 1"
  , Msg "L2" [] "The message 2"
  ]

lang12 = Catalogue "fr" [
    Msg "L1" [] "Le message 1"
  , Msg "L2" [] "Le message 2"
  ]

lang13 = Catalogue "es" [
    Msg "L1" [] "El mensaje 1"
  , Msg "L2" [] "El mensaje 2"
  , Msg "L3" [] "El mensaje 3"
  ]

lang21 = Catalogue "en" [
     Msg "L1" [Param (mkName "x") intType] "The message #{x}"
   , Msg "L2" [Param (mkName "y") floatListType] "The message #{y}"
   ]

lang22 = Catalogue "fr" [
     Msg "L1" [Param (mkName "x") WildCardT] "Le #{x} message"
   , Msg "L2" [Param (mkName "y") floatListType] "Le message #{y}"
   ]

lang23 = Catalogue "es" [
     Msg "L1" [Param (mkName "x") floatType] "El mensaje #{x}"
   , Msg "L2" [Param (mkName "y") WildCardT] "El mensaje #{y}"
   ]

lang24 = Catalogue "pt" [
     Msg "L1" [Param (mkName "x") WildCardT, Param (mkName "y") WildCardT] "A mensagem #{x}"
   , Msg "L2" [Param (mkName "y") WildCardT] "A mensagem #{y}"
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

extractException :: Either SomeException a -> IO MessageException
extractException result = do
  let mex = fromException (either id (\_ -> error "Expecting Left") result)
  assertBool "No MessageException" (isJust mex)
  return $ fromJust mex

testCheckLang = TestList [
  testCheckLang1, testCheckLang2, testCheckLang3, testCheckLang4,
  testCheckLang5, testCheckLang6
  ]

testCheckLang1 = TestCase (do
  checkLang lang11 lang12
  )

testCheckLang2 = TestCase (do
  result <- (try $ checkLang lang11 lang13) :: IO (Either SomeException ())
  assertBool "Check Lang 2 1" (isLeft result)
  ex <- extractException result
  assertEqual "Check Lang 2 2" "Additional message: {L3}" (meMsg ex)
  assertEqual "Check Lang 2 3" (Just "es") (meLang ex)
  assertEqual "Check Lang 2 4" Nothing (meConstructor ex)
  assertEqual "Check Lang 2 5" Nothing (meArg ex)
  )


testCheckLang3 = TestCase (do
  checkLang lang21 lang22
  )

testCheckLang4 = TestCase (do
  result <- (try $ checkLang lang22 lang21) :: IO (Either SomeException ())
  assertBool "Check Lang 4 1" (isLeft result)
  ex <- extractException result
  assertEqual "Check Lang 4 2" "Default message has no type" (meMsg ex)
  assertEqual "Check Lang 4 3" (Just "en") (meLang ex)
  assertEqual "Check Lang 4 4" (Just "L1") (meConstructor ex)
  assertEqual "Check Lang 4 5" (Just "x") (meArg ex)
  )

testCheckLang5 = TestCase (do
  result <- (try $ checkLang lang21 lang23) :: IO (Either SomeException ())
  assertBool "Check Lang 5 1" (isLeft result)
  ex <- extractException result
  assertEqual "Check Lang 5 2" "Type mismatch from default" (meMsg ex)
  assertEqual "Check Lang 5 3" (Just "es") (meLang ex)
  assertEqual "Check Lang 5 4" (Just "L1") (meConstructor ex)
  assertEqual "Check Lang 5 5" (Just "x") (meArg ex)
  )

testCheckLang6 = TestCase (do
  result <- (try $ checkLang lang21 lang24) :: IO (Either SomeException ())
  assertBool "Check Lang 6 1" (isLeft result)
  ex <- extractException result
  assertEqual "Check Lang 6 2" "Mismatching parameter numbers" (meMsg ex)
  assertEqual "Check Lang 6 3" (Just "pt") (meLang ex)
  assertEqual "Check Lang 6 4" (Just "L1") (meConstructor ex)
  assertEqual "Check Lang 6 5" Nothing (meArg ex)
  )

testMakeConstructor = TestList [
  testMakeConstructor1, testMakeConstructor2, testMakeConstructor3, testMakeConstructor4,
  testMakeConstructor5
  ]

testMakeConstructor1 = TestCase (do
  constructor <- runQ $ makeConstructor "en" $ Msg "L1" [] "A message"
  assertEqual "Make Constructor 1 1" "L1" (pprint constructor)
  )

testMakeConstructor2 = TestCase (do
  constructor <- runQ $ makeConstructor "en" $ Msg "L2" [Param (mkName "x") floatType] "A message"
  assertEqual "Make Constructor 2 1" "L2 Float" (pprint constructor)
  )

testMakeConstructor3 = TestCase (do
  constructor <- runQ $ makeConstructor "en" $ Msg "L3" [Param (mkName "x")  intFloatMapType] "A message"
  assertEqual "Make Constructor 3 1" "L3 (M.Map Int Float)" (pprint constructor)
  )

testMakeConstructor4 = TestCase (do
  let atype = either (\l -> error $ "Make Constructor 4 2:" ++ show l) id $ parse typeDecParserTop "" "(M.Map Int Float)"
  constructor <- runQ $ makeConstructor "en" $ Msg "L4" [Param (mkName "x") atype] "A message"
  assertEqual "Make Constructor 4 2" "L4 (M.Map Int Float)" (pprint constructor)
  )

testMakeConstructor5 = TestCase (do
  constructor <- runQ $ makeConstructor "en" $ Msg "L5" [Param (mkName "x") intType, Param (mkName "y") floatType] "A message"
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

testMakeCatalogueDec = TestList [
  testMakeCatalogueDec1, testMakeCatalogueDec2
  ]

cd1 = [r|renderTestMessage_EN master langs (L1) = GHC.Maybe.Just (GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "The message 1"))
renderTestMessage_EN master langs (L2) = GHC.Maybe.Just (GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "The message 2"))
renderTestMessage_EN _ _ msg = error ("Unrecognised message " ++ show msg)|]

testMakeCatalogueDec1 = TestCase (do
  dec <- runQ $ makeCatalogueDec (defaultMessageContext lang11) lang11
  assertEqual "Make CatalogueDec 1 1" cd1 (pprint dec)
  )

cd2 = [r|renderTestMessage_FR master langs (L1 x) = GHC.Maybe.Just (do {GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "Le ");
                                                               GHC.Base.id (Text.Blaze.Html.toHtml x);
                                                               GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) " message")})
renderTestMessage_FR master langs (L2 y) = GHC.Maybe.Just (do {GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "Le message ");
                                                               GHC.Base.id (Text.Blaze.Html.toHtml y)})
renderTestMessage_FR _ _ msg = Nothing|]

testMakeCatalogueDec2 = TestCase (do
  dec <- runQ $ makeCatalogueDec (defaultMessageContext lang21) lang22
  assertEqualStripped "Make CatalogueDec 2 1" cd2 (pprint dec)
  )
  
testMakeRenderDec = TestList [
  testMakeRenderDec1, testMakeRenderDec2
  ]

rd1 = [r|renderTestMessage :: Test ->
                     [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage master langs msg = renderTestMessage' master langs langs msg
renderTestMessage' :: Test ->
                      [Text.Shakespeare.I18N.Lang] ->
                      [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage' master [] langs msg = Data.Maybe.maybe "XXX" GHC.Base.id (renderTestMessage_EN master langs msg)
renderTestMessage' master ("en" : lang_r) langs msg = Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Base.id (renderTestMessage_EN master langs msg)
renderTestMessage' master ("fr" : lang_r) langs msg = Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Base.id (renderTestMessage_FR master langs msg)
renderTestMessage' master (_ : lang_r) langs msg = renderTestMessage' master lang_r langs msg|]

testMakeRenderDec1 = TestCase (do
  dec <- runQ $ makeRenderDec (defaultMessageContext lang11) [lang11, lang12]
  assertEqual "Make RenderDec 1 1" rd1 (pprint dec)
  )

rd2 = [r|renderTestMessage :: Test ->
                     [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage master langs msg = renderTestMessage' master langs langs msg
renderTestMessage' :: Test ->
                      [Text.Shakespeare.I18N.Lang] ->
                      [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage' master [] langs msg = Data.Maybe.maybe "XXX" GHC.Base.id (renderTestMessage_EN master langs msg)
renderTestMessage' master ("en" : lang_r) langs msg = Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Base.id (renderTestMessage_EN master langs msg)
renderTestMessage' master ("fr" : lang_r) langs msg = Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Base.id (renderTestMessage_FR master langs msg)
renderTestMessage' master ("es" : lang_r) langs msg = Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Base.id (renderTestMessage_ES master langs msg)
renderTestMessage' master (_ : lang_r) langs msg = renderTestMessage' master lang_r langs msg|]

testMakeRenderDec2 = TestCase (do
  dec <- runQ $ makeRenderDec (defaultMessageContext lang21) [lang21, lang22, lang23]
  assertEqualStripped "Make RenderDec 2 1" rd2 (pprint dec)
  )


testMkMessageCatalogue = TestList [
  testMkMessageCatalogue1, testMkMessageCatalogue2
  ]

mk1 = [r|data TestMessage
    = Label1
    | Label2
    deriving (Eq, Show)
renderTestMessage_EN master langs (Label1) = GHC.Maybe.Just (GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "This is label 1"))
renderTestMessage_EN master langs (Label2) = GHC.Maybe.Just (GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "This is label 2"))
renderTestMessage_EN _ _ msg = error ("Unrecognised message " ++ show msg)
renderTestMessage_FR master langs (Label1) = GHC.Maybe.Just (GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "C'est le signe 1"))
renderTestMessage_FR master langs (Label2) = GHC.Maybe.Just (GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "C'est le signe 2"))
renderTestMessage_FR _ _ msg = Nothing
renderTestMessage :: Test ->
                     [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage master langs msg = renderTestMessage' master langs langs msg
renderTestMessage' :: Test ->
                      [Text.Shakespeare.I18N.Lang] ->
                      [Text.Shakespeare.I18N.Lang] -> TestMessage -> Text.Blaze.Html.Html
renderTestMessage' master [] langs msg = Data.Maybe.maybe "XXX" GHC.Base.id (renderTestMessage_EN master langs msg)
renderTestMessage' master ("en" : lang_r) langs msg = Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Base.id (renderTestMessage_EN master langs msg)
renderTestMessage' master ("fr" : lang_r) langs msg = Data.Maybe.maybe (renderTestMessage' master lang_r langs msg) GHC.Base.id (renderTestMessage_FR master langs msg)
renderTestMessage' master (_ : lang_r) langs msg = renderTestMessage' master lang_r langs msg
instance Text.Shakespeare.I18N.RenderMessage Test TestMessage
    where {Text.Shakespeare.I18N.renderMessage master langs msg = Text.MessageCatalogue.Internal.renderMarkupToText (renderTestMessage master langs msg)}|]

testMkMessageCatalogue1 = TestCase (do
  dec <- runQ $ mkMessageCatalogueSimple "Test" "./test/messages1" "en"
  assertEqual "Make MkMessageCatalogue 1 1" mk1 (pprint dec)
  )

mk2 = [r|data TestMsg
    = Ascent Float
    | Descent Float
    deriving (Eq, Show)
renderTestMsg_EN sou langs (Ascent h) = GHC.Maybe.Just (do {GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "Ascent <span descent .height>");
                                                            GHC.Base.id (Text.Blaze.Html.toHtml (sformat int h));
                                                            GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "</span>")})
renderTestMsg_EN sou langs (Descent h) = GHC.Maybe.Just (do {GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "Descent <span .descent .height>");
                                                             GHC.Base.id (Text.Blaze.Html.toHtml (sformat int h));
                                                             GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "</span>")})
renderTestMsg_EN _ _ msg = error ("Unrecognised message " ++ show msg)
renderTestMsg_PT sou langs (Ascent h) = GHC.Maybe.Just (do {GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "ascens\227o <span descent .height>");
                                                            GHC.Base.id (Text.Blaze.Html.toHtml (sformat int h));
                                                            GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "</span>")})
renderTestMsg_PT sou langs (Descent h) = GHC.Maybe.Just (do {GHC.Base.id ((Text.Blaze.Internal.preEscapedText GHC.Base.. Data.Text.Internal.pack) "descens\227o <div .descent>");
                                                             formatHeight sou h})
renderTestMsg_PT _ _ msg = Nothing
renderTestMsg :: SystemOfUnits ->
                 [Locale] -> TestMsg -> Text.Blaze.Html.Html
renderTestMsg sou langs msg = renderTestMsg' sou langs langs msg
renderTestMsg' :: SystemOfUnits ->
                  [Locale] -> [Locale] -> TestMsg -> Text.Blaze.Html.Html
renderTestMsg' sou [] langs msg = Data.Maybe.maybe "XXX" GHC.Base.id (renderTestMsg_EN sou langs msg)
renderTestMsg' sou ("en" : lang_r) langs msg = Data.Maybe.maybe (renderTestMsg' sou lang_r langs msg) GHC.Base.id (renderTestMsg_EN sou langs msg)
renderTestMsg' sou ("pt" : lang_r) langs msg = Data.Maybe.maybe (renderTestMsg' sou lang_r langs msg) GHC.Base.id (renderTestMsg_PT sou langs msg)
renderTestMsg' sou (_ : lang_r) langs msg = renderTestMsg' sou lang_r langs msg
instance Text.Shakespeare.I18N.RenderMessage a TestMsg
    where {Text.Shakespeare.I18N.renderMessage _ langs msg = Text.MessageCatalogue.Internal.renderMarkupToText (renderTestMsg Data.Default.Class.def langs msg)}|]

testMkMessageCatalogue2 = TestCase (do
  dec <- runQ $ mkMessageCatalogue Nothing (mkName "TestMsg") (mkName "Locale") [("sou", mkName "SystemOfUnits")] "./test/messages2" "en" True
  assertEqual "Make MkMessageCatalogue 2 1" mk2 (pprint dec)
  )
