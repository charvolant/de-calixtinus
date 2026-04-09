{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell       #-}
{-|
Module      : Text.MessageCatalogue.Internal
Description : Message catalogue implementation
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Text.MessageCatalogue.Internal where

import Control.Exception (Exception, throw)
import Control.Monad (void, when)
import Control.Monad.Catch (throwM)
import qualified Data.ByteString as BS
import Data.Char (isLetter)
import Data.Default.Class (def)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Summary
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8Lenient)
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath (FilePath, stripExtension, (</>))
import qualified Text.Blaze.Internal as TB
import Text.Hamlet
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Shakespeare.I18N (Lang, RenderMessage, renderMessage)

-- | An message processing exception, usually the result of a parse error or incompatible message files
data MessageException = MessageException {
      meMsg :: String -- ^ The error text
    , meLang :: Maybe Lang -- ^ The language that caused this exception
    , meConstructor :: Maybe String -- ^ The underlying message constructor
    , meArg :: Maybe String -- ^ The message argument
  } deriving (Show)

instance Exception MessageException

data Param = Param {
    paName :: Name
  , paType :: Type
}

data Msg = Msg {
    msgConstructor :: String
  , msgParams :: [Param]
  , msgBody :: Text
}

data Catalogue = Catalogue {
    caLang :: Lang
  , caMsgs :: [Msg]
}

data MessageContext = MessageContext {
    mcRender :: Name
  , mcAppType :: Maybe Type
  , mcMsg :: Param
  , mcLocale :: Param
  , mcLocales :: Param
  , mcMarkupType :: Type
  , mcContext :: [Param]
  , mcBase :: Catalogue
}

mcRenderSig :: MessageContext -> Type
mcRenderSig ctx =
  foldr1 (\t -> \e -> ArrowT `AppT` t `AppT` e) (map paType (mcContext ctx) ++ [paType $ mcLocales ctx, paType $ mcMsg ctx, mcMarkupType ctx])

mcRender'Sig :: MessageContext -> Type
mcRender'Sig ctx =
  foldr1 (\t -> \e -> ArrowT `AppT` t `AppT` e) (map paType (mcContext ctx) ++ [paType $ mcLocales ctx, paType $ mcLocales ctx, paType $ mcMsg ctx, mcMarkupType ctx])

mcRenderLangSig :: MessageContext -> Type
mcRenderLangSig ctx =
  foldr1 (\t -> \e -> ArrowT `AppT` t `AppT` e) (map paType (mcContext ctx) ++ [paType $ mcLocales ctx, paType $ mcMsg ctx, mcMarkupType ctx])

mcLocaleRest :: MessageContext -> Name
mcLocaleRest ctx = mkName $ nameBase (paName $ mcLocale ctx) ++ "_r"

mcMsgTypeName :: MessageContext -> Name
mcMsgTypeName ctx = mcMsgTypeName' (paType $ mcMsg ctx)

mcMsgTypeName' (ConT n) = n
mcMsgTypeName' (AppT t _) = mcMsgTypeName' t
msMsgTypeName' _ = error "Can't get message type name"

htmlType :: Type
htmlType = ConT ''Text.Hamlet.Html

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

defaultBangQ :: Q Bang
defaultBangQ = return defaultBang

loadLang :: FilePath -> FilePath -> IO (Maybe Catalogue)
loadLang folder file = do
    let file' = folder </> file
    e <- doesFileExist file'
    let lang = stripExtension ".msg" file
    case (e, lang) of
      (True, Just lang') -> do
        bs <- BS.readFile file'
        let lang'' = T.pack lang'
        let s = decodeUtf8 bs
        let lines' = collectMessages (T.lines s) []
        defs <- mapM (parseMessage lang'') lines'
        return $ Just (Catalogue lang'' defs)
      _ -> return Nothing

collectMsg msg msgs = if null msg then msgs else (T.intercalate "\n" (reverse msg)):msgs

collectMessages [] msg = collectMsg msg []
collectMessages (line:rest) msg = case T.uncons line of
    Nothing -> collectMsg msg (collectMessages rest [])
    Just ('#', _) -> collectMessages rest msg
    Just (' ', _) -> collectMessages rest (line:msg)
    _ -> collectMsg msg (collectMessages rest [line])

parseMessage :: Lang -> Text -> IO Msg
parseMessage lang msg = do
  let (dec, body') = T.breakOn ":" msg
  let body = if T.null body' then throw $ MessageException ("No message constructor for " ++ T.unpack msg) (Just lang) Nothing Nothing else (T.strip $ T.drop 1 body')
  let (constructor, params) = parseConstructor lang (T.unpack dec)
  return $ Msg constructor params body

parseConstructor :: Lang -> String -> (String, [Param])
parseConstructor lang dec = either
  (\e -> throw $ MessageException ("Can't parse constructor " <> dec) (Just lang) Nothing Nothing)
  id
  (parse constructorParser "" dec)

parseType :: String -> Type
parseType dec = either
  (\e -> throw $ MessageException ("Can't parse type " <> dec) Nothing Nothing Nothing)
  id
  (parse typeDecParserTop "" dec)

whitespace :: Parser ()
whitespace = void $ many1 space

identifierParser :: Bool -> Parser String
identifierParser uc = do
  f <- if uc then upper else letter
  r <- many alphaNum
  return $ (f:r)

typeNameParser :: Parser Name
typeNameParser =
  try (do
    ns <- identifierParser True
    void $ char '.'
    nm <- identifierParser True
    return $ mkName $ ns ++ "." ++ nm
    )
  <|> (do
    nm <- identifierParser True
    return $ mkName nm
    )

typeDecParser6 :: Parser [Type]
typeDecParser6 =
  try (do
    void whitespace
    texp <- typeDecParserTop
    trest <- typeDecParser6
    return (texp:trest)
    )
  <|>
    return []

typeDecParser5 :: Parser Type
typeDecParser5 = do
  name1 <- typeNameParser
  types <- typeDecParser6
  void spaces
  return $ foldl (\ta -> \t -> ta `AppT` t) (ConT name1) types

typeDecParser4 :: Parser Type
typeDecParser4 = do
    void $ char '['
    texp <- typeDecParser1
    void $ char ']'
    void spaces
    return $ ListT `AppT` texp

typeDecParser3 :: Parser Type
typeDecParser3 = do
    void $ char '('
    texp <- typeDecParser1
    void $ char ')'
    void spaces
    return $ ParensT texp

typeDecParser2 :: Parser Type
typeDecParser2 =
  try typeDecParser3
  <|> try typeDecParser4
  <|> typeDecParser5

typeDecParser1 :: Parser Type
typeDecParser1 = do
    void spaces
    func1 <- typeDecParser2
    funcs <- many ((string "->" *> spaces) *> typeDecParser2)
    return $ foldr (\t1 -> \t2 -> ArrowT `AppT` t2 `AppT` t1) func1 funcs

-- Top-level Either a plain type name or a parenthesised expression
typeDecParserTop :: Parser Type
typeDecParserTop =
  try (do
    tname <- typeNameParser
    return $ ConT tname
    )
  <|> do
    void $ char '['
    void spaces
    texp <- typeDecParser1
    void $ char ']'
    return $ ListT `AppT` texp
  <|> do
    void $ char '('
    void spaces
    texp <- typeDecParser1
    void $ char ')'
    return $ ParensT texp

typeParser :: Parser Type
typeParser = do
  void $ char '@'
  typeDecParserTop

argParser :: Parser Param
argParser = do
  arg <- identifierParser False
  argType <- option WildCardT typeParser
  return $ Param (mkName arg) argType

constructorParser :: Parser (String, [Param])
constructorParser = do
  constructor <- identifierParser True
  void $ spaces
  args <- sepBy argParser whitespace
  return $ (constructor, args)

checkLang'' _lang _msg [] [] = do
  return ()
checkLang'' lang msg (dparam:drest) (lparam:lrest) =
  let
    merr = case (paType dparam, paType lparam) of
      (WildCardT, _) -> Just $ "Default message has no type"
      (dtype, WildCardT) -> Nothing
      (dtype, ltype) -> if dtype == ltype then Nothing else Just $ "Type mismatch from default"
  in
    case merr of
      Nothing -> checkLang'' lang msg drest lrest
      Just err -> throwM $ MessageException err (Just lang) (Just msg) (Just $ nameBase $ paName dparam)

checkLang' lang ddefs ldef = do
  let lmsg = msgConstructor ldef
  let ddef = ddefs M.! lmsg
  let dargs = msgParams ddef
  let largs = msgParams ldef
  when (length dargs /= length largs) (throwM $ MessageException "Mismatching parameter numbers" (Just lang) (Just lmsg) Nothing)
  checkLang'' lang lmsg dargs largs

checkLang dcat lcat = do
  let llang = caLang lcat
  let ddefs = caMsgs dcat
  let ldefs = caMsgs lcat
  let dcons = S.fromList $ map (T.pack . msgConstructor) ddefs
  let lcons = S.fromList $ map (T.pack . msgConstructor) ldefs
  let additional = S.difference lcons dcons
  when (not $ S.null additional) (throwM $ MessageException ("Additional message: " ++ summaryString additional) (Just llang) Nothing Nothing)
  let ddefs' = M.fromList $ map (\m -> (msgConstructor m, m)) ddefs
  mapM_ (checkLang' llang ddefs') ldefs

makeDataDec :: MessageContext -> Q [Dec]
makeDataDec ctx = do
  let catalogue = mcBase ctx
  constructors <- mapM (makeConstructor (caLang catalogue)) (caMsgs catalogue)
  return $ [
    DataD [] (mcMsgTypeName ctx) [] Nothing constructors [DerivClause Nothing [(ConT $ mkName "Eq"), (ConT $ mkName "Show")]]
    ]

makeConstructor :: Lang -> Msg -> Q Con
makeConstructor lang msg = do
  let name = mkName $ msgConstructor msg
  let pts = map paType $ msgParams msg
  when (any (== WildCardT) pts) (throw $ MessageException "Untyped parameter" (Just lang) (Just $ msgConstructor msg) Nothing)
  let pts' = map (\argType -> bangType defaultBangQ (return argType)) pts
  normalC name pts'

makeCatalogueDec :: MessageContext ->  Catalogue -> Q [Dec]
makeCatalogueDec ctx catalogue = do
  let fname = makeLangRenderName ctx (caLang catalogue)
      base = mcBase ctx
  clauses <- mapM (\msg -> makeMsgClause ctx (findMsg msg base) msg) (caMsgs catalogue)
  final <- makeNoMatchClause ctx (caLang base == caLang catalogue) catalogue
  return $ [FunD fname (clauses ++ [final])]

findMsg :: Msg -> Catalogue -> Msg
findMsg msg catalogue = maybe
  (throw $ MessageException "Can't find base constructor" (Just $ caLang catalogue) (Just $ msgConstructor msg) Nothing)
  id
  (L.find (\b -> msgConstructor b == msgConstructor msg) (caMsgs catalogue))

makeLangRenderName :: MessageContext -> Lang -> Name
makeLangRenderName ctx lang = mkName (nameBase (mcRender ctx) ++ "_" ++ makeLangTag lang)

makeLangTag :: Lang -> String
makeLangTag lang = T.unpack $ T.toUpper $ T.filter (\c -> isLetter c || c == '_') $ T.replace "-" "_" lang

makeMsgClause :: MessageContext -> Msg -> Msg -> Q Clause
makeMsgClause ctx base msg = do
  let just = 'Just
      context = map paName $ mcContext ctx
      constructor = mkName $ msgConstructor msg
      args = map (\a -> VarP $ paName a) $ msgParams msg
      langs = paName $ mcLocales ctx
      mpattern = ConP constructor [] args
      cpattern = (map VarP context) ++ [VarP langs, mpattern]
  msgExpr <- hamletFromString htmlRules defaultHamletSettings (T.unpack $ msgBody msg)
  let expr = (ConE just) `AppE` msgExpr
  return $ Clause cpattern (NormalB expr) []

makeNoMatchClause :: MessageContext -> Bool -> Catalogue -> Q Clause
makeNoMatchClause ctx base catalogue = do
  let context = map paName $ mcContext ctx
      msg = paName $ mcMsg ctx
      cpattern = (map (const WildP) context) ++ [WildP, VarP msg]
      expr = if base then
          AppE
            (VarE $ mkName "error")
            (InfixE
              (Just $ LitE $ StringL "Unrecognised message ")
              (VarE $ mkName "++")
              (Just $ AppE (VarE $ mkName "show") (VarE msg))
              )
        else
          ConE (mkName "Nothing")
  return $ Clause cpattern (NormalB expr) []

makeRenderDec :: MessageContext -> [Catalogue] -> Q [Dec]
makeRenderDec ctx catalogues = do
  let rname = mcRender ctx
      rtype = mcRenderSig ctx
      rname' = mkName $ nameBase rname ++ "'"
      rtype' = mcRender'Sig ctx
  mainClause <- makeMsgRenderMain ctx rname rname'
  nullClause <- makeMsgRenderNullClause ctx
  nfClause <- makeMsgRenderNotFoundClause ctx
  clauses <- mapM (makeMsgRenderClause ctx rname') catalogues
  return $ [SigD rname rtype, FunD rname [mainClause], SigD rname' rtype', FunD rname' ([nullClause] ++ clauses ++ [nfClause])]

makeMsgRenderMain :: MessageContext -> Name -> Name -> Q Clause
makeMsgRenderMain ctx rname rname' = do
  let msg = paName $ mcMsg ctx
      langs = paName $ mcLocales ctx
      context = map paName $ mcContext ctx
      cpattern = (map VarP context) ++ [VarP langs, VarP msg]
      expr = foldl1 AppE $ map VarE $ [rname'] ++ context ++ [langs, langs, msg]
  return $ Clause cpattern (NormalB expr) []

makeMsgRenderClause :: MessageContext -> Name -> Catalogue -> Q Clause
makeMsgRenderClause ctx rname catalogue = do
  let msg = paName $ mcMsg ctx
      lang = caLang catalogue
      langp = LitP $ StringL $ T.unpack lang
      rest = mcLocaleRest ctx
      langs = paName $ mcLocales ctx
      rlname = makeLangRenderName ctx lang
      context = map paName $ mcContext ctx
      cpattern = (map VarP context) ++ [InfixP langp (mkName ":") (VarP rest), VarP langs, VarP msg]
      rexpr = foldl1 AppE $ map VarE $ [rname] ++ context ++ [rest, langs, msg]
      expr = foldl1 AppE $ map VarE $ [rlname] ++ context ++ [langs, msg]
      mexpr = (VarE $ 'maybe) `AppE` rexpr `AppE` (VarE $ 'id) `AppE` expr
  return $ Clause cpattern (NormalB mexpr) []

makeMsgRenderNullClause :: MessageContext -> Q Clause
makeMsgRenderNullClause ctx = do
  let lang = caLang $ mcBase ctx
      msg = paName $ mcMsg ctx
      langs = paName $ mcLocales ctx
      rlname = makeLangRenderName ctx lang
      context = map paName $ mcContext ctx
      cpattern = (map VarP context) ++ [ListP [], VarP langs, VarP msg]
      expr = foldl1 AppE $ map VarE $ [rlname] ++ context ++ [langs, msg]
  return $ Clause cpattern (NormalB expr) []

makeMsgRenderNotFoundClause :: MessageContext -> Q Clause
makeMsgRenderNotFoundClause ctx = do
  let rname = mcRender ctx
      msg = paName $ mcMsg ctx
      rest = mcLocaleRest ctx
      langs = paName $ mcLocales ctx
      context = map paName $ mcContext ctx
      cpattern = (map VarP context) ++ [InfixP WildP (mkName ":") (VarP rest), VarP langs, VarP msg]
      rexpr = foldl1 AppE $ map VarE $ [rname] ++ context ++ [rest, langs, msg]
  return $ Clause cpattern(NormalB rexpr) []

makeInstanceDec :: MessageContext -> Q [Dec]
makeInstanceDec ctx = let
    defe = VarE $ 'def
    msgp = VarP $ paName $ mcMsg ctx
    langs = paName $ mcLocales ctx
    mmp = maybe Nothing (\mt -> L.find (\p -> paType p == mt) (mcContext ctx)) (mcAppType ctx) -- Matching arg to application type
    (mastert, masterp, exps) = maybe
      (VarT $ mkName "a", WildP, map (const defe) (mcContext ctx))
      (\mp -> (paType mp, VarP $ paName mp, map (\p -> if paName p == paName mp then VarE (paName p) else defe) (mcContext ctx)))
      mmp
    instancedec = InstanceD
      Nothing
      []
      (ConT ''RenderMessage `AppT` mastert `AppT` (paType $ mcMsg ctx))
      [
        FunD ('renderMessage) [
          Clause [masterp, VarP langs, msgp] (NormalB $ AppE (VarE 'renderMarkupToText) $ foldl AppE (VarE $ mcRender ctx) $ exps ++ [VarE langs, VarE $ paName $ mcMsg ctx]) []
        ]
      ]
  in
    return [instancedec]


renderMarkupToText :: TB.MarkupM a -> Text
renderMarkupToText (TB.Parent _ _ _ c)           = renderMarkupToText c
renderMarkupToText (TB.CustomParent _ c)         = renderMarkupToText c
renderMarkupToText (TB.Content s _)              = renderChoiceStringToText s
renderMarkupToText (TB.Append c1 c2)             = renderMarkupToText c1 <> renderMarkupToText c2
renderMarkupToText (TB.AddAttribute _ _ _ c)     = renderMarkupToText c
renderMarkupToText (TB.AddCustomAttribute _ _ c) = renderMarkupToText c
renderMarkupToText _                          = ""

renderChoiceStringToText :: TB.ChoiceString -> Text
renderChoiceStringToText (TB.Static ss) = TB.getText ss
renderChoiceStringToText (TB.String s) = T.pack s
renderChoiceStringToText (TB.Text t) = t
renderChoiceStringToText (TB.ByteString bs) = decodeUtf8Lenient bs
renderChoiceStringToText (TB.PreEscaped c) = renderChoiceStringToText c
renderChoiceStringToText (TB.External c) = renderChoiceStringToText c
renderChoiceStringToText (TB.AppendChoiceString c1 c2) = renderChoiceStringToText c1 <> renderChoiceStringToText c2
renderChoiceStringToText (TB.EmptyChoiceString) =""
