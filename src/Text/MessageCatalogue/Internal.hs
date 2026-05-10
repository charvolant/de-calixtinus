{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_HADDOCK hide #-}
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

import qualified Control.Exception as CE
import Control.Monad (void, when)
import qualified Data.ByteString as BS
import Data.Char (isLetter)
import Data.Default.Class (def)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String
import Data.Summary
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8Lenient)
import Language.Haskell.TH
import Language.Haskell.TH.Variables
import System.Directory (doesFileExist)
import System.FilePath (stripExtension, (</>))
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Internal as TB
import Text.Hamlet
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Shakespeare.I18N (Lang, RenderMessage, renderMessage)

data Param = Param {
    paName :: Name
  , paType :: Type
} deriving (Show)

data Msg = Msg {
    msgConstructor :: Name
  , msgParams :: [Param]
  , msgBody :: Text
} deriving (Show)

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

-- Create a detailed error message, since the error could be anywhere in the message file
msgError :: String -> Maybe Lang -> Maybe Name -> Maybe Name -> Maybe Text -> Q ()
msgError msg mlang mcons mparam mbody =
  reportError $ msg
    ++ maybe "" (\l -> "\n  lang=" ++ T.unpack l) mlang
    ++ maybe "" (\c -> "\n  message=" ++ show c) mcons
    ++ maybe "" (\p -> "\n  param=" ++ show p) mparam
    ++ maybe "" (\b -> "\n  body=" ++ T.unpack b) mbody

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

mcMsgTypeName' :: Type -> Name
mcMsgTypeName' (ConT n) = n
mcMsgTypeName' (AppT t _) = mcMsgTypeName' t
mcMsgTypeName' _ = error "Can't get message type name"

htmlType :: Type
htmlType = ConT ''Text.Hamlet.Html

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

defaultBangQ :: Q Bang
defaultBangQ = return defaultBang

loadLang :: FilePath -> FilePath -> Q (Maybe Catalogue)
loadLang folder file = do
    let file' = folder </> file
    e <- runIO $ doesFileExist file'
    let lang = stripExtension ".msg" file
    case (e, lang) of
      (True, Just lang') -> do
        bs <- runIO $ BS.readFile file'
        let lang'' = T.pack lang'
        let s = decodeUtf8 bs
        let lines' = collectMessages (T.lines s) []
        defs <- mapM (parseMessage lang'') lines'
        return $ Just (Catalogue lang'' defs)
      _ -> return Nothing

collectMsg :: [Text] -> [Text] -> [Text]
collectMsg msg msgs = if null msg then msgs else (T.intercalate "\n" (reverse msg)):msgs

collectMessages :: [Text] -> [Text] -> [Text]
collectMessages [] msg = collectMsg msg []
collectMessages (line:rest) msg = case T.uncons line of
    Nothing -> collectMessages rest (line:msg)
    Just ('#', line') -> case T.uncons line' of
      Just ('{', _) -> collectMessages rest (line:msg) -- Start of expression
      _ -> collectMessages rest msg -- Comment
    Just (' ', _) -> collectMessages rest (line:msg)
    Just ('$', _) -> collectMessages rest (line:msg)
    Just ('<', _) -> collectMessages rest (line:msg)
    Just ('^', _) -> collectMessages rest (line:msg)
    Just ('_', _) -> collectMessages rest (line:msg)
    _ -> collectMsg msg (collectMessages rest [line]) -- Start of a new message

parseMessage :: Lang -> Text -> Q Msg
parseMessage lang msg = do
  let (dec', body') = T.breakOn ":" msg
  (dec, body) <- if T.null body' then do
      msgError "No message constructor found" (Just lang) Nothing Nothing (Just msg)
      return ("Error", "")
    else
      return (dec', T.strip $ T.drop 1 body')
  (constructor, params) <- parseConstructor lang (T.unpack dec)
  return $ Msg constructor params body

parseConstructor :: Lang -> String -> Q (Name, [Param])
parseConstructor lang dec = do
  case parse constructorParser "" dec of
    Left e -> do
      msgError (show e) (Just lang) Nothing Nothing (Just $ T.pack dec)
      constructor <- newName "Error"
      return (constructor, [])
    Right sp -> return sp

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

constructorParser :: Parser (Name, [Param])
constructorParser = do
  constructor <- identifierParser True
  void $ spaces
  args <- sepBy argParser whitespace
  return $ (mkName constructor, args)

checkLang'' :: S.Set Name -> Lang -> Name -> [Param] -> [Param] -> Q Bool
checkLang'' _reserved _lang _msg [] [] = do
  return True
checkLang'' reserved lang msg (dparam:drest) (lparam:lrest) =
  let
    merr = case (paType dparam, paType lparam) of
      (WildCardT, _) -> Just $ "Default message has no type"
      (_dtype, WildCardT) -> Nothing
      (dtype, ltype) -> if dtype == ltype then Nothing else Just $ "Type mismatch from default"
    perr = if S.member (paName lparam) reserved then Just $ "Parameter name is reserved" else Nothing
    errs = case (merr, perr) of
      (Nothing, Nothing) -> Nothing
      (e@(Just _), Nothing) -> e
      (Nothing, e@(Just _)) -> e
      (Just e1, Just e2) -> Just $ e1 ++ " " ++ e2
  in
    case errs of
      (Nothing) -> checkLang'' reserved lang msg drest lrest
      (Just err) -> do
        msgError err (Just lang) (Just msg) (Just $ paName dparam) Nothing
        return False
checkLang'' _reserved lang msg _ _ = do
  msgError "Unexpected mispatch in parameeter numbers" (Just lang) (Just msg) Nothing Nothing
  return False

checkLang' :: S.Set Name -> Lang -> M.Map Name Msg -> Msg -> Q Bool
checkLang' reserved lang ddefs ldef = do
  let lmsg = msgConstructor ldef
  case M.lookup lmsg ddefs of
    Nothing -> do
      msgError "Can't find default message" (Just lang) (Just lmsg) Nothing Nothing
      return False
    Just ddef -> do
      let dargs = msgParams ddef
          largs = msgParams ldef
      if length dargs /= length largs then do
          msgError "Mismatching parameter numbers" (Just lang) (Just lmsg) Nothing Nothing
          return False
        else
          checkLang'' reserved lang lmsg dargs largs

checkLang :: S.Set Name -> Catalogue -> Catalogue -> Q Bool
checkLang reserved dcat lcat = do
  let llang = caLang lcat
  let ddefs = caMsgs dcat
  let ldefs = caMsgs lcat
  let dcons = S.fromList $ map msgConstructor ddefs
  let lcons = S.fromList $ map msgConstructor ldefs
  let additional = S.difference lcons dcons
  ok1 <- if S.null additional then
      return True
    else do
      msgError ("Additional messages: " ++ summaryString (S.map (T.pack . show) additional)) (Just llang) Nothing Nothing Nothing
      return False
  let ddefs' = M.fromList $ map (\m -> (msgConstructor m, m)) ddefs
  ok <- mapM (checkLang' reserved llang ddefs') ldefs
  return $ and (ok1:ok)

-- | Ensure context is acceptable
checkMessageDeclaration :: MessageContext -> [Catalogue] -> Q Bool
checkMessageDeclaration ctx catalogues = do
  let base = S.fromList [mcRender ctx, paName $ mcMsg ctx, paName $ mcLocale ctx, paName $ mcLocales ctx, mcLocaleRest ctx, mkName "langs", mkName "msg"]
  let context = map paName (mcContext ctx)
  ok <- mapM (\n -> do
    if S.member n base then do
      msgError "Duplicate context name" Nothing Nothing (Just n) Nothing
      return False
    else
      return True
    ) context
  let reserved = S.union base (S.fromList context)
  okl <- mapM (checkLang reserved (mcBase ctx)) catalogues
  return $ and (ok ++ okl)

makeDataDec :: MessageContext -> Q [Dec]
makeDataDec ctx = do
  let catalogue = mcBase ctx
  constructors <- mapM (makeConstructor (caLang catalogue)) (caMsgs catalogue)
  return $ [
    DataD [] (mcMsgTypeName ctx) [] Nothing constructors [DerivClause Nothing [(ConT $ mkName "Eq"), (ConT $ mkName "Show")]]
    ]

makeConstructor :: Lang -> Msg -> Q Con
makeConstructor lang msg = do
  let name = msgConstructor msg
  let pts = map paType $ msgParams msg
  when (any (== WildCardT) pts) (msgError "Untyped parameter" (Just lang) (Just $ msgConstructor msg) Nothing Nothing)
  let pts' = map (\argType -> bangType defaultBangQ (return argType)) pts
  normalC name pts'

makeCatalogueDec :: MessageContext ->  Catalogue -> Q [Dec]
makeCatalogueDec ctx catalogue = do
  let lang = caLang catalogue
      fname = makeLangRenderName ctx lang
      base = mcBase ctx
  clauses <- mapM (\msg -> makeMsgClause ctx lang (findMsg msg base) msg) (caMsgs catalogue)
  final <- makeNoMatchClause ctx (caLang base == caLang catalogue) catalogue
  return $ [FunD fname (clauses ++ [final])]

findMsg :: Msg -> Catalogue -> Maybe Msg
findMsg msg catalogue = L.find (\b -> msgConstructor b == msgConstructor msg) (caMsgs catalogue)

makeLangRenderName :: MessageContext -> Lang -> Name
makeLangRenderName ctx lang = mkName (nameBase (mcRender ctx) ++ "_" ++ makeLangTag lang)

makeLangTag :: Lang -> String
makeLangTag lang = T.unpack $ T.toUpper $ T.filter (\c -> isLetter c || c == '_') $ T.replace "-" "_" lang

-- Dear God this is messy
parseMsgBody :: String -> Q (Either CE.SomeException Exp)
parseMsgBody body = runIO $ CE.try $ runQ $ hamletFromString htmlRules defaultHamletSettings body

mkClause :: MessageContext -> Msg -> Maybe Exp -> Q Clause
mkClause ctx msg mexpr = do
  let vars = maybe S.empty unboundVarsExp mexpr
      varp n = if S.member n vars then VarP n else WildP
      context = map (varp . paName) $ mcContext ctx
      constructor = msgConstructor msg
      args = map (varp . paName) $ msgParams msg
      locales = varp $ paName $ mcLocales ctx
      mpattern = ConP constructor [] args
      cpattern = context ++ [locales, mpattern]
      body = maybe (ConE 'Nothing) (\e -> ConE 'Just `AppE` e) mexpr
  return $ Clause cpattern (NormalB body) []

makeMsgClause :: MessageContext -> Lang -> Maybe Msg -> Msg -> Q Clause
makeMsgClause ctx lang Nothing msg = do
  msgError "Can't find base message" (Just lang) (Just $ msgConstructor msg) Nothing Nothing
  mkClause ctx msg Nothing
makeMsgClause ctx lang _mbase msg = do
  let body = T.unpack $ msgBody msg
  eexpr <- parseMsgBody body
  case eexpr of
    Left e -> do
      msgError ("Unable to parse body: " ++ CE.displayException e) (Just lang) (Just $ msgConstructor msg) Nothing (Just $ msgBody msg)
      mkClause ctx msg Nothing
    Right expr ->
      mkClause ctx msg (Just expr)

makeNoMatchClause :: MessageContext -> Bool -> Catalogue -> Q Clause
makeNoMatchClause ctx base _catalogue = do
  let context = map paName $ mcContext ctx
      msg = paName $ mcMsg ctx
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
      vars = unboundVarsExp expr
      varp n = if S.member n vars then VarP n else WildP
      cpattern = (map (const WildP) context) ++ [WildP, varp msg]
  return $ Clause cpattern (NormalB expr) []

makeRenderDec :: MessageContext -> [Catalogue] -> Q [Dec]
makeRenderDec ctx catalogues = do
  let rname = mcRender ctx
      rtype = mcRenderSig ctx
      rname' = mkName $ nameBase rname ++ "'"
      rtype' = mcRender'Sig ctx
  mainClause <- makeMsgRenderMain ctx rname rname'
  nullClause <- makeMsgRenderNullClause ctx
  nfClause <- makeMsgRenderNotFoundClause ctx rname'
  clauses <- mapM (makeMsgRenderClause ctx rname') catalogues
  return $ [SigD rname rtype, FunD rname [mainClause], SigD rname' rtype', FunD rname' ([nullClause] ++ clauses ++ [nfClause])]

makeMsgRenderMain :: MessageContext -> Name -> Name -> Q Clause
makeMsgRenderMain ctx _rname rname' = do
  let msg = paName $ mcMsg ctx
      locales = paName $ mcLocales ctx
      context = map paName $ mcContext ctx
      cpattern = (map VarP context) ++ [VarP locales, VarP msg]
      expr = foldl1 AppE $ map VarE $ [rname'] ++ context ++ [locales, locales, msg]
  return $ Clause cpattern (NormalB expr) []

makeMsgRenderClause :: MessageContext -> Name -> Catalogue -> Q Clause
makeMsgRenderClause ctx rname catalogue = do
  let msg = paName $ mcMsg ctx
      lang = caLang catalogue
      langp = LitP $ StringL $ T.unpack lang
      rest = mcLocaleRest ctx
      locales = paName $ mcLocales ctx
      rlname = makeLangRenderName ctx lang
      context = map paName $ mcContext ctx
      cpattern = (map VarP context) ++ [InfixP langp (mkName ":") (VarP rest), VarP locales, VarP msg]
      rexpr = foldl1 AppE $ map VarE $ [rname] ++ context ++ [rest, locales, msg]
      expr = foldl1 AppE $ map VarE $ [rlname] ++ context ++ [locales, msg]
      mexpr = (VarE $ 'maybe) `AppE` rexpr `AppE` (VarE $ 'id) `AppE` expr
  return $ Clause cpattern (NormalB mexpr) []

makeMsgRenderNullClause :: MessageContext -> Q Clause
makeMsgRenderNullClause ctx = do
  let lang = caLang $ mcBase ctx
      msg = paName $ mcMsg ctx
      locales = paName $ mcLocales ctx
      rlname = makeLangRenderName ctx lang
      context = map paName $ mcContext ctx
      cpattern = (map VarP context) ++ [ListP [], VarP locales, VarP msg]
      expr = foldl1 AppE $ map VarE $ [rlname] ++ context ++ [locales, msg]
      mexpr = (VarE 'maybe) `AppE` ((VarE 'toHtml) `AppE` ((VarE 'show) `AppE` (VarE msg))) `AppE` (VarE 'id) `AppE` expr
  return $ Clause cpattern (NormalB mexpr) []

makeMsgRenderNotFoundClause :: MessageContext -> Name -> Q Clause
makeMsgRenderNotFoundClause ctx rname' = do
  let msg = paName $ mcMsg ctx
      rest = mcLocaleRest ctx
      locales = paName $ mcLocales ctx
      context = map paName $ mcContext ctx
      cpattern = (map VarP context) ++ [InfixP WildP (mkName ":") (VarP rest), VarP locales, VarP msg]
      rexpr = foldl1 AppE $ map VarE $ [rname'] ++ context ++ [rest, locales, msg]
  return $ Clause cpattern(NormalB rexpr) []

makeInstanceDec :: MessageContext -> Q [Dec]
makeInstanceDec ctx = let
    defe = VarE $ 'def
    msgp = VarP $ paName $ mcMsg ctx
    locs = paName $ mcLocales ctx
    langsn = mkName "langs"
    decoder = InfixE (Just (VarE 'fromString)) (VarE '(.)) (Just (VarE 'unpack))
    (langs, locales, mapper) = if locs == ''Lang then
      (locs, locs, [])
    else
      (langsn, locs, [ValD (VarP locales) (NormalB $ (VarE 'map) `AppE` decoder `AppE` (VarE langsn)) []])
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
          Clause
            [masterp, VarP langs, msgp]
            (NormalB $ AppE (VarE 'renderMarkupToText) $ foldl AppE (VarE $ mcRender ctx) $ exps ++ [VarE locales, VarE $ paName $ mcMsg ctx])
            mapper
        ]
      ]
  in
    return [instancedec]


-- | Convert markup into its plain text equivalent
--
--   Only content is included.
--   Elements and attributes are discarded
--
--   >>> renderMarkupToText [shamlet|<span .something>Hello World</span>|]
--   Hello World!
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
renderChoiceStringToText (TB.EmptyChoiceString) = ""
