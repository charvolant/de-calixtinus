{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Fields
Description : Fields specialised for the De Calixtinus application
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

A collection of fields for yesod forms that display data in ways that fit the Camino preferences and data
-}

module Camino.Server.Fields (
    clickSelectionField
  , extendedCheckboxField
  , extendedRadioFieldList
  , extendedSelectionField
  , implyingCheckListField
  , parsingHiddenField
  , penanceField
  , penanceMapField
  , rangeField
) where

import Camino.Camino (Penance(..))
import Camino.Preferences (PreferenceRange(..), validRange)
import Camino.Display.I18n (formatPenance)
import Data.Either (fromRight, isLeft, rights)
import Data.List (find)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, cons, intercalate, pack, snoc, splitOn, unpack)
import Yesod.Core
import Yesod.Form.Types
import Yesod.Form.Functions
import Text.Blaze.Html (ToMarkup)

-- | Creates an input with @type="hidden"@ where the hidden fields can be mapped to and from an actual value.
--   This can be useful when you have something that needs to be decoded in context and a @PathPiece@ just doesn't have the relevant information.
parsingHiddenField :: (Monad m, PathPiece p, RenderMessage (HandlerSite m) FormMessage)
  => (a -> p) -- ^ The encoder function
  -> (p -> Maybe a) -- ^ The decoder function
  -> Field m a    
parsingHiddenField encoder decoder = Field
    { fieldParse = parseHelper $ (\v -> maybe (Left $ MsgInvalidEntry v) Right $ maybe Nothing decoder $ fromPathPiece v)
    , fieldView = \theId name attrs val _isReq -> toWidget [hamlet|<input type="hidden" id="#{theId}" name="#{name}" *{attrs} value="#{encode val}">|]
    , fieldEnctype = UrlEncoded
    }
    where
      encode v = either id (toPathPiece . encoder) v

data Option a = Option {
    opIndex :: Int
  , opKey :: Text
  , opLabel :: Html
  , opValue :: a
  , opExplanation :: Maybe Html
}

renderOptions :: (msg -> Html) -> [(Text, msg, a, Maybe msg)] -> [Option a]
renderOptions render options = let zopts = zip [1 ..] options in
  map (\(idx, (key, label, val, mexp)) -> Option idx key (render label) val (render <$> mexp)) zopts

extendedRadioFieldList' :: (Eq a, RenderMessage site FormMessage) => [Option a] -> Field (HandlerFor site) a
extendedRadioFieldList' options = Field
  { 
      fieldParse = parseHelper $ getValue
    , fieldView = \theId name _attrs val _isReq -> let
        valkey = either (const "") getKeyValue val
      in [whamlet|
        $forall opt <- options
          <div .form-check>
          <input .form-check-input type=radio name=#{name} value=#{opKey opt} id="#{theId}-#{opIndex opt}" :opKey opt == valkey:checked>
          <label .form-check-label for="#{theId}-#{opIndex opt}">#{opLabel opt}
          $maybe exp <- opExplanation opt
           <div .form-text>#{exp}
      |]
    , fieldEnctype = UrlEncoded
  }
  where
   keymap = M.fromList $ map (\opt -> (opKey opt, opValue opt)) options
   getValue v = maybe (Left $ MsgInvalidEntry v) Right (M.lookup v keymap)
   getKeyValue v = maybe "" opKey $ find (\opt-> v == opValue opt) options
   
-- | Create a series of radio buttons for a series of mutually exclusive options with additional explanation
extendedRadioFieldList :: (Eq a, RenderMessage site FormMessage) => 
     (msg -> Html) -- ^ The render function. Because basically composing monads makes my brain drop out
  -> [(Text, msg, a, Maybe msg)] -- ^ The possible options for the radio list, with a key (identifier), label, value and optional description
  -> Field (HandlerFor site) a -- ^ The resulting field
extendedRadioFieldList render options = extendedRadioFieldList' (renderOptions render options)
   
-- | Create a checkbox with additional explanation
extendedCheckboxField :: (msg -> Html) -- ^ The render function. Because basically composing monads makes my brain drop out
  -> msg -- ^ The label associated with the checkbox
  -> Maybe msg -- ^ The possible options for the radio list, with a key (identifier), label, value and optional description
  -> Field (HandlerFor site) Bool -- ^ The resulting field
extendedCheckboxField render label mexp = Field
    { fieldParse = \e _ -> return $ checkBoxParser e
    , fieldView  = \theId name attrs val _ -> [whamlet|
        <div .form-check>
          <input .form-check-input id=#{theId} *{attrs} type=checkbox name=#{name} value=yes :showVal id val:checked>
          <label .form-check-label for="#{theId}">^{render label}
          $maybe exp <- mexp
           <div .form-text>^{render exp}
|]
    , fieldEnctype = UrlEncoded
    }
    where
        checkBoxParser [] = Right $ Just False
        checkBoxParser (x:_) = case x of
            "yes" -> Right $ Just True
            "on" -> Right $ Just True
            _     -> Right $ Just False

        showVal = either (\_ -> False)


-- | Create a field that handles preference ranges
rangeField :: (RenderMessage site FormMessage) => Float -> Float -> Float -> Field (HandlerFor site) (PreferenceRange Float)
rangeField minv maxv stepv = Field
    { fieldParse = \rawVals -> \_fileVals -> case rawVals of
      [min', lower', target', upper', max'] -> let
            range = PreferenceRange
             <$> pure Nothing
             <*> parseFloat target'
             <*> parseFloat lower'
             <*> parseFloat upper'
             <*> parseMaybeFloat min'
             <*> parseMaybeFloat max'

          in
            return $ either (Left . SomeMessage) (Right . Just) (validate range)
      [] -> return $ Right Nothing
      _ -> return $ Left $ SomeMessage $ MsgInvalidEntry (pack $ show rawVals)
    , fieldView = \theId name attrs val isReq ->  [whamlet|
      <div .row .g-3>
        <div .col-auto>
          <input .form-control .text-danger id="#{theId}-min" name=#{name} type=number min=#{minv} max=#{maxv} step=#{stepv} value=#{showMaybeFloat rangeMinimum val} *{attrs}>
        <div .col-auto>#{dash}
        <div .col-auto>
          <input .form-control id="#{theId}-lower" name=#{name} type=number min=#{minv} max=#{maxv} step=#{stepv} value=#{showFloat rangeLower val} *{attrs} :isReq:required>
        <div .col-auto>#{dash}
        <div .col-auto>
          <input .form-control .text-success id="#{theId}-target" name=#{name} type=number min=#{minv} max=#{maxv} step=#{stepv} value=#{showFloat rangeTarget val} *{attrs} :isReq:required>
        <div .col-auto>#{dash}
        <div .col-auto>
          <input .form-control id="#{theId}-upper" name=#{name} type=number min=#{minv} max=#{maxv} step=#{stepv} value=#{showFloat rangeUpper val} *{attrs} :isReq:required>
        <div .col-auto>#{dash}
        <div .col-auto>
          <input .form-control .text-danger id="#{theId}-max" name=#{name} type=number min=#{minv} max=#{maxv} step=#{stepv} value=#{showMaybeFloat rangeMaximum val} *{attrs}>
      |]
    , fieldEnctype = UrlEncoded
    }
    where
      parseFloat s = case (reads $ unpack s) of
        [(v, "")] -> Right v
        _ -> Left $ MsgInvalidNumber s
      parseMaybeFloat s = if (s  == (""::Text) || s == ("--"::Text)) then Right Nothing else (Just <$> parseFloat s)
      showFloat f v = either id (pack . show . f) v
      showMaybeFloat f v = either id (\x -> maybe "--" (pack . show) (f x)) v
      validate e@(Left _msg) = e
      validate v@(Right r) = if validRange r then v else Left (MsgInvalidEntry "")
      dash = '\x2014'


-- | The standard list of penance options
penanceOptions :: [(Text, Penance, Bool, Html)]
penanceOptions =
    map (\(idx, p) -> (pack $ show idx, p, p == mempty, formatPenance p)) (zip [1::Int ..] range)
  where
    range = [mempty, Penance 0.5, Penance 1.0, Penance 1.5, Penance 2.0, Penance 2.5, Penance 3.0, Penance 4.0, Penance 5.0, Penance 8.0, Penance 10.0, Reject]

-- | Parse a penance value
parsePenance s = case find (\(key, _, _, _) -> key == s) penanceOptions of
    Just (_, v, _, _) -> Right $ v
    _ -> Left $ MsgInvalidEntry s

-- | A penance selection field
penanceSelect :: Text -> Text -> [(Text, Text)] -> Either Text Penance -> WidgetFor site ()
penanceSelect theId name attrs val = [whamlet|
     <select .form-select ##{theId} name=#{name} *{attrs}>
       $forall (key, opt, dflt, label) <- penanceOptions
         <option value=#{key} :isSelected dflt opt val:selected>^{label}
  |]
  where
    isSelected dflt opt v = either (const dflt) (== opt) v


-- | Create a field that handles penances
penanceField :: (RenderMessage site FormMessage) => Field (HandlerFor site) (Penance)
penanceField = Field
    { fieldParse = parseHelper parsePenance
    , fieldView = \theId name attrs val _isReq -> penanceSelect theId name attrs val
    , fieldEnctype = UrlEncoded
    }

-- | Create a field that handles preference ranges
penanceMapField :: (RenderMessage site FormMessage, Ord a) => [(a, Html)] -> Field (HandlerFor site) (M.Map a Penance)
penanceMapField values = Field
    { fieldParse = \rawVals -> \_fileVals -> if null rawVals then
          return $ Right Nothing
        else if length values /= length rawVals then
          return $ Left $ SomeMessage $ MsgInvalidEntry (pack $ show rawVals)
        else let
            pvals = map parsePenance rawVals
          in
            if any isLeft pvals then
              return $ Left $ SomeMessage $ MsgInvalidEntry (pack $ show rawVals)
             else let
                zvals = zip (map fst values) (map (fromRight mempty) pvals)
                rvals = filter (\p -> snd p /= mempty) zvals
              in
                return $ Right $ Just $ M.fromList rvals
    , fieldView = \theId name attrs val _isReq -> let
          zippo = zip [1::Int ..] values
        in [whamlet|
      $forall (idx, (opt, label)) <- zippo
        <div .input-group .g-3>
          <span .input-group-text id="#{makeSub theId idx}-label" >^{label}
          ^{penanceSelect (makeSub theId idx) name attrs (fmap (M.findWithDefault mempty opt) val)}
      |]
    , fieldEnctype = UrlEncoded
    }
    where
      makeSub base idx = pack (unpack base ++ "-" ++ show idx)

-- | Create a series of radio buttons for a series of options
implyingCheckListField :: (Ord a, ToMarkup msg, RenderMessage site FormMessage) => [(Text, msg, a, Maybe msg, Bool, S.Set a, S.Set a)] -> Field (HandlerFor site) (S.Set a)
implyingCheckListField options = Field
    { fieldParse = \rawVals -> \_fileVals -> let
             pvals = map getValue rawVals
           in
             if any isLeft pvals then
               return $ Left $ SomeMessage $ MsgInvalidEntry (pack $ show rawVals)
              else let
                 -- The gets pretty ugly. Disabled values are not returned in HTML so we have to
                 -- deduce the extra values needed
                 vals = S.fromList $ rights pvals
                 reqlookup = M.fromList $ map (\(_, _, v, _, _, r, _) -> (v, r)) options
                 req = S.unions $ S.map (reqlookup M.!) vals
                 complete = vals `S.union` required `S.union` req
               in
                 return $ Right $ Just $ complete
    , fieldView = \theId name _attrs val _isReq -> let
          merr = either Just (const Nothing) val
          chosen = required `S.union` either (const S.empty) id val
          optional = S.fromList $ map (\(_, _, v, _, _, _, _) -> v) $ filter (\(_, _, _, _, req, _, _) -> not req) options
        in do
           toWidget [whamlet|
$maybe err <- merr
  <div .alert .alert-danger>#{err}
$forall (idx, (key, label, opt, mexp, _req, _requ, _excl)) <- zoptions
  <div .form-check id="#{inputId theId idx}-container" :S.member opt required:.text-secondary>
    <input .form-check-input type=checkbox name=#{name} value=#{key} id="#{inputId theId idx}" onchange="changed_#{theId}()" :S.member opt required:disabled :S.member opt chosen:checked>
    <label .form-check-label for="#{inputId theId idx}">#{label}
    $maybe exp <- mexp
      <div .form-text>#{exp}
      |]
           toWidgetBody [hamlet|
<script>
  var required_#{theId} = new Object();
  var exclusive_#{theId} = new Object();
  $forall (idx, (_key, _label, _opt, _mexp, _req, requ, excl)) <- zoptions
    required_#{theId}["#{inputId theId idx}"] = new Set([#{setIds theId requ}]);
    exclusive_#{theId}["#{inputId theId idx}"] = new Set([#{setIds theId excl}]);
  function changed_#{theId}() {
    var checked = $('input[name="#{name}"]:checked');
    var enable = new Set([#{setIds theId optional}]);
    var disable = new Set([#{setIds theId required}]);
    var check = new Set([#{setIds theId required}]);
    var uncheck = new Set([]);
    checked.each(function() {
      var id = this.id;
      var req = required_#{theId}[id];
      var exl = exclusive_#{theId}[id];
      enable = enable.difference(exl).difference(req);
      disable = disable.union(exl).union(req);
      uncheck = uncheck.union(exl);
      check = check.union(req);
   });
   enable.forEach(function(v, k, s) {
    var e = $('#' + v);
    e.prop("disabled", false);
    e = $('#' + v + '-container');
    e.removeClass("text-secondary");
   });
   disable.forEach(function(v, k, s) {
     var e = $('#' + v);
     e.prop("disabled", true);
     e = $('#' + v + '-container');
     e.addClass("text-secondary");
   });
   uncheck.forEach(function(v, k, s) {
    var e = $('#' + v);
    e.prop("checked", false);
   });
   check.forEach(function(v, k, s) {
    var e = $('#' + v);
    e.prop("checked", true);
   });
  }
|]
    , fieldEnctype = UrlEncoded
    }
    where
      zoptions = zip [1::Int ..] options
      zlookup = M.fromList $ map (\(idx, (_key, _label, v, _mmsg, _dflt, _re, _ex)) -> (v, idx)) zoptions
      rlookup = M.fromList $ map (\(key, _label, v, _mmsg, _dflt, _re, _ex) -> (key, v)) options
      required = S.fromList $ map (\(_, _, opt, _, _, _, _) -> opt) $ filter (\(_, _, _, _, req, _, _) -> req) options
      inputId base idx = pack (unpack base ++ "-" ++ show idx)
      setIds base vals = preEscapedToMarkup $ intercalate ", " $ map (\v -> cons '"' (snoc (inputId base (zlookup M.! v)) '"')) $ S.toList vals
      getValue v = maybe (Left $ MsgInvalidEntry v) Right (M.lookup v rlookup)

-- | Create a field that allows clicking of possible values into a "select-pen"
clickSelectionField :: (Ord a, ToMarkup msg, RenderMessage site FormMessage) => [(msg, [(Text, msg, a)])] -> Field (HandlerFor site) (S.Set a)
clickSelectionField categories = Field
      { fieldParse = \rawVals -> \_fileVals -> case rawVals of
          [] -> return $ Right Nothing
          [rawVal] -> let
              pvals = if rawVal == "" then [] else map getValue (splitOn "|" rawVal)
            in
              if any isLeft pvals then
                return $ Left $ SomeMessage $ MsgInvalidEntry (pack $ show rawVals)
              else
                return $ Right $ Just $ S.fromList $ rights pvals
          _ -> return $ Left $ SomeMessage $ MsgInvalidEntry (pack $ show rawVals)
    , fieldView = \theId name _attrs val _isReq -> let
          merr = either Just (const Nothing) val
          chosen = either (const S.empty) id val
          chosenKeys = intercalate "|" (map (\v -> klookup M.! v) (S.toList chosen))
          chosenIndexes = intercalate ", " (map (\v -> pack $ show $ getIndex v) (S.toList chosen))
      in do
        toWidget [whamlet|
<div .container-fluid id=#{theId}>
  $maybe err <- merr
    <div .alert .alert-danger>#{err}
  <input id="#{theId}-value" type="hidden" name=#{name} value="#{chosenKeys}">
  <div .row .mb-1 .border .border-primary style="min-height: 1.5rem;" id="#{theId}-selected">
    $forall sel <- chosen
      <div .col-3 .col-md-2>
        <a href="#" onclick="remove_#{theId}(#{getIndex sel});">#{getLabel sel}
  <div .accordion id="accordion-#{theId}">
    $forall (cidx, (category, opts)) <- zcategories
      $if not (null opts)
        <div .accordion-item>
          <h6 .text-primary .accordion-header id="heading-#{theId}-#{cidx}">
            <button .accordion-button :cidx /= 1:.collapsed type=button data-bs-toggle=collapse data-bs-target="#collapse-#{theId}-#{cidx}" :cidx == 1:aria-expanded="true" :cidx /= 1:aria-expanded="false" aria-controls="collapse-#{theId}-#{cidx}">
                #{category}
          <div id="collapse-#{theId}-#{cidx}" .accordion-collapse .collapse :cidx == 1:.show aria-labelledby="heading-#{theId}-#{cidx}" data-bs-parent="#accordion-#{theId}">
            <div .accordion-body>
              <div .row>
                $forall (_, label, opt) <- opts
                  <div .col-3 .col-md-2>
                    <a id=#{inputId theId opt} href="#" :S.member opt chosen:.text-secondary  onclick="add_#{theId}(#{getIndex opt});">#{label}
|]
        toWidgetBody [hamlet|
<script>
  var lookup_#{theId} = new Object();
  var lookup_key_#{theId} = new Object();
  var selected_#{theId} = new Set([#{chosenIndexes}]);
  $forall (idx, (key, label, _)) <- zoptions
    lookup_#{theId}[#{idx}] = "#{label}";
    lookup_key_#{theId}[#{idx}] = "#{key}";
  function display_#{theId}() {
    var seli = Array.from(selected_#{theId});
    var selk = seli.map(i => lookup_key_#{theId}[i]);
    \$('##{theId}-value').val(selk.join("|"));
    \$('##{theId} a.text-secondary').removeClass('text-secondary');
    var sel = $('##{theId}-selected');
    sel.empty();
    selected_#{theId}.forEach(function(v, k, s) {
      sel.append('<div class="col-3 col-md-2"><a href="#" onclick="remove_#{theId}(' + v +');">' + lookup_#{theId}[v] + '</a></div>');
      \$('##{theId}-' + v).addClass('text-secondary')
    });
  }
  function add_#{theId}(idx) {
    selected_#{theId}.add(idx);
    display_#{theId}();
  }
  function remove_#{theId}(idx) {
    selected_#{theId}.delete(idx);
    display_#{theId}();
  }
|]
    , fieldEnctype = UrlEncoded
    }
    where
      zcategories = zip [1::Int ..] categories
      options = concat $ map snd categories
      zoptions = zip [1::Int ..] options
      zlookup = M.fromList $ map (\(idx, (_, _, v)) -> (v, idx)) zoptions
      llookup = M.fromList $ map (\(_, label, v) -> (v, label)) options
      klookup = M.fromList $ map (\(key, _, v) -> (v, key)) options
      rlookup = M.fromList $ map (\(key, _, v) -> (key, v)) options
      inputId base v = pack (unpack base ++ "-" ++ show (zlookup M.! v))
      getIndex v = zlookup M.! v
      getLabel v = llookup M.! v
      getValue v = maybe (Left $ MsgInvalidEntry v) Right (M.lookup v rlookup)
      
-- | A selection field that allows keys
extendedSelectionField :: (Ord a, ToMarkup msg, RenderMessage site FormMessage) => [(msg, [(Text, msg, a)])] -> Field (HandlerFor site) a
extendedSelectionField categories = Field
      { fieldParse = parseHelper $ getValue
    , fieldView = \theId name _attrs val _isReq -> let
          merr = either Just (const Nothing) val
          chosenKey = either (const "") (\v -> klookup M.! v) val
      in [whamlet|
$maybe err <- merr
  <div .alert .alert-danger>#{err}
<select .form-select id=#{theId} name=#{name} value=#{chosenKey}>
  $forall (category, opts) <- categories
    $if not (null opts)
      <optgroup label=#{category}>
        $forall (key, label, _) <- opts
          <option value=#{key} :key == chosenKey:selected>#{label}
|]

    , fieldEnctype = UrlEncoded
    }
    where
      options = concat $ map snd categories
      klookup = M.fromList $ map (\(key, _, v) -> (v, key)) options
      rlookup = M.fromList $ map (\(key, _, v) -> (key, v)) options
      getValue v = maybe (Left $ MsgInvalidEntry v) Right (M.lookup v rlookup)
