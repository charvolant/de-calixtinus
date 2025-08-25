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
  , dateField
  , extendedCheckboxField
  , extendedCheckboxFieldList
  , extendedRadioFieldList
  , extendedSelectionField
  , fieldSettingsLabelTooltip
  , floatField
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
import Data.Propositional
import qualified Data.Set as S
import Data.Text (Text, cons, intercalate, pack, snoc, splitOn, unpack)
import Data.Time.Calendar (Day)
import Yesod.Core
import Yesod.Form.Fields (parseDate)
import Yesod.Form.Types
import Yesod.Form.Functions
import Text.Blaze.Html (ToMarkup, preEscapedToHtml)

parseVal :: (Read a) => Text -> Either FormMessage a
parseVal s = case (reads $ unpack s) of
        [(v, "")] -> Right v
        _ -> Left $ MsgInvalidNumber s

parseMaybeVal :: (Read a) => Text -> Either FormMessage (Maybe a)
parseMaybeVal s = if (s  == (""::Text) || s == ("--"::Text)) then Right Nothing else (Just <$> parseVal s)

-- | Generate a 'FieldSettings' from the given label and tooltip message.
fieldSettingsLabelTooltip :: RenderMessage site msg => msg -- ^ The label message
  -> msg -- ^ The tooptip message
  -> FieldSettings site -- ^ The resulting field settings
fieldSettingsLabelTooltip msg tooltip = FieldSettings (SomeMessage msg) (Just $ SomeMessage tooltip) Nothing Nothing []

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
              <div .clearfix .form-text>#{exp}
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
  -> Maybe msg -- ^ The an optional explanation of the checkbox
  -> Field (HandlerFor site) Bool -- ^ The resulting field
extendedCheckboxField render label mexp = Field
    { fieldParse = \e _ -> return $ checkBoxParser e
    , fieldView  = \theId name attrs val _ -> [whamlet|
        <div .form-check>
          <input .form-check-input id=#{theId} *{attrs} type=checkbox name=#{name} value=yes :showVal id val:checked>
          <label .form-check-label for="#{theId}">^{render label}
          $maybe exp <- mexp
           <div .clearfix .form-text>^{render exp}
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


extendedCheckboxFieldList' :: (Ord a, RenderMessage site FormMessage) => [Option a] -> Field (HandlerFor site) (S.Set a)
extendedCheckboxFieldList' options = Field
    { fieldParse = \rawVals -> \_fileVals -> let
             pvals = map getValue rawVals
           in
             if any isLeft pvals then
               return $ Left $ SomeMessage $ MsgInvalidEntry (pack $ show rawVals)
              else let
                 vals = S.fromList $ rights pvals
               in
                 return $ Right $ Just $ vals
    , fieldView = \theId name _attrs val _isReq -> let
          chosen = either (const S.empty) id val
        in [whamlet|
          $forall option <- options
            <div .form-check id="#{inputId theId (opIndex option)}-container">
              <input .form-check-input type=checkbox name=#{name} value=#{opKey option} id="#{inputId theId (opIndex option)}" :S.member (opValue option) chosen:checked>
              <label .form-check-label for="#{inputId theId (opIndex option)}">^{opLabel option}
              $maybe exp <- opExplanation option
                <div .clearfix .form-text>^{exp}
        |]
    , fieldEnctype = UrlEncoded
    }
    where
      rlookup = M.fromList $ map (\o -> (opKey o, opValue o)) options
      inputId base idx = base <> "-" <> pack (show idx)
      getValue v = maybe (Left $ MsgInvalidEntry v) Right (M.lookup v rlookup)
   
-- | Create a series of checkbox for a series of options with additional explanation
extendedCheckboxFieldList :: (Ord a, RenderMessage site FormMessage) =>
     (msg -> Html) -- ^ The render function. Because basically composing monads makes my brain drop out
  -> [(Text, msg, a, Maybe msg)] -- ^ The possible options for the radio list, with a key (identifier), label, value and optional description
  -> Field (HandlerFor site) (S.Set a) -- ^ The resulting field
extendedCheckboxFieldList render options = extendedCheckboxFieldList' (renderOptions render options)

-- | Create a field that handles preference ranges
rangeField :: (RenderMessage site FormMessage, Read a, Show a, Ord a, ToMarkup a) => a -> a -> a -> Field (HandlerFor site) (PreferenceRange a)
rangeField minv maxv stepv = Field
    { fieldParse = \rawVals -> \_fileVals -> case rawVals of
      [min', lower', target', upper', max'] -> let
            range = PreferenceRange
             <$> pure Nothing
             <*> parseVal target'
             <*> parseVal lower'
             <*> parseVal upper'
             <*> parseMaybeVal min'
             <*> parseMaybeVal max'
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
      showFloat f v = either id (pack . show . f) v
      showMaybeFloat f v = either id (\x -> maybe "--" (pack . show) (f x)) v
      validate e@(Left _msg) = e
      validate v@(Right r) = if validRange r then v else Left (MsgInvalidEntry "Range not valid")
      dash = '\x2014'


-- | The standard list of penance options for services
penanceServiceOptions :: [(Text, Penance, Bool, Html)]
penanceServiceOptions =
    map (\(idx, p) -> (pack $ show idx, p, p == mempty, label p)) (zip [1::Int ..] range)
  where
    label p = if p == mempty then "--" else formatPenance True p
    range = [mempty, Penance 0.5, Penance 1.0, Penance 1.5, Penance 2.0, Penance 2.5, Penance 3.0, Penance 4.0, Penance 5.0, Penance 8.0, Penance 10.0, Reject]


-- | The standard list of penance options for accommodation
penanceAccommodationOptions :: [(Text, Penance, Bool, Html)]
penanceAccommodationOptions =
    map (\(idx, p) -> (pack $ show idx, p, p == mempty, formatPenance True p)) (zip [1::Int ..] range)
  where
    range = [mempty, Penance 1.0, Penance 2.0, Penance 3.0, Penance 4.0, Penance 5.0, Penance 6.0, Penance 7.0, Penance 8.0, Penance 10.0, Penance 12.0, Penance 15.0, Reject]

-- | Parse a penance value
parsePenance options s = case find (\(key, _, _, _) -> key == s) options of
    Just (_, v, _, _) -> Right $ v
    _ -> Left $ MsgInvalidEntry s

-- | A penance selection field
penanceSelect :: [(Text, Penance, Bool, Html)] -> Text -> Text -> [(Text, Text)] -> Either Text Penance -> WidgetFor site ()
penanceSelect options theId name attrs val = [whamlet|
     <select .form-select ##{theId} name=#{name} *{attrs}>
       $forall (key, opt, dflt, label) <- options
         <option value=#{key} :isSelected dflt opt val:selected>^{label}
  |]
  where
    isSelected dflt opt v = either (const dflt) (== opt) v


-- | Create a field that handles penances
penanceField :: (RenderMessage site FormMessage) => Bool -> Field (HandlerFor site) (Penance)
penanceField accom = Field
    { fieldParse = parseHelper (parsePenance options)
    , fieldView = \theId name attrs val _isReq -> penanceSelect options theId name attrs val
    , fieldEnctype = UrlEncoded
    }
  where
    options = if accom then penanceAccommodationOptions else penanceServiceOptions


-- | Create a field that handles preference ranges
penanceMapField :: (RenderMessage site FormMessage, Ord a) => Bool -> Bool -> [(a, Html)] -> Field (HandlerFor site) (M.Map a Penance)
penanceMapField accom allVals values = Field
    { fieldParse = \rawVals -> \_fileVals -> if null rawVals then
          return $ Right Nothing
        else if length values /= length rawVals then
          return $ Left $ SomeMessage $ MsgInvalidEntry (pack $ show rawVals)
        else let
            pvals = map (parsePenance options) rawVals
          in
            if any isLeft pvals then
              return $ Left $ SomeMessage $ MsgInvalidEntry (pack $ show rawVals)
             else let
                zvals = zip (map fst values) (map (fromRight mempty) pvals)
                rvals = filter (\p -> allVals || (snd p /= mempty)) zvals
              in
                return $ Right $ Just $ M.fromList rvals
    , fieldView = \theId name attrs val _isReq -> let
          zippo = zip [1::Int ..] values
        in [whamlet|
      $forall (idx, (opt, label)) <- zippo
        <div .input-group .g-3>
          <span .input-group-text id="#{makeSub theId idx}-label" >^{label}
          ^{penanceSelect options (makeSub theId idx) name attrs (fmap (M.findWithDefault mempty opt) val)}
      |]
    , fieldEnctype = UrlEncoded
    }
    where
      options = if accom then penanceAccommodationOptions else penanceServiceOptions
      makeSub base idx = pack (unpack base ++ "-" ++ show idx)

createCheckFieldCondition' :: (Ord a) =>  M.Map a Int -> Formula a -> Text
createCheckFieldCondition' _zlookup T = "true"
createCheckFieldCondition' _zlookup F = "false"
createCheckFieldCondition' zlookup (Variable v) = "t_" <> pack (show (zlookup M.! v)) 
createCheckFieldCondition' zlookup (And fs) = "(" <> (intercalate " && " (map (createCheckFieldCondition' zlookup) fs)) <> ")"
createCheckFieldCondition' zlookup (Or fs) = "(" <> (intercalate " || " (map (createCheckFieldCondition' zlookup) fs)) <> ")"
createCheckFieldCondition' zlookup (Not f) = "!" <> createCheckFieldCondition' zlookup f
createCheckFieldCondition' zlookup (Implies p c) = "!" <> createCheckFieldCondition' zlookup p <> " || " <> createCheckFieldCondition' zlookup c -- p -> q = !p v q

createCheckFieldCondition :: (Ord a) => Text -> Text -> M.Map a Int -> Formula a -> Html
createCheckFieldCondition base positive zlookup (Implies p (Variable v)) = [shamlet|if (#{preEscapedToHtml (createCheckFieldCondition' zlookup p)}) #{positive}.add("#{base}-#{idx}");|] where idx = zlookup M.! v
createCheckFieldCondition _ _ _ _ = error "Only program clauses permitted"

-- | Create a series of checkboxes for a series of options
implyingCheckListField :: (Ord a, RenderMessage site FormMessage) => (msg -> Html) -> [(Text, msg, a, Maybe msg, Bool)] -> [Formula a] -> [Formula a] -> [Formula a] -> Field (HandlerFor site) (S.Set a)
implyingCheckListField render options requiredClauses allowedClauses prohibitedClauses = Field
    { fieldParse = \rawVals -> \_fileVals -> let
             pvals = map getValue rawVals
           in
             if any isLeft pvals then
               return $ Left $ SomeMessage $ MsgInvalidEntry (pack $ show rawVals)
              else let
                 -- The gets pretty ugly. Disabled values are not returned in HTML so we have to
                 -- deduce any extra values needed
                 vals = S.fromList $ rights pvals
                 sub = substitutionFromDomain values vals
                 implies = implications requiredClauses sub
                 implied = S.filter (\v -> implies v == Just T) values
                 complete = vals `S.union` required `S.union` implied
               in
                 return $ Right $ Just $ complete
    , fieldView = \theId name _attrs val _isReq -> let
          merr = either Just (const Nothing) val
          chosen = required `S.union` either (const S.empty) id val
        in do
           toWidget [whamlet|
$maybe err <- merr
  <div .alert .alert-danger>#{err}
$forall (idx, (key, label, opt, mexp, _req)) <- zoptions
  <div .form-check id="#{inputId theId idx}-container" :S.member opt required:.text-secondary>
    <input .form-check-input type=checkbox name=#{name} value=#{key} id="#{inputId theId idx}" onchange="changed_#{theId}()" :not (S.member opt initialAllowed):disabled :S.member opt chosen:checked>
    <label .form-check-label for="#{inputId theId idx}">#{render label}
    $maybe exp <- mexp
      <div .clearfix .form-text>#{render exp}
      |]
           toWidgetBody [hamlet|
<script>
  function imply_#{theId}(selected, requires, allows, prohibits) {#{nl}
    var nrequires = new Set([]);
    var nallows = new Set([]);
    var nprohibits = new Set([]);#{nl}
    $forall (idx, (_key, _label, _opt, _mexp, _req)) <- zoptions
      var t_#{idx} = (selected.has("#{inputId theId idx}") || requires.has("#{inputId theId idx}")) && !prohibits.has("#{inputId theId idx}");#{nl}
    $forall clause <- requiredClauses
      #{createCheckFieldCondition theId "nrequires" zlookup clause}#{nl}
    $forall clause <- allowedClauses
      #{createCheckFieldCondition theId "nallows" zlookup clause}#{nl}
    $forall clause <- prohibitedClauses
      #{createCheckFieldCondition theId "nprohibits" zlookup clause}#{nl}
    if (nrequires.size == requires.size && requires.isSupersetOf(nrequires) && nallows.size == allows.size && allows.isSupersetOf(nallows) && nprohibits.size == prohibits.size && prohibits.isSupersetOf(nprohibits))#{nl}
      return [nrequires, nallows, nprohibits];#{nl}
    return imply_#{theId}(selected, nrequires, nallows, nprohibits);#{nl}
  }
  
  function changed_#{theId}() {#{nl}
    var checked = $('input[name="#{name}"]:checked');
    var selected = new Set([]);
    var enable = new Set([#{setIds theId initialAllowed}]);
    var disable = new Set([#{setIds theId initialDisabled}]);
    var check = new Set([#{setIds theId required}]);
    var uncheck = new Set([]);
    checked.each(function() {
      selected.add(this.id);
    });
    var implied = imply_#{theId}(selected, new Set([]), new Set([]), new Set([]));
    enable = enable.union(implied[1]).difference(implied[0]).difference(implied[2]);
    disable = disable.difference(implied[1]).union(implied[0]).union(implied[2]);
    uncheck = uncheck.union(implied[2]);
    check = check.union(implied[0]);
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

  \$(document).ready(changed_#{theId});
|]
    , fieldEnctype = UrlEncoded
    }
    where
      values = S.fromList $ map (\(_key, _label, v, _mmsg, _dflt) -> v) options
      required = S.fromList $ map (\(_, _, opt, _, _) -> opt) $ filter (\(_, _, _, _, req) -> req) options
      optional = S.fromList $ map (\(_, _, v, _, _) -> v) $ filter (\(_, _, _, _, req) -> not req) options
      conditional = S.fromList $ map clauseConsequentVar allowedClauses
      initialAllowed = optional `S.difference` conditional
      initialDisabled = conditional `S.union` required
      zoptions = zip [1::Int ..] options
      zlookup = M.fromList $ map (\(idx, (_key, _label, v, _mmsg, _dflt)) -> (v, idx)) zoptions
      rlookup = M.fromList $ map (\(key, _label, v, _mmsg, _dflt) -> (key, v)) options
      inputId base idx = pack (unpack base ++ "-" ++ show idx)
      setIds base vals = preEscapedToMarkup $ intercalate ", " $ map (\v -> cons '"' (snoc (inputId base (zlookup M.! v)) '"')) $ S.toList vals
      getValue v = maybe (Left $ MsgInvalidEntry v) Right (M.lookup v rlookup)
      nl = "\n" :: Text

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
          chosenKey = either (const "") (\v -> M.findWithDefault "" v klookup) val
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

dateField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Day
dateField = Field
    { fieldParse = parseHelper $ parseDate . unpack
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input .form-control id="#{theId}" name="#{name}" *{attrs} type="date" :isReq:required value="#{showVal val}">
|]
    , fieldEnctype = UrlEncoded
    }
  where showVal = either id (pack . show)

-- | Enter a float number
floatField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Float -> Float -> Float -> Field m Float
floatField minv maxv stepv = Field
    {
        fieldParse = parseHelper $ parseVal
      , fieldView = \theId name attrs val _isReq -> toWidget [hamlet|
$newline never
    <input .form-control id="#{theId}" name=#{name} type=number min=#{minv} max=#{maxv} step=#{stepv} value=#{showFloat val} *{attrs}>
|]
      , fieldEnctype = UrlEncoded
    }
    where
      showFloat v = either id (pack . show) v
