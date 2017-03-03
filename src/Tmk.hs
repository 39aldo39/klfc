{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tmk where

import BasePrelude
import Prelude.Unicode hiding ((∈), (∉))
import Data.Foldable.Unicode ((∈), (∉))
import Data.List.Unicode ((∖))
import Data.Monoid.Unicode ((⊕))
import qualified Data.Set.Unicode as S
import Util (show', toString, (>$>), filterOnFst, groupWith')
import WithBar (WithBar(..))
import WithPlus (WithPlus(..))

import Control.Monad.State (State, runState, get, gets, modify)
import Control.Monad.Writer (runWriter, tell)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Lens.Micro.Platform (view, over, makeLenses, _1, _2)

import Layout.Key (Key(..), getLevel)
import Layout.Layout
import Layout.Modifier (toBaseModifier, getEqualModifiers)
import Layout.ModifierEffect (defaultModifierEffect)
import Layout.Types
import Lookup.Linux (actionAndRedirect)
import Lookup.Tmk
import PresetLayout (defaultKeys, defaultFullKeys, defaultFullLayout)

prepareLayout ∷ Logger m ⇒ Layout → m Layout
prepareLayout =
    addSingletonKeysAsKeys >>>
    addDefaultKeys defaultKeys >>>
    addDefaultKeysWith getDefaultKeys' defaultFullKeys >>>
    _keys (filterM (supportedPos ∘ view _pos))
  where
    getDefaultKeys' = flip $ \layout →
        filter (liftA2 (∧) (∉ posses layout) isSupportedPos ∘ view _pos)
    isSupportedPos = fst ∘ runWriter ∘ supportedPos
    posses = map (view _pos) ∘ view _keys

supportedPos ∷ Logger m ⇒ Pos → m Bool
supportedPos pos
  | pos S.∈ __usedPosses unimap = pure True
  | otherwise = False <$ tell [show' pos ⊕ " is not supported in TMK"]

data TmkLetter
    = TmkAction String
    | TmkFn String
    | TmkModifier Modifier
    | TmkMacro String [String] [String]
    deriving (Eq, Show, Read)

isTmkModifier ∷ TmkLetter → Bool
isTmkModifier (TmkModifier _) = True
isTmkModifier _ = False

toMacro ∷ TmkLetter → Maybe (String, [String], [String])
toMacro (TmkMacro mId press release) = Just (mId, press, release)
toMacro _ = Nothing

type Tmk = State [String]

printTmkLetter ∷ Set Modifier → TmkLetter → Tmk String
printTmkLetter _ (TmkAction s) = pure s
printTmkLetter _ (TmkFn s) = writeAction s
printTmkLetter modifiersInfluenced (TmkModifier modifier)
  | modifier ∈ modifiersInfluenced
  = writeAction ("ACTION_FUNCTION_OPT(F_MODIFIER, MOD_" ⊕ map toUpper (toString modifier) ⊕ ")")
  | otherwise = pure $ fromMaybe "NO" (lookup modifier modifierAndKeycode)
printTmkLetter _ (TmkMacro macroId _ _) = writeAction ("ACTION_MACRO(" ⊕ macroId ⊕ ")")

writeAction ∷ String → Tmk String
writeAction s = findIndex (≡ s) <$> get >>= maybe
    (gets (("FN" ⊕) ∘ show ∘ length) <* modify (⧺ [s]))
    (pure ∘ ("FN" ⊕) ∘ show)

data TmkLayer = TmkLayer
    { __tmkShiftstate ∷ Shiftstate
    , __tmkLetters ∷ Map Pos TmkLetter
    } deriving (Show, Read)
makeLenses ''TmkLayer

printTmkLayer ∷ Set Modifier → Int → (Map Pos TmkLetter, Shiftlevel) → Tmk [String]
printTmkLayer modifiersInfluenced i (tmkLetters, shiftlevel) = do
    letterMaps ← traverse (printTmkLetter modifiersInfluenced) tmkLetters
    let actions = zipWith (zipWith (\size → printf ("%" ⊕ show size ⊕ "s") ∘ getAction letterMaps)) (__sizes unimap) (__posses unimap)
    pure (
        [ "// " ⊕ toString shiftlevel
        , "[" ⊕ show i ⊕ "] = " ⊕ __name unimap ⊕ "("
        ] ⧺ addCommas (map (intercalate ",") actions) ⧺
        [ "),"
        ])
  where
    getAction lMaps pos = fromMaybe "NO" $ M.lookup pos lMaps <|> lookup pos posAndTmkAction
    addCommas [] = []
    addCommas [x] = [x]
    addCommas (x:xs) = x ⊕ "," : addCommas xs

data TmkKeymap = TmkKeymap
    { __tmkGetMaxIndex ∷ Shiftstate → Int
    , __tmkLayers ∷ [TmkLayer]
    }
makeLenses ''TmkKeymap

printTmkKeymap ∷ TmkKeymap → String
printTmkKeymap (TmkKeymap getMaxIndex layers') = unlines $
    [ "#include \"unimap_trans.h\""
    , "#include \"action_util.h\""
    , "#include \"action_layer.h\""
    , ""
    , "enum function_id {"
    , "    F_MODIFIER,"
    , "};"
    , ""
    ] ⧺ bool (
    [ "enum macro_id {"
    ] ⧺ map (\macro → replicate 4 ' ' ⊕ view _1 macro ⊕ ",") macros ⧺
    [ "};"
    , ""
    ]) [] (null macros) ⧺ bool (
    [ "enum modifier_id {"
    ] ⧺ map (\m → replicate 4 ' ' ⊕ showMod m ⊕ ",") modifiersInfluencedList ⧺
    [ "};"
    , ""
    ]) [] (null modifiersInfluencedList)
    ⧺ zipWith (\i action → "#define AC_FN" ⊕ show i ⊕ " " ⊕ action) [0 ∷ Int ..] functionKeys ⧺
    [ ""
    , "#ifdef KEYMAP_SECTION_ENABLE"
    , "const action_t actionmaps[][UNIMAP_ROWS][UNIMAP_COLS] __attribute__ ((section (\".keymap.keymaps\"))) = {"
    , "#else"
    , "const action_t actionmaps[][UNIMAP_ROWS][UNIMAP_COLS] PROGMEM = {"
    , "#endif"
    ] ⧺ map (replicate 4 ' ' ⊕) printedLayers ⧺
    [ "};"
    , ""
    ] ⧺ bool (
    [ "const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt) {"
    , "    switch (id) {"
    ] ⧺ concatMap (map (replicate 8 ' ' ⊕) ∘ printMacro) macros ⧺
    [ "    }"
    , "    return MACRO_NONE;"
    , "}"
    , ""
    ]) [] (null macros)
    ⧺ map (\m → "#define " ⊕ showMask m ⊕ " (" ⊕ intercalate "|" (map (\m' → "MOD_BIT(" ⊕ showKeycode m' ⊕ ")") (modifierToRealModifiers m)) ⊕ ")") realModsInShiftstates
    ⧺ bool [""] [] (null realModsInShiftstates)
    ⧺ bool (
    [ "enum virtual_mod_mask {"
    ] ⧺ zipWith (\i m → replicate 4 ' ' ⊕ showMask m ⊕ " = " ⊕ show (2^i ∷ Int) ⊕ ",") [0 ∷ Int ..] virtualModsInShiftstates ⧺
    [ "};"
    , ""
    ]) [] (null virtualModsInShiftstates) ⧺
    [ "uint" ⊕ show indexSize ⊕ "_t virtual_mods = 0;"
    , ""
    , "const uint" ⊕ show layerSize ⊕ "_t layer_states[] = {"
    ] ⧺ map (\(i, state) → replicate 4 ' ' ⊕ "0x" ⊕ showHex (layerStateAfterGrouping i) "," ⊕ " // " ⊕ toString state) (getLayers getMaxIndex layers') ⧺
    [ "};"
    , ""
    , "void action_function(keyrecord_t *record, uint8_t id, uint8_t opt) {"
    , "    uint8_t pressed = record->event.pressed;"
    , "    switch (id) {"
    , "        case F_MODIFIER:"
    , "            // Set the new modifier"
    , "            switch (opt) {"
    ] ⧺
    map (\m → "                case " ⊕ showMod m ⊕ ": pressed ? add_key(" ⊕ showKeycode m ⊕ ") : del_key(" ⊕ showKeycode m ⊕ "); break;") realModsInfluenced ⧺
    map (\m → "                case " ⊕ showMod m ⊕ ": pressed ? (virtual_mods |= " ⊕ showMask m ⊕ ") : (virtual_mods &= ~" ⊕ showMask m ⊕ "); break;") virtualModsInfluenced ⧺
    [ "            }"
    , ""
    , "            // Update the layer"
    , "            uint8_t mods = get_mods();"
    , "            uint" ⊕ show indexSize ⊕ "_t layer_index = 0;"
    ] ⧺ zipWith (\i m → "            layer_index |= mods & " ⊕ showMask m ⊕ " ? " ⊕ show (2^i ∷ Int) ⊕ " : 0;") [0 ∷ Int ..] realModsInShiftstates ⧺
    [ "            layer_index |= virtual_mods << " ⊕ show (length realModsInShiftstates) ⊕ ";"
    , "            layer_clear();"
    , "            layer_or(layer_states[layer_index]);"
    , "            break;"
    , "    }"
    , "}"
    ]
  where
    (printedLayers, functionKeys) = flip runState [] $
        concat <$> zipWithM (printTmkLayer modifiersInfluenced) [0..] layers
    shiftstates = map __tmkShiftstate layers'
    (layerStateAfterGrouping, layers) = groupLayers layers'
    modifiersInShiftstates = S.unions ∘ map getSet $ shiftstates
    modifiersInfluenced = S.fromList ∘ concatMap (concatMap getEqualModifiers ∘ toList) $ shiftstates
    modifiersInfluencedList = toList modifiersInfluenced
    showMod = ("MOD_" ⊕) ∘ map toUpper ∘ toString
    showKeycode modifier =
        let e = error (show' modifier ⊕ " is not a real modifier in TMK")
        in "KC_" ⊕ fromMaybe e (lookup modifier modifierAndKeycode)
    showMask modifier =
        bool "VIRTUAL_" "" (isRealModifier modifier) ⊕
        showMod (toBaseModifier modifier) ⊕ "_MASK"
    (realModsInShiftstates, virtualModsInShiftstates) = partition isRealModifier (toList modifiersInShiftstates)
    (realModsInfluenced, virtualModsInfluenced) = partition isRealModifier modifiersInfluencedList
    asPower2 x
      | x < 1     = 8
      | otherwise = max 8 (bit (ceiling (logBase 2 (fromIntegral x ∷ Double)) ∷ Int)) ∷ Int
    indexSize = asPower2 (length modifiersInShiftstates)
    layerSize = asPower2 (length layers')

    macros = nub (mapMaybe toMacro (concatMap (M.elems ∘ fst) layers))
    printMacro (mId, press, release) =
        [ "case " ⊕ mId ⊕ ":"
        , "    return record->event.pressed ?"
        , "           " ⊕ printMacroPart press ⊕ " :"
        , "           " ⊕ printMacroPart release ⊕ ";"
        ]
    printMacroPart [] = "MACRO_NONE"
    printMacroPart parts = "MACRO(" ⊕ concatMap (⊕ ", ") parts ⊕ "END)"

getLayers ∷ (Shiftstate → Int) → [TmkLayer] → [(Int, Shiftstate)]
getLayers getMaxIndex layers = ($ shiftstates) $
    S.toAscList ∘ S.unions ∘ map getSet >>> -- get an ascending list of modifiers
    uncurry (⧺) ∘ partition isRealModifier >>> -- place the real modifiers at the front
    map (WithPlus ∘ S.fromList) ∘ subsequences >>> -- generate the substates with the real modifiers at the front
    map (sum ∘ map bit ∘ getActiveStates getMaxIndex shiftstates &&& id)
  where
    shiftstates = map __tmkShiftstate layers

getActiveStates ∷ (Shiftstate → Int) → [Shiftstate] → Shiftstate → [Int]
getActiveStates getMaxIndex states state =
    findIndices (`isSubState` state) (take (getMaxIndex state) states)

toTmkKeymap ∷ Logger m ⇒ Layout → m TmkKeymap
toTmkKeymap =
    prepareLayout >=>
    toTmkKeymap' >$>
    over _tmkLayers addTransLetters >>>
    removeTransLayers

toTmkKeymap' ∷ Logger m ⇒ Layout → m TmkKeymap
toTmkKeymap' layout =
    TmkKeymap getMaxIndex <$> zipWithM lettersToTmkLayer shiftstates letters
  where
    (keys, shiftstates) = unifyShiftstates (view _keys layout)
    shiftlevels = map (WithBar ∘ (:| [])) shiftstates
    posses = map (view _pos) keys
    letters = map (M.fromList ∘ zip posses) ∘ transpose ∘ map (view _letters) $ keys
    getMaxIndex = maybe (length shiftstates) (succ ∘ fst) ∘ getLevel keyForShiftstates
    keyForShiftstates = Key undefined undefined shiftlevels undefined (Just False)

removeTransLayers ∷ TmkKeymap → TmkKeymap
removeTransLayers (TmkKeymap getMaxIndex layers) = TmkKeymap getMaxIndex' layers'
  where
    (layers', filteredIndices) = ($ layers) $
        map (mfilter (not ∘ allTrans) ∘ Just) >>>
        catMaybes &&& findIndices isJust
    getMaxIndex' = (\i → length (takeWhile (< i) filteredIndices)) ∘ getMaxIndex

lettersToTmkLayer ∷ Logger m ⇒ Shiftstate → Map Pos Letter → m TmkLayer
lettersToTmkLayer state = traverse (letterToTmkLetter state) >$> TmkLayer state

letterToTmkLetter ∷ Logger m ⇒ Shiftstate → Letter → m TmkLetter
letterToTmkLetter state letter = letterToTmkLetter' letter state letter

letterToTmkLetter' ∷ Logger m ⇒ Letter → Shiftstate → Letter → m TmkLetter
letterToTmkLetter' _ _ LNothing = pure (TmkAction "NO")
letterToTmkLetter' errorL _ (Ligature _ s) =
    maybe e pure (TmkMacro mId <$> macro s <*> pure [])
  where
    mId = map (\x → bool '_' x (isAscii x ∧ isAlphaNum x)) s
    macro =
        traverse (getPosAndShiftlevel ∘ Char) >=>
        groupWith' snd >>>
        (traverse ∘ _1) (traverse (flip lookup modifierAndKeycode) ∘ toList ∘ NE.head ∘ getNonEmpty) >=>
        (traverse ∘ _2 ∘ traverse) (flip lookup posAndTmkAction ∘ fst) >$>
        concatMap (uncurry modsAndCodesToMacro) >>>
        (["SM()", "CM()"] ⧺) ∘ (⧺ ["RM()"])
    getPosAndShiftlevel letter =
        view _keys defaultFullLayout &
        concatMap (\key → map ((,) (view _pos key)) (getValidShiftlevels letter key)) &
        listToMaybe
    getValidShiftlevels letter key =
        filterOnFst (≡ letter) (view _letters key `zip` view _shiftlevels key)
    modsAndCodesToMacro modifiers keycodes =
        map (\m → "D(" ⊕ m ⊕ ")") modifiers ⧺
        map (\c → "T(" ⊕ c ⊕ ")") keycodes ⧺
        map (\m → "U(" ⊕ m ⊕ ")") modifiers
    e = TmkAction "NO" <$ tell [show' errorL ⊕ " is not supported in TMK"]
letterToTmkLetter' errorL state (Action a)
  | Just action ← lookup a actionAndTmkAction = pure (TmkAction action)
  | Just letter ← lookup a actionAndRedirect = letterToTmkLetter' errorL state letter
letterToTmkLetter' _ _ (Modifiers _ []) = pure (TmkAction "NO")
letterToTmkLetter' _ _ (Modifiers effect [modifier])
  |  effect ≡ defaultModifierEffect modifier = pure (TmkModifier modifier)
letterToTmkLetter' _ _ (Redirect [] pos)
  | Just posAction ← lookup pos posAndTmkAction = pure (TmkAction posAction)
letterToTmkLetter' _ state (Redirect modifiers pos)
  | Just posAction ← lookup pos posAndTmkAction = do
      mods ← intercalate "|" ∘ catMaybes <$> traverse printMod (modifiers ∖ toList state)
      case mods of
        "" → pure (TmkAction posAction)
        _  → pure $ TmkFn ("ACTION_MODS_KEY(" ⊕ mods ⊕ ", KC_" ⊕ posAction ⊕ ")")
  where
    printMod modifier =
      case lookup modifier modifierAndKeycode of
        Just keycode → pure (Just ("MOD_BIT(KC_" ⊕ keycode ⊕ ")"))
        Nothing → Nothing <$ tell [show' modifier ⊕ " is not supported in TMK"]
letterToTmkLetter' _ state letter
  | action:_ ← mapMaybe posToAction posses = pure (TmkAction action)
  where
    posses = getPosByLetterAndShiftstate letter state defaultFullLayout
    posToAction = flip lookup posAndTmkAction
letterToTmkLetter' errorL state _ =
    TmkAction "NO" <$ tell [show' errorL ⊕ " is not supported on " ⊕ show' state ⊕ " in TMK"]

groupLayers ∷ [TmkLayer] → (Int → Int, [(Map Pos TmkLetter, Shiftlevel)])
groupLayers layers = (flip layerStateAfterGrouping &&& layers') groupedLayers
  where
    groupedLayers = NE.groupWith __tmkLetters layers
    layerStateAfterGrouping initState =
        NE.tail ∘ NE.scanl (\(_, start) gr → (start, start + length gr)) (0, 0) >>>
        map (any (testBit initState) ∘ range ∘ over _2 pred) >>>
        sum ∘ zipWith (bool 0 ∘ bit) [0..]
    layers' = map (__tmkLetters ∘ NE.head &&& WithBar ∘ fmap __tmkShiftstate)

addTransLetters ∷ [TmkLayer] → [TmkLayer]
addTransLetters [] = []
addTransLetters (x:xs) = addTransLetters' [] x xs

addTransLetters' ∷ [TmkLayer] → TmkLayer → [TmkLayer] → [TmkLayer]
addTransLetters' low layer high = layer' : nextLayers
  where
    prevLetters = ($ low) $
        filter (`isSubLayer` layer) >>>
        M.unions ∘ map (M.filter (≢ TmkAction "TRNS") ∘ __tmkLetters)
    layer' = over _tmkLetters replaceLetters layer
    replaceLetters = M.unionWith replaceLetter prevLetters
    replaceLetter otherLetter letter
      | otherLetter ≡ letter = TmkAction "TRNS"
      | otherwise = letter
    nextLayers = fromMaybe [] $
        uncurry (addTransLetters' (layer' : low)) <$> uncons high

allTrans ∷ TmkLayer → Bool
allTrans = all (∈ map TmkAction ["TRNS", "NO"]) ∘ __tmkLetters

isSubLayer ∷ TmkLayer → TmkLayer → Bool
isSubLayer = isSubState `on` __tmkShiftstate

isSubState ∷ Shiftstate → Shiftstate → Bool
isSubState = (S.⊆) `on` getSet
