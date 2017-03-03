{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Layout.Layout
    ( Information(..)
    , _fullName
    , _name
    , _copyright
    , _company
    , _localeId
    , _version
    , _description
    , Layout(..)
    , SingletonKey
    , _info
    , _singletonKeys
    , _mods
    , _keys
    , isEmptyLayout
    , addSingletonKeysAsKeys
    , singletonKeyToKey
    , layoutOrd
    , layoutDelims
    , applyModLayout
    , getDefaultKeys
    , addDefaultKeysWith
    , addDefaultKeys
    , removeEmptyLetters
    , unifyShiftstates
    , unifyShiftlevels
    , getLetterByPosAndShiftstate
    , getPosByLetterAndShiftstate
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (parseString, lensWithDefault', expectedKeys, combineOn, nubWithOn, groupWith', mconcatMapM)

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Aeson
import Data.Aeson.TH (deriveJSON, fieldLabelModifier, omitNothingFields)
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T (Text, unpack)
import Data.Text.Lazy.Builder (Builder)
import Lens.Micro.Platform (Lens', view, set, over, makeLenses, _2)

import FileType (FileType)
import Filter (Filter(..))
import JsonPretty (keyOrder', delimsFromList)
import Layout.DeadKey (DeadKey)
import Layout.Key
import Layout.Mod (Mod(..), isEmptyMod, applyMod)
import Layout.Modifier (Shiftstate, Shiftlevel, parseJSONShiftlevels)
import qualified Layout.Modifier as M
import Layout.Pos (Pos)
import WithBar (WithBar(..), getNonEmpty)

data Information = Information
    { infoFullName, infoName, __copyright, __company
    , __localeId, __version, __description ∷ Maybe String
    } deriving (Show, Read)
makeLenses ''Information
_fullName ∷ Lens' Information String
_fullName = lensWithDefault' "Custom Keyboard Layout" (\y x → x {infoFullName = y}) infoFullName
_name ∷ Lens' Information String
_name = lensWithDefault' "custom" (\y x → x {infoName = y}) infoName

infoKeys ∷ [T.Text]
infoKeys = ["fullName","name","copyright","company","localeId","version","description"]

isEmptyInformation ∷ Information → Bool
isEmptyInformation (Information Nothing Nothing Nothing Nothing Nothing Nothing Nothing) = True
isEmptyInformation _ = False

instance Monoid Information where
    mempty = Information Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    mappend (Information a1 b1 c1 d1 f1 g1 h1)
            (Information a2 b2 c2 d2 f2 g2 h2) =
        Information (a1 ⊕ a2) (b1 ⊕ b2) (c1 ⊕ c2) (d1 ⊕ d2)
                    (f1 ⊕ f2) (g1 ⊕ g2) (h1 ⊕ h2)

$(deriveJSON defaultOptions
    { fieldLabelModifier = \s →
        case stripPrefix "info" s of
          Just (x:xs) → toLower x : xs
          _ → dropWhile (≡'_') s
    , omitNothingFields = True
    } ''Information)

type SingletonKey = (Pos, Letter)
data Layout = Layout
    { __info ∷ Information
    , __singletonKeys ∷ [SingletonKey]
    , __mods ∷ [Mod]
    , __keys ∷ [Key]
    } deriving (Show, Read)
makeLenses ''Layout

layoutKeys ∷ [T.Text]
layoutKeys = ["singletonKeys","qwertyShortcuts","mods","keysWithShiftlevels","shiftlevels","keysWithShiftstates","shiftstates","keys","customDeadKeys"]

isEmptyLayout ∷ Layout → Bool
isEmptyLayout (Layout info [] [] []) = isEmptyInformation info
isEmptyLayout _ = False

instance Monoid Layout where
    mempty = Layout (∅) (∅) (∅) []
    mappend (Layout a1 b1 c1 keys1)
            (Layout a2 b2 c2 keys2) =
        Layout (a1 ⊕ a2) (b1 `combineSingletonKeys` b2)
               (c1 ⊕ c2) (keys1 `combineKeys` keys2)

nubSingletonKeys ∷ [SingletonKey] → [SingletonKey]
nubSingletonKeys = nubWithOn (\(pos,l) ks → (pos, NE.last (l :| map snd ks))) fst

combineSingletonKeys ∷ [SingletonKey] → [SingletonKey] → [SingletonKey]
combineSingletonKeys = combineOn (\(pos,l) ks → (pos, NE.last (l :| map snd ks))) fst

addSingletonKeysAsKeys ∷ Layout → Layout
addSingletonKeysAsKeys layout = set _singletonKeys [] $
    over _keys (⧺ map singletonKeyToKey (view _singletonKeys layout)) layout

singletonKeyToKey ∷ SingletonKey → Key
singletonKeyToKey (pos, letter) = Key pos Nothing [M.empty] [letter] Nothing

unifyShiftstates ∷ [Key] → ([Key], [Shiftstate])
unifyShiftstates keys = (map (setLevels levels) keys, shiftstates)
  where
    levels = map (WithBar ∘ (:| [])) shiftstates
    shiftstates = nub ∘ concatMap (concatMap toList ∘ view _shiftlevels) $ keys

unifyShiftlevels ∷ [Key] → ([Key], [Shiftlevel])
unifyShiftlevels keys = (map (setLevels levels) keys, levels)
  where
    levels = ($ keys) $
        shiftstatesToShiftlevels <*>
        nub ∘ concatMap (concatMap toList ∘ view _shiftlevels)

shiftstatesToShiftlevels ∷ [Key] → [Shiftstate] → [Shiftlevel]
shiftstatesToShiftlevels keys = maybe [] (shiftstatesToShiftlevels' keys ∘ (:[]) ∘ WithBar) ∘ nonEmpty

shiftstatesToShiftlevels' ∷ [Key] → [Shiftlevel] → [Shiftlevel]
shiftstatesToShiftlevels' [] = id
shiftstatesToShiftlevels' (key:keys) =
    shiftstatesToShiftlevels' keys >>>
    concatMap (map (WithBar ∘ fmap snd) ∘ NE.groupWith fst ∘ fmap (getLetter key &&& id) ∘ getNonEmpty)

setLevels ∷ [Shiftlevel] → Key → Key
setLevels levels key
  | levels ≡ view _shiftlevels key = key
  | otherwise = set _shiftlevels levels ∘ set _letters letters $ key
  where
    letters = map (getLetter key ∘ NE.head ∘ getNonEmpty) levels

instance ToJSON Layout where
    toJSON (Layout info singletonKeys mods keys) =
        let hmInfo = fromValue (∅) $ toJSON info
            hmKeys = fromValue (∅) $ toJSON' keys
            customDeadKeys = concatMap (mapMaybe letterToCustomDeadKey ∘ view _letters) keys
            hmSingletonKeys  = HM.fromList [ "singletonKeys"  .= singletonKeys  | not (null singletonKeys) ]
            hmMods           = HM.fromList [ "mods"           .= mods           | not (null mods) ]
            hmCustomDeadKeys = HM.fromList [ "customDeadKeys" .= customDeadKeys | not (null customDeadKeys) ]
        in
        Object (HM.unions [hmInfo, hmKeys, hmSingletonKeys, hmMods, hmCustomDeadKeys])
      where
        fromValue _ (Object hm) = hm
        fromValue x _           = x

toJSON' ∷ [Key] → Value
toJSON' =
    groupWith' (view _shiftlevels) >>>
    set (traverse ∘ _2 ∘ traverse ∘ _shiftlevels) [] >>>
    whileChange (combineKeysOnShiftlevels >=> removeSingleShiftlevels) >>>
    \case
      []   → object []
      [ks] → keysToJSON ks
      kss  → object ["keysWithShiftlevels" .= map keysToJSON kss]
  where
    keysToJSON (shiftlevels, ks) = object ["shiftlevels" .= shiftlevels, "keys" .= toJSON ks]

    whileChange ∷ (α → Writer Any α) → α → α
    whileChange g x = bool x (whileChange g gx) change
      where (gx, Any change) = runWriter (g x)

    shiftlevelsUnion ∷ [Shiftlevel] → [Shiftlevel] → Maybe [Shiftlevel]
    shiftlevelsUnion state1 state2
      | state1 `isPrefixOf` state2 = Just state2
      | state2 `isPrefixOf` state1 = Just state1
      | otherwise = Nothing

    combineKeysOnShiftlevels ∷ [([Shiftlevel], [Key])] → Writer Any [([Shiftlevel], [Key])]
    combineKeysOnShiftlevels [] = pure []
    combineKeysOnShiftlevels [x] = pure [x]
    combineKeysOnShiftlevels (x1@(state1,keys1) : x2@(state2,keys2) : xs) =
      case shiftlevelsUnion state1 state2 of
        Just state →
            tell (Any True) *>
            combineKeysOnShiftlevels ((state, keys1 ⧺ keys2) : xs)
        Nothing → (x1 :) <$> combineKeysOnShiftlevels (x2 : xs)

    removeSingleShiftlevels ∷ [([Shiftlevel], [Key])] → Writer Any [([Shiftlevel], [Key])]
    removeSingleShiftlevels [] = pure []
    removeSingleShiftlevels ((state1,keys1) : (state2,keys2) : (state3,keys3) : xs)
      | Just state13 ← shiftlevelsUnion state1 state3
      , null (drop 1 keys2)
      = tell (Any True) *>
        removeSingleShiftlevels ((state13, keys1 ⧺ keys2' ⧺ keys3) : xs)
      where keys2' = set (traverse ∘ _shiftlevels) state2 keys2
    removeSingleShiftlevels (x:xs) = (x :) <$> removeSingleShiftlevels xs

instance FromJSON (FileType → Layout) where
    parseJSON = withObject "layout" $ \o → do
        expectedKeys ("filter" : infoKeys ⧺ layoutKeys) o
        filt ← o .:? "filter" .!= Filter (const True)
        info ← parseJSON (Object o)
        customDeadKeys  ← o .:? "customDeadKeys"  .!= []
        qwertyShortcuts ← o .:? "qwertyShortcuts" .!= False
        singletonKeys ← nubSingletonKeys <$> o .:? "singletonKeys" .!= []
        keys ← (fmap ∘ fmap)
                   (nubKeys >>>
                    either error id ∘ setCustomDeads customDeadKeys >>>
                    bool id (map (liftA2 (set _shortcutPos) (view _pos) id)) qwertyShortcuts
                   ) (parseJSON' o)
        layout ← Layout info singletonKeys <$> o .:? "mods" .!= []
        pure (bool (∅) ∘ layout ∘ keys <*> runFilter filt)

parseJSON' ∷ Object → Parser (FileType → [Key])
parseJSON' = lift3A2 (⧺) parseKeys parseKeysWithShiftlevels
  where
    lift3A2 = liftA2 ∘ liftA2 ∘ liftA2
    parseKeysWithShiftlevels =
        (\o → HM.lookup "keysWithShiftlevels" o <|> HM.lookup "keysWithShiftstates" o) >>>
        maybe (pure []) (withArray "keys" (pure ∘ toList)) >=>
        mconcatMapM (withObject "keys" parseJSON')
    parseKeys o = do
        keys'' ← o .:? "keys" ∷ Parser (Maybe [FileType → Maybe Key])
        case keys'' of
          Nothing → pure (∅)
          Just keys' → do
            let e = fail "object with property keys, but without corresponding property shiftlevels"
            levels ← sequence (parseJSONShiftlevels o) >>= maybe e pure
            let keys t = mapMaybe ($ t) keys'
            pure $ map (setDefaultShiftstates levels) <$> keys

getDefaultKeys ∷ [Key] → Layout → [Key]
getDefaultKeys = flip $ \layout → filter ((∈ posses layout) ∘ view _pos)
  where
    posses = map (view _pos) ∘ view _keys

addDefaultKeysWith ∷ ([Key] → Layout → [Key]) → [Key] → Layout → Layout
addDefaultKeysWith f defaultKeys layout =
    over _keys (`combineKeysWithoutOverwrite` f defaultKeys layout) layout

addDefaultKeys ∷ [Key] → Layout → Layout
addDefaultKeys = addDefaultKeysWith getDefaultKeys

removeEmptyLetters ∷ Key → Key
removeEmptyLetters key =
    let (ls, ms) = unzip $ dropWhileEnd ((≡) LNothing ∘ fst) (view _letters key `zip` view _shiftlevels key)
    in  set _letters ls ∘ set _shiftlevels ms $ key

setCustomDeads ∷ [DeadKey] → [Key] → Either String [Key]
setCustomDeads = traverse ∘ _letters ∘ traverse ∘ setCustomDeadKey

layoutOrd ∷ T.Text → T.Text → Ordering
layoutOrd = keyOrder'
    ["fullName","name","copyright","company","localName","localeId","version","description"
    ,"qwertyShortcuts","singletonKeys"
    ,"pos","shortcutPos","shiftlevels","shiftstates","letters","capslock"
    ,"keys","keysWithShiftlevels","keysWithShiftstates","customDeadKeys","mods"
    ,"baseChar","stringMap"
    ]
    (parseString ∘ T.unpack ∷ T.Text → Maybe Pos)

layoutDelims ∷ T.Text → [Builder]
layoutDelims = delimsFromList
    [ ("singletonKeys", ["\n", " "])
    , ("shiftlevels", [" ", " "])
    , ("shiftstates", [" ", " "])
    , ("keys", ["\n", " ", " "])
    , ("customDeadKeys", ["\n", "\n", "\n", " "])
    , ("mods", ["\n", "\n", "\n", " "])
    ]

applyModLayout ∷ Mod → Layout → Layout
applyModLayout layoutMod | isEmptyMod layoutMod = id
applyModLayout layoutMod@(Mod nameM _) =
    over (_info ∘ _fullName) (⊕ (" (" ⊕ nameM ⊕ ")")) ∘
    over (_info ∘ _name) (⊕ ('_':nameM)) ∘
    over (_keys ∘ traverse ∘ _pos) (applyMod layoutMod) ∘
    over (_keys ∘ traverse ∘ _shortcutPos) id -- make the (perhaps) guessed shortcut position explicit

getLetterByPosAndShiftstate ∷ Pos → Shiftstate → Layout → Maybe Letter
getLetterByPosAndShiftstate pos state = listToMaybe ∘ liftA2 (⧺)
    (map toLetter ∘ filter ((≡ pos) ∘ view _pos) ∘ view _keys)
    (map snd ∘ filter ((≡ pos) ∘ fst) ∘ view _singletonKeys)
  where
    toLetter = flip getLetter state

getPosByLetterAndShiftstate ∷ Letter → Shiftstate → Layout → [Pos]
getPosByLetterAndShiftstate letter state = liftA2 (⧺)
    (map (view _pos) ∘ filter ((≡ letter) ∘ toLetter) ∘ view _keys)
    (map fst ∘ filter ((≡ letter) ∘ snd) ∘ view _singletonKeys)
  where
    toLetter = flip getLetter state
