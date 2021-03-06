{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

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
    , _info
    , _singletonKeys
    , _mods
    , _variants
    , _keys
    , SingletonKey(..)
    , _sPos
    , _sLetter
    , Variant(..)
    , isEmptyVariant
    , isEmptyLayout
    , addSingletonKeysAsKeys
    , singletonKeyToKey
    , layoutOrd
    , layoutDelims
    , applyModLayout
    , applyVariantLayout
    , getDefaultKeys
    , addDefaultKeysWith
    , addDefaultKeys
    , removeEmptyLetters
    , unifyShiftstates
    , unifyShiftlevels
    , getLetterByPosAndShiftstate
    , getPosByLetterAndShiftstate
    , getPosByEqAndShiftstate
    ) where

import BasePrelude hiding (toList)
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Data.List.Unicode ((∖))
import Util (parseString, lensWithDefault', expectedKeys, combineOn, nubWithOn, groupWith', mconcatMapM)

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T (Text, unpack)
import Data.Text.Lazy.Builder (Builder)
import Lens.Micro.Platform (Lens', view, set, over, makeLenses, _2)

import FileType (FileType)
import Filter (runFilter)
import JsonPretty (keyOrder', delimsFromList)
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

instance Semigroup Information where
    (Information a1 b1 c1 d1 f1 g1 h1) <>
            (Information a2 b2 c2 d2 f2 g2 h2) =
        Information (a1 ⊕ a2) (b1 ⊕ b2) (c1 ⊕ c2) (d1 ⊕ d2)
                    (f1 ⊕ f2) (g1 ⊕ g2) (h1 ⊕ h2)

instance Monoid Information where
    mempty = Information Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    mappend = (<>)

$(deriveJSON defaultOptions
    { fieldLabelModifier = \s →
        case stripPrefix "info" s of
          Just (x:xs) → toLower x : xs
          _ → dropWhile (≡'_') s
    , omitNothingFields = True
    } ''Information)

data SingletonKey = SingletonKey
    { __sPos ∷ Pos
    , __sLetter ∷ Letter
    } deriving (Eq, Show, Read)
makeLenses ''SingletonKey

newtype Variant = Variant { variantToLayout ∷ Layout } deriving (Show, Read)

data Layout = Layout
    { __info ∷ Information
    , __singletonKeys ∷ [SingletonKey]
    , __mods ∷ [Mod]
    , __variants ∷ [Variant]
    , __keys ∷ [Key]
    } deriving (Show, Read)
makeLenses ''Layout

deriving instance Semigroup Variant
deriving instance Monoid Variant
deriving instance ToJSON Variant
instance FromJSON (FileType → Variant) where
    parseJSON = withObject "variant" $ \o → do
        expectedKeys ("filter" : "name" : layoutKeys ∖ ["mods", "variants"]) o
        _ ← o .: "name" ∷ Parser String -- Ensure a name
        fmap Variant <$> parseJSON (Object o)

layoutKeys ∷ [T.Text]
layoutKeys = ["singletonKeys","qwertyShortcuts","mods","variants","keysWithShiftlevels","shiftlevels","keysWithShiftstates","shiftstates","keys","customDeadKeys"]

isEmptyLayout ∷ Layout → Bool
isEmptyLayout (Layout info [] [] [] []) = isEmptyInformation info
isEmptyLayout _ = False

isEmptyVariant ∷ Variant → Bool
isEmptyVariant (Variant layout) = isEmptyLayout layout

instance Semigroup Layout where
    (Layout a1 b1 c1 d1 keys1) <>
            (Layout a2 b2 c2 d2 keys2) =
        Layout (a1 ⊕ a2) (b1 `combineSingletonKeys` b2)
               (c1 ⊕ c2) (d1 `combineVariants` d2) (keys1 `combineKeys` keys2)

instance Monoid Layout where
    mempty = Layout (∅) (∅) (∅) (∅) []
    mappend = (<>)

nubSingletonKeys ∷ [SingletonKey] → [SingletonKey]
nubSingletonKeys = nubWithOn (\k ks → NE.last (k :| ks)) (view _sPos)

combineSingletonKeys ∷ [SingletonKey] → [SingletonKey] → [SingletonKey]
combineSingletonKeys = combineOn (\k ks → NE.last(k :| ks)) (view _sPos)

combineVariants ∷ [Variant] → [Variant] → [Variant]
combineVariants = combineOn (\v vs → setName v (mconcat (v:vs))) (view (_info ∘ _name) ∘ variantToLayout)
  where
    setName (Variant l) =
        variantToLayout >>>
        set (_info ∘ _name) (view (_info ∘ _name) l) >>>
        Variant

addSingletonKeysAsKeys ∷ Layout → Layout
addSingletonKeysAsKeys layout = set _singletonKeys [] $
    over _keys (⧺ map singletonKeyToKey (view _singletonKeys layout)) layout

singletonKeyToKey ∷ SingletonKey → Key
singletonKeyToKey (SingletonKey pos letter) = Key pos Nothing [M.empty] [letter] Nothing

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
shiftstatesToShiftlevels keys = maybe [] (shiftstatesToShiftlevels' keys ∘ (:[]) ∘ WithBar) ∘ NE.nonEmpty

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

instance ToJSON SingletonKey where
    toJSON (SingletonKey pos letter) = toJSON (pos, letter)

instance FromJSON (FileType → Maybe SingletonKey) where
    parseJSON (Object o) = do
        filt ← o .:? "filter" .!= (∅)
        sKey ← liftA2 SingletonKey (o .: "pos") (o .: "letter")
        pure (bool Nothing (Just sKey) ∘ runFilter filt)
    parseJSON v = pure ∘ Just ∘ uncurry SingletonKey <$> parseJSON v

instance ToJSON Layout where
    toJSON (Layout info singletonKeys mods variants keys) =
        let hmInfo = fromValue (∅) $ toJSON info
            hmKeys = fromValue (∅) $ toJSON' keys
            customDeadKeys = concatMap (mapMaybe letterToCustomDeadKey ∘ view _letters) keys
            hmSingletonKeys  = HM.fromList [ "singletonKeys"  .= singletonKeys  | not (null singletonKeys) ]
            hmMods           = HM.fromList [ "mods"           .= mods           | not (null mods) ]
            hmVariants       = HM.fromList [ "variants"       .= variants       | not (null variants) ]
            hmCustomDeadKeys = HM.fromList [ "customDeadKeys" .= customDeadKeys | not (null customDeadKeys) ]
        in
        Object (HM.unions [hmInfo, hmKeys, hmSingletonKeys, hmMods, hmVariants, hmCustomDeadKeys])
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
        filt ← o .:? "filter" .!= (∅)
        info ← parseJSON (Object o)
        allSingletonKeysFilt ← sequence <$> o .:? "singletonKeys" .!= []
        let singletonKeysFilt = nubSingletonKeys ∘ catMaybes <$> allSingletonKeysFilt
        mods ← o .:? "mods" .!= []
        variants ← sequence <$> o .:? "variants" .!= []
        customDeadKeys  ← o .:? "customDeadKeys"  .!= []
        qwertyShortcuts ← o .:? "qwertyShortcuts" .!= False
        keysFilt ← (fmap ∘ fmap)
            (nubKeys >>>
             either error id ∘ setCustomDeads customDeadKeys >>>
             bool id (map (liftA2 (set _shortcutPos) (view _pos) id)) qwertyShortcuts
            ) (parseJSON' o)
        let layoutFilt = Layout info <$> singletonKeysFilt <*> pure mods <*> variants <*> keysFilt
        pure (bool (∅) ∘ layoutFilt <*> runFilter filt)

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
    , ("customDeadKeys", ["\n", "\n", "\n", " ", " ", " "])
    , ("mods", ["\n", "\n", "\n", " "])
    ]

applyModLayout ∷ Mod → Layout → Layout
applyModLayout layoutMod | isEmptyMod layoutMod = id
applyModLayout layoutMod@(Mod nameM _) =
    over (_info ∘ _fullName) (⊕ (" (" ⊕ nameM ⊕ ")")) ∘
    over (_info ∘ _name) (⊕ ('_':nameM)) ∘
    over (_singletonKeys ∘ traverse ∘ _sPos) (applyMod layoutMod) ∘
    over (_keys ∘ traverse ∘ _pos) (applyMod layoutMod) ∘
    over (_keys ∘ traverse ∘ _shortcutPos) id -- make the (perhaps) guessed shortcut position explicit

applyVariantLayout ∷ Variant → Layout → Layout
applyVariantLayout variant | isEmptyVariant variant = id
applyVariantLayout (Variant layout) = (⊕ layoutV)
  where
    layoutV = ($ layout) $
        set (_info ∘ _fullName) (" (" ⊕ name ⊕ ")") >>>
        set (_info ∘ _name) ('_':name)
    name = view (_info ∘ _name) layout

getLetterByPosAndShiftstate ∷ Pos → Shiftstate → Layout → Maybe Letter
getLetterByPosAndShiftstate pos state = listToMaybe ∘ liftA2 (⧺)
    (map toLetter ∘ filter ((≡ pos) ∘ view _pos) ∘ view _keys)
    (map (view _sLetter) ∘ filter ((≡ pos) ∘ view _sPos) ∘ view _singletonKeys)
  where
    toLetter = flip getLetter state

getPosByLetterAndShiftstate ∷ Letter → Shiftstate → Layout → [Pos]
getPosByLetterAndShiftstate letter = getPosByEqAndShiftstate (≡ letter)

getPosByEqAndShiftstate ∷ (Letter → Bool) → Shiftstate → Layout → [Pos]
getPosByEqAndShiftstate eq state = liftA2 (⧺)
    (map (view _pos) ∘ filter (eq ∘ toLetter) ∘ view _keys)
    (map (view _sPos) ∘ filter (eq ∘ view _sLetter) ∘ view _singletonKeys)
  where
    toLetter = flip getLetter state
